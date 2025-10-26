###############
# model setup #
###############
n_items <- 10
prior_formulas <- list(
  n_items ~ 10,
  # parameters with distributions
  A_beta_0 ~ distributional::dist_uniform(1.00, 10.00),
  A_beta_1 ~ distributional::dist_uniform(1.00, 10.00),
  # V
  V_beta_0 ~ distributional::dist_uniform(1.0, 5.0),
  V_beta_1 ~ distributional::dist_uniform(-0.50, -0.01),
  # ndt
  ndt ~ distributional::dist_uniform(0.01, 3.00),
  # noise param
  noise_coef ~ 1,
  # between trial varibaility
  sigma ~ distributional::dist_uniform(0.1, 3.00)
)

between_trial_formulas <- list(
  V_var ~ distributional::dist_normal(0, sigma)
)

item_formulas <- list(
  A ~ A_beta_0 + seq(1, n_items) * A_beta_1,
  V ~ V_beta_0 + seq(1, n_items) * V_beta_1 + V_var
)

noise_factory <- function(context) {
  noise_coef <- context$noise_coef

  function(n, dt) {
    noise_coef * rnorm(n, mean = 0, sd = sqrt(dt))
  }
}

####################
# simulation setup #
####################
sim_config <- new_simulation_config(
  prior_formulas = prior_formulas,
  between_trial_formulas = between_trial_formulas,
  item_formulas = item_formulas,
  n_conditions_per_chunk = NULL, # automatic chunking
  n_conditions = 500,
  n_trials_per_condition = 100,
  n_items = n_items,
  max_reached = n_items,
  max_t = 100,
  dt = 0.01,
  noise_mechanism = "add",
  noise_factory = noise_factory,
  model = "ddm",
  parallel = TRUE,
  n_cores = NULL, # Will use default: detectCores() - 1
  rand_seed = NULL # Will use default random seed
)
print(sim_config)

# output temporary path setup
temp_output_path <- tempfile("multieam_demo_output")
# remove if exists
if (dir.exists(temp_output_path)) {
  unlink(temp_output_path, recursive = TRUE)
}
cat("Temporary output path:\n")
cat(temp_output_path, "\n")

##################
# run simulation #
##################
sim_output <- run_simulation(
  config = sim_config,
  output_dir = temp_output_path
)

#####################
# abc model prepare #
#####################
# preview the output data variables
head(sim_output$open_dataset())
# uncommented to collect all data into memory (avoid for large sims)
# sim_output_df <- sim_output$open_dataset() |> dplyr::collect()

# define the summary procedure
summary_pipe <-
  summarise_by(
    .by = c("condition_idx"),
    # single value
    rt_mean = mean(rt),
    # named vector
    rt_quantiles = quantile(rt, probs = c(0.1, 0.5, 0.9)),
    # regression coefficients
    lm_coef = lm(rt ~ item_idx)$coefficients,
    # complex functions
    aic = {
      model <- glm(rt ~ item_idx, family = gaussian())
      summary(model)$aic
    }
  ) +
  summarise_by(
    .by = c("condition_idx", "item_idx"),
    rt_mean = mean(rt),
    rt_quantiles = quantile(rt, probs = c(0.1, 0.5, 0.9))
  )

# summarise simulation output
simulation_sumstat <- map_by_condition(
  sim_output,
  .progress = TRUE,
  .parallel = TRUE,
  function(cond_df) {
    # clean data here
    complete_df <- cond_df |>
      dplyr::filter(V_beta_0 > 0, !is.na(rt))
    # extract the summary by calling spec directly
    summary_pipe(complete_df)
  }
)

# clean non-complete simulations
simulation_sumstat <- simulation_sumstat[complete.cases(simulation_sumstat),]

# summarized observed/target data
# pretend observed data is condition 1
# you should load your own observed data here and rename columns accordingly
observed_data <- sim_output$open_dataset() |>
  dplyr::filter(chunk_idx == 1, condition_idx == 1) |>
  dplyr::collect()
target_sumstat <- summary_pipe(observed_data)

# Prepare data for ABC fitting
abc_input <- build_abc_input(
  simulation_output = sim_output,
  simulation_summary = simulation_sumstat,
  target_summary = target_sumstat,
  param = c("A_beta_0", "A_beta_1", "V_beta_0", "V_beta_1", "ndt", "sigma")
)

#####################
# ABC model fitting #
#####################
abc_rejection_model <- abc::abc(
  target = abc_input$target,
  param = abc_input$param,
  sumstat = abc_input$sumstat,
  tol = 0.5,
  method = "rejection"
)

abc_loclinear_model <- abc::abc(
  target = abc_input$target,
  param = abc_input$param,
  sumstat = abc_input$sumstat,
  tol = 0.5,
  method = "loclinear",
  transf  = c("log", "log", "log", "none", "log", "log")
)

abc_neuralnet_model <- abc::abc(
  target = abc_input$target,
  param = abc_input$param,
  sumstat = abc_input$sumstat,
  tol     = 0.05,
  method  = "neuralnet",
  sizenet = 8,
  maxit   = 10000,
  lambda  = 1e-2,
  kernel  = "epanechnikov",
  transf  = c("log", "log", "log", "none", "log", "log")
)
####################
# cross validation #
####################
abc_neuralnet_cv <- abc::cv4abc(
  param = abc_input$param,
  sumstat = abc_input$sumstat,
  abc.out = abc_neuralnet_model,
  nval = 100,
  tols = c(0.05, 0.1)
)

