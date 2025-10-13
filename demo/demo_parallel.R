# Demo: Parallel simulation using the new configuration interface

# placeholder
n_items <- 10

prior_formulas <- list(
  n_items ~ 10,
  # parameters with distributions
  # A
  A_alpha ~ distributional::dist_uniform(1, 10),
  A_kappa ~ distributional::dist_uniform(0.1, 0.2),
  A_gamma ~ distributional::dist_uniform(0.01, 0.2),
  # V
  V_beta_1 ~ distributional::dist_uniform(-0.4, -0.01),
  V_beta_0 ~ distributional::dist_uniform(0.5, 0.8),
  # ndt
  ndt ~ distributional::dist_uniform(0.1, 3),
  # noise param
  noise_coef ~ 100
)

between_trial_formulas <- list(
  V_var ~ distributional::dist_normal(0, 0.1)
)

item_formulas <- list(
  A ~ A_alpha * exp(A_kappa * seq(1, n_items)) + A_gamma,
  V ~ V_beta_0 + seq(1, n_items) * V_beta_1 + V_var
)

noise_factory <- function(context) {
  noise_coef <- context$noise_coef

  function(n, dt) {
    rnorm(n, mean = 0, sd = sqrt(dt) * noise_coef)
  }
}

# Create simulation configuration
sim_config <- new_simulation_config(
  prior_formulas = prior_formulas,
  between_trial_formulas = between_trial_formulas,
  item_formulas = item_formulas,
  n_conditions_per_chunk = 100,
  n_conditions = 1000,
  n_trials_per_condition = 100,
  n_items = n_items,
  max_reached = n_items,
  max_t = 100,
  dt = 0.01,
  noise_mechanism = "add",
  noise_factory = noise_factory,
  model = "ddm",
  parallel = TRUE,
  n_cores = NULL,  # Will use default: detectCores() - 1
  rand_seed = NULL  # Will use default random seed
)
print(sim_config)

# create temporary output path
temp_output_path = tempfile("multieam_demo_output")
# remove if exists
if (dir.exists(temp_output_path)) {
  unlink(temp_output_path, recursive = TRUE)
}
cat("Temporary output path:\n")
cat(temp_output_path, "\n")

# Run simulation using the configuration
sim_output <- run_simulation(
  config = sim_config,
  output_dir = temp_output_path # No output directory, results kept in temp file
)

# Access results through the dataset
print(sim_output$simulation_config)

# load data into memory
df <- as.data.frame(sim_output$dataset)

# re-load a dataset from the output path
sim_output_reloaded <- load_simulation_output(temp_output_path)

