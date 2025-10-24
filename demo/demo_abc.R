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
  V_beta_1 ~ distributional::dist_uniform(-0.50, -0.01),
  V_beta_0 ~ distributional::dist_uniform(1.0, 5.0),
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
  n_conditions = 200,
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
# output processing #
#####################
# preview the output data
head(sim_output$open_dataset())

summarise_by <- function(.data, .by = NULL, ...) {
  dots <- rlang::enquos(...)

  # group_by
  grouped <- if (is.null(.by)) list(.data)
  else dplyr::group_split(.data, dplyr::across(dplyr::all_of(.by)))


  # evaluate
  purrr::map_dfr(grouped, function(sub_df) {
    key_vals <- if (is.null(.by)) list() else
      as.list(dplyr::slice_head(dplyr::ungroup(sub_df), n = 1)[, .by, drop = FALSE])

    vals <- purrr::imap(dots, function(expr, name) {
      if (rlang::is_symbol(rlang::ensym(name))) {
        colname <- rlang::quo_name(expr)
      } else {
        colname <- name
      }

      val <- rlang::eval_tidy(expr, data = sub_df)

      if (is.list(val) || length(val) > 1) {
        nm <- names(val)
        if (is.null(nm) || any(nm == "")) {
          nm <- paste0(colname, "_", seq_along(val))
        } else {
          nm <- paste0(colname, "_", nm)
        }
        stats::setNames(as.list(val), nm)
      } else {
        stats::setNames(list(val), colname)
      }
    }) |> purrr::flatten()

    tibble::as_tibble(c(key_vals, vals))
  })
}

# summarise
condition_summary <- map_by_condition(
  sim_output,
  function(cond_df) {
    # cond_df |>
    #   dplyr::group_by(condition_idx, item_idx) |>
    #   dplyr::summarise(
    #   rt_mean = mean(rt),
    #   !!!as.list(quantile(rt, probs = 0.1)),
    #   .groups = "drop"
    # )

    summarise_by(
      cond_df,
      .by = c("condition_idx", "item_idx"),
      rt_mean = mean(rt),
      quantiles = quantile(rt, probs = c(0.1, 0.5, 0.9))
    )
  }
)

