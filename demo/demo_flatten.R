# placeholder
n_items <- 4

prior_formulas <- list(
  n_items ~ 4,
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

sim_result <- run_simulation(
  prior_formulas = prior_formulas,
  between_trial_formulas = between_trial_formulas,
  item_formulas = item_formulas,
  n_condition = 100,
  n_trial_per_condition = 100,
  n_items = n_items,
  max_t = 100,
  dt = 0.01,
  noise_mechanism = "add",
  noise_factory = noise_factory,
  trajectories = FALSE
)

tidy_data <- flatten_simulation_results(sim_result)