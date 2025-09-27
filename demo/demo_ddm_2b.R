# placeholder
n_items <- 2

prior_formulas <- list()

between_trial_formulas <- list()

item_formulas <- list(
  A_upper ~ c(5, 10),
  A_lower ~ c(-5, -10),
  V ~ c(2, -1),
  ndt ~ 0
)

noise_factory <- function(context) {
  function(n, dt) {
    rnorm(n, mean = 0, sd = 1e-5)
  }
}

sim_result <- run_simulation(
  prior_formulas = prior_formulas,
  between_trial_formulas = between_trial_formulas,
  item_formulas = item_formulas,
  n_condition = 2,
  n_trial_per_condition = 2,
  n_items = n_items,
  max_reached = n_items,
  max_t = 100,
  dt = 0.01,
  noise_mechanism = "add",
  noise_factory = noise_factory,
  trajectories = FALSE,
  model = "ddm-2b",
  parallel = FALSE
)

flat_result <- flatten_simulation_results(sim_result)
