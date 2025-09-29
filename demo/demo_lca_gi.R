# Demo for LCA-GI (Leaky Competing Accumulator with Global Inhibition) model
n_items <- 3

prior_formulas <- list()

between_trial_formulas <- list()

item_formulas <- list(
  A ~ c(8, 10, 12),        # Thresholds for each reached_idx
  V ~ c(1.5, 1.0, 0.8),    # Drift rates for each item
  ndt ~ c(0.2, 0.3, 0.25), # Non-decision times for each item
  beta ~ c(0.1, 0.15, 0.12), # Inhibition strengths for each item
  k ~ c(0.05, 0.08, 0.06)  # Leakage rates for each item
)

noise_factory <- function(context) {
  function(n, dt) {
    rnorm(n, mean = 0, sd = sqrt(dt) * 0.5)  # Standard Gaussian noise
  }
}

sim_result <- run_simulation(
  prior_formulas = prior_formulas,
  between_trial_formulas = between_trial_formulas,
  item_formulas = item_formulas,
  n_condition = 2,
  n_trial_per_condition = 5,
  n_items = n_items,
  max_reached = n_items,
  max_t = 50,
  dt = 0.01,
  noise_factory = noise_factory,
  trajectories = FALSE,
  model = "lca-gi",
  parallel = FALSE
)

flat_result <- flatten_simulation_results(sim_result)
