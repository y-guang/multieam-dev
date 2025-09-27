n_items <- 10

between_trial_formulas <- list(
  n_items ~ 10,
  A ~ 10,
  V_0 ~ distributional::dist_uniform(0.01, 0.02),
  ndt ~ 1
)

item_formulas <- list(
  V ~ V_0 + 0.1 * seq(n_items),
  ndt ~ ndt
)

noise_factory <- function(condition_setting) {
  function(n, dt) {
    rep(0, n)
  }
}

res <- run_condition(
  condition_setting = list(),
  between_trial_formulas = between_trial_formulas,
  item_formulas = item_formulas,
  n_trials = 100,
  n_items = n_items,
  max_reached = n_items,
  max_t = 100,
  dt = 0.01,
  noise_mechanism = "add",
  noise_factory = noise_factory,
  model = "ddm",
  trajectories = FALSE
)

# test speed
# results <- replicate(100, run_condition(
#   condition_setting = list(),
#   between_trial_formulas = between_trial_formulas,
#   item_formulas = item_formulas,
#   n_trials = 100,
#   n_items = n_items,
#   max_reached = n_items,
#   max_t = 100,
#   dt = 0.01,
#   noise_mechanism = "add",
#   noise_factory = noise_factory,
#   model = "ddm",
#   trajectories = FALSE
# ), simplify = FALSE)
