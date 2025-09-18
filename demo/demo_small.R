library("multieam")

n_items <- 10

# placeholder
reach_index <- seq(1, n_items)
item_index <- seq(1, n_items)

# A, V, ndt are fixed in each condition
prior_formulas <- list(
  A ~ distributional::dist_uniform(1, 10),
  V ~ distributional::dist_uniform(0.2, 0.4),
  ndt ~ 1
)

# no trial and position dependent variability
between_trial_formulas <- prior_formulas
between_trial_formulas <- list(A ~ distributional::dist_uniform(1, 10),
                        V ~ distributional::dist_uniform(0.2, 0.4),
                        ndt ~ 1)  # Use the prior formulas as item formulas
item_formulas <- list()


evaluate_with_dt(prior_formulas, n = 10)

# sim_result <- run_simulation(
#   prior_formulas = prior_formulas,
#   between_trial_formulas = between_trial_formulas,
#   item_formulas = item_formulas,
#   n_condition = 5,
#   n_trial_per_condition = 200,
#   n_item = n_items,
#   max_t = 10
# )
# sim_result


# res <- run_trial(
#   trial_setting = list(),
#   item_formulas = item_formulas,
#   n_items = n_items,
#   max_reached = n_items,
#   max_t = 10,
#   dt = 0.01,
#   noise_mechanism = "add",
#   noise_factory = function(trial_setting) {
#     function(n, dt) {
#       rnorm(n, 0, 0)
#     }
#   },
#   trajectories = TRUE
# )

res <- run_condition(
  condition_setting = list(),
  between_trial_formulas = between_trial_formulas,
  item_formulas = item_formulas,
  n_trials = 5,
  n_items = n_items,
  max_reached = n_items,
  max_t = 10,
  dt = 0.01,
  noise_mechanism = "add",
  noise_factory = function(condition_setting) {
    function(n, dt) {
      rnorm(n, 0, 0)
    }
  },
  trajectories = TRUE
)
