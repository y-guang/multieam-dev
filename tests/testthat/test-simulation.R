# test evaluate_with_dt
test_that("evaluate_with_dt works with numeric", {
  test_formula <- list(
    a ~ 5
  )
  result <- multieam:::evaluate_with_dt(test_formula, data = list(), n = 1)
  expect_equal(result$a, 5)
})

test_that("evaluate_with_dt works with numeric vec", {
  test_formula <- list(
    a ~ 5
  )
  result <- multieam:::evaluate_with_dt(test_formula, data = list(), n = 7)
  expect_equal(length(result$a), 7)
  expect_true(all(result$a == 5))
})

test_that("evaluate_with_dt works with chain", {
  test_formula <- list(
    a ~ 1,
    b ~ a + 2
  )
  result <- multieam:::evaluate_with_dt(test_formula, data = list(), n = 1)
  expect_equal(result$a, 1)
  expect_equal(result$b, 3)
})

# test_that("run_trial with simple parameters", {
#   n_items <- 5
#   trial_formula <- list(
#     A ~ 1,
#     V ~ 1,
#     ndt ~ 0.3
#   )

#   res <- run_trial(
#     trial_setting = list(),
#     item_formulas = trial_formula,
#     n_item = n_items,
#     dt = 0.01,
#     max_reached = n_items,
#     max_t = 10,
#     noise_mechanism = "add",
#     noise_factory = function(trial_setting) {
#       function(n, dt) {
#         rnorm(n, 0, 0)
#       }
#     },
#     trajectories = TRUE
#   )

#   expect_equal(length(res$rt), n_items)
#   expect_equal(length(res$response), n_items)
#   expect_equal(nrow(res$.item_params), n_items)
# })
