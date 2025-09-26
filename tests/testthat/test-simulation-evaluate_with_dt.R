# test evaluate_with_dt
test_that("evaluate_with_dt works with numeric", {
  test_formulas <- list(
    a ~ 5
  )
  result <- multieam:::evaluate_with_dt(test_formulas, data = list(), n = 1)
  expect_equal(result$a, 5)
})

test_that("evaluate_with_dt works with numeric vec", {
  test_formulas <- list(
    a ~ 5
  )
  result <- multieam:::evaluate_with_dt(test_formulas, data = list(), n = 7)
  expect_equal(length(result$a), 7)
  expect_true(all(result$a == 5))
})

test_that("evaluate_with_dt works with chain", {
  test_formulas <- list(
    a ~ 1,
    b ~ a + 2
  )
  result <- multieam:::evaluate_with_dt(test_formulas, data = list(), n = 1)
  expect_equal(result$a, 1)
  expect_equal(result$b, 3)
})

test_that("run_trial with simple parameters", {
  n_items <- 5
  trial_formulas <- list(
    A ~ 1,
    V ~ 1,
    ndt ~ 0.01
  )

  res <- multieam:::run_trial(
    trial_setting = list(),
    item_formulas = trial_formulas,
    n_items = n_items,
    max_reached = n_items,
    max_t = 10,
    dt = 0.01,
    noise_mechanism = "add",
    noise_factory = function(trial_setting) {
      function(n, dt) {
        rnorm(n, 0, 0)
      }
    },
    trajectories = TRUE
  )

  expect_equal(length(res$rts), n_items)
  expect_equal(length(res$item_idx), n_items)
  expect_equal(length(res$.item_params$A), n_items)
  expect_equal(length(res$.item_params$V), n_items)
})

test_that("run_trial with varying parameters", {
  n_items <- 5
  trial_formulas <- list(
    n_items ~ 5,
    A ~ 1,
    # Drift rates that give approx RT of [1, 2, 3, 4, 5]
    V ~ c(1, 0.5, 1 / 3, 0.25, 0.2),
    ndt ~ distributional::dist_uniform(-1e-5, 1e-5)
  )

  res <- multieam:::run_trial(
    trial_setting = list(),
    item_formulas = trial_formulas,
    n_items = n_items,
    max_reached = n_items,
    max_t = 10,
    dt = 0.01,
    noise_mechanism = "add",
    noise_factory = function(trial_setting) {
      function(n, dt) {
        rep(0, n)
      }
    },
    trajectories = TRUE
  )

  expect_equal(length(res$rts), n_items)
  expect_equal(length(res$item_idx), n_items)
  expect_equal(length(res$.item_params$A), n_items)
  expect_true(all(sapply(res$.item_params, length) == n_items))
  expect_equal(res$item_idx, seq(1, n_items))
  expect_equal(res$rts, seq(1, n_items), tolerance = 1e-1)
})
