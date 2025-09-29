
# Tests for run_condition function
test_that("run_condition with basic setup", {
  n_items <- 4

  between_trial_formulas <- list(
    n_items ~ 4,
    A ~ 2,
    V_0 ~ 0.5,
    ndt ~ 0.1
  )

  item_formulas <- list(
    A ~ A,
    V ~ V_0,
    ndt ~ ndt
  )

  noise_factory <- function(condition_setting) {
    function(n, dt) {
      rep(0, n)
    }
  }

  res <- multieam:::run_condition(
    condition_setting = list(),
    between_trial_formulas = between_trial_formulas,
    item_formulas = item_formulas,
    n_trials = 3,
    n_items = n_items,
    max_reached = n_items,
    max_t = 10,
    dt = 0.01,
    noise_mechanism = "add",
    noise_factory = noise_factory,
    model = "ddm",
    trajectories = FALSE
  )

  # Check structure
  expect_true(is.list(res))
  expect_true("result" %in% names(res))
  expect_true("cond_params" %in% names(res))

  # Check results
  expect_equal(length(res$result), 3) # 3 trials

  # Check each trial result
  for (trial in res$result) {
    expect_true(is.list(trial))
    expect_true("rt" %in% names(trial))
    expect_true("item_idx" %in% names(trial))
    expect_equal(length(trial$rt), n_items)
    expect_equal(length(trial$item_idx), n_items)
  }

  # Check condition parameters
  expect_true(is.list(res$cond_params))
  expect_equal(length(res$cond_params$n_items), 3)
  expect_equal(length(res$cond_params$A), 3)
  expect_equal(length(res$cond_params$V_0), 3)
  expect_equal(length(res$cond_params$ndt), 3)
})

test_that("run_condition with distributional parameters", {
  n_items <- 5

  between_trial_formulas <- list(
    n_items ~ 5,
    A ~ 3,
    V_0 ~ distributional::dist_uniform(0.01, 0.02),
    ndt ~ 0.1
  )

  item_formulas <- list(
    A ~ A,
    V ~ V_0 + 0.1 * seq(n_items),
    ndt ~ ndt
  )

  noise_factory <- function(condition_setting) {
    function(n, dt) {
      rep(0, n)
    }
  }

  res <- multieam:::run_condition(
    condition_setting = list(),
    between_trial_formulas = between_trial_formulas,
    item_formulas = item_formulas,
    n_trials = 5,
    n_items = n_items,
    max_reached = n_items,
    max_t = 10,
    dt = 0.01,
    noise_mechanism = "add",
    noise_factory = noise_factory,
    model = "ddm",
    trajectories = TRUE
  )

  # Check structure
  expect_equal(length(res$result), 5) # 5 trials

  # Check that V_0 varies between trials (due to distribution)
  v0_values <- res$cond_params$V_0
  expect_true(all(v0_values >= 0.01 & v0_values <= 0.02))

  # Check trajectories are included
  for (trial in res$result) {
    expect_true(".item_params" %in% names(trial))
    expect_equal(length(trial$.item_params$V), n_items)
    # Check that V increases with item sequence
    expect_true(all(diff(trial$.item_params$V) > 0))
  }
})

test_that("run_condition with condition setting input", {
  n_items <- 3

  between_trial_formulas <- list(
    n_items ~ 3,
    A ~ base_difficulty + 1,
    V_0 ~ 0.5,
    ndt ~ 0.1
  )

  item_formulas <- list(
    A ~ A,
    V ~ V_0,
    ndt ~ ndt
  )

  noise_factory <- function(condition_setting) {
    function(n, dt) {
      rep(0, n)
    }
  }

  condition_setting <- list(base_difficulty = 2.5)

  res <- multieam:::run_condition(
    condition_setting = condition_setting,
    between_trial_formulas = between_trial_formulas,
    item_formulas = item_formulas,
    n_trials = 2,
    n_items = n_items,
    max_reached = n_items,
    max_t = 10,
    dt = 0.01,
    noise_mechanism = "add",
    noise_factory = noise_factory,
    model = "ddm",
    trajectories = FALSE
  )

  # Check that base_difficulty was used
  expect_equal(unique(res$cond_params$A), 3.5) # 2.5 + 1
})

test_that("run_condition with noise", {
  n_items <- 3

  between_trial_formulas <- list(
    n_items ~ 3,
    A ~ 2,
    V_0 ~ 0.5,
    ndt ~ 0.1,
    noise_sd ~ 0.1
  )

  item_formulas <- list(
    A ~ A,
    V ~ V_0,
    ndt ~ ndt
  )

  noise_factory <- function(condition_setting) {
    function(n, dt) {
      rnorm(n, 0, condition_setting$noise_sd)
    }
  }

  res <- multieam:::run_condition(
    condition_setting = list(),
    between_trial_formulas = between_trial_formulas,
    item_formulas = item_formulas,
    n_trials = 2,
    n_items = n_items,
    max_reached = n_items,
    max_t = 10,
    dt = 0.01,
    noise_mechanism = "add",
    noise_factory = noise_factory,
    model = "ddm",
    trajectories = FALSE
  )

  # Check structure is still correct
  expect_equal(length(res$result), 2)
  expect_true("noise_sd" %in% names(res$cond_params))
  expect_equal(unique(res$cond_params$noise_sd), 0.1)
})
