test_that("accumulate_evidence_ddm returns expected output", {
  result <- accumulate_evidence_ddm(
    A = c(10),
    V = c(1),
    ndt = c(1),
    max_t = 30,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rt" %in% names(result))
})

test_that(
  "accumulate_evidence_ddm returns expected output, never reaching threshold",
  {
    result <- accumulate_evidence_ddm(
      A = c(10),
      V = c(0),
      ndt = c(1),
      max_t = 5,
      dt = 0.01,
      max_reached = 1,
      noise_mechanism = "add",
      noise_func = function(n, dt) rep(0, n)
    )

    expect_true("item_idx" %in% names(result))
    expect_true("rt" %in% names(result))
    expect_true(length(result$rt) == 0)
    expect_true(length(result$item_idx) == 0)
  }
)

# Test multiple items with different drift rates
test_that("accumulate_evidence_ddm handles multiple items", {
  result <- accumulate_evidence_ddm(
    A = c(5, 8, 10),
    V = c(2, 1, 0.5),
    ndt = c(0.5, 0.8, 1.0),
    max_t = 20,
    dt = 0.01,
    max_reached = 3,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  expect_true(is.list(result))
  expect_true("item_idx" %in% names(result))
  expect_true("rt" %in% names(result))
  expect_true(length(result$item_idx) <= 3)
  expect_true(length(result$rt) == length(result$item_idx))
})

# Test multiplicative noise mechanism
test_that("accumulate_evidence_ddm works with multiplicative noise on t", {
  result <- accumulate_evidence_ddm(
    A = c(5),
    V = c(1),
    ndt = c(0.5),
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "mult_t",
    noise_func = function(n, dt) rep(0.1, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rt" %in% names(result))
})

# Test with positive noise
test_that("accumulate_evidence_ddm works with positive noise", {
  result <- accumulate_evidence_ddm(
    A = c(10),
    V = c(0.5),
    ndt = c(1),
    max_t = 30,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0.5, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rt" %in% names(result))
  # With positive noise, should reach threshold faster
  if (length(result$rt) > 0) {
    expect_true(result$rt[1] < 20) # Should be faster than without noise
  }
})

# Test with negative noise
test_that("accumulate_evidence_ddm works with negative noise", {
  result <- accumulate_evidence_ddm(
    A = c(5),
    V = c(1),
    ndt = c(0.5),
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(-0.2, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rt" %in% names(result))
})

# Test parameter validation - invalid A length
test_that("accumulate_evidence_ddm validates A parameter length", {
  expect_error(
    accumulate_evidence_ddm(
      A = c(5, 8, 10, 12), # Too many thresholds
      V = c(1, 2),
      ndt = c(0.5, 0.8),
      max_t = 20,
      dt = 0.01,
      max_reached = 2,
      noise_mechanism = "add",
      noise_func = function(n, dt) rep(0, n)
    ),
    "Length of A must be <= number of items"
  )
})

# Test parameter validation - invalid max_reached
test_that("accumulate_evidence_ddm validates max_reached parameter", {
  expect_error(
    accumulate_evidence_ddm(
      A = c(5),
      V = c(1, 2),
      ndt = c(0.5, 0.8),
      max_t = 20,
      dt = 0.01,
      max_reached = 0, # Invalid: must be > 0
      noise_mechanism = "add",
      noise_func = function(n, dt) rep(0, n)
    ),
    "max_reached must be > 0"
  )
})

# Test parameter validation - ndt length mismatch
test_that("accumulate_evidence_ddm validates ndt parameter length", {
  expect_error(
    accumulate_evidence_ddm(
      A = c(5),
      V = c(1, 2),
      ndt = c(0.5), # Length mismatch with V
      max_t = 20,
      dt = 0.01,
      max_reached = 1,
      noise_mechanism = "add",
      noise_func = function(n, dt) rep(0, n)
    ),
    "Length of ndt must be equal to number of items"
  )
})

# Test parameter validation - invalid dt
test_that("accumulate_evidence_ddm validates dt parameter", {
  expect_error(
    accumulate_evidence_ddm(
      A = c(5),
      V = c(1),
      ndt = c(0.5),
      max_t = 20,
      dt = 0, # Invalid: must be > 0
      max_reached = 1,
      noise_mechanism = "add",
      noise_func = function(n, dt) rep(0, n)
    ),
    "dt and max_t must be > 0"
  )
})

# Test parameter validation - invalid noise mechanism
test_that("accumulate_evidence_ddm validates noise_mechanism parameter", {
  expect_error(
    accumulate_evidence_ddm(
      A = c(5),
      V = c(1),
      ndt = c(0.5),
      max_t = 20,
      dt = 0.01,
      max_reached = 1,
      noise_mechanism = "invalid", # Invalid mechanism
      noise_func = function(n, dt) rep(0, n)
    ),
    "noise_mechanism must be"
  )
})

# Test parameter validation - NULL noise function
test_that("accumulate_evidence_ddm validates noise_func parameter", {
  expect_error(
    accumulate_evidence_ddm(
      A = c(5),
      V = c(1),
      ndt = c(0.5),
      max_t = 20,
      dt = 0.01,
      max_reached = 1,
      noise_mechanism = "add",
      noise_func = NULL # NULL function
    ),
    "Cannot convert object to a function" # Rcpp error message
  )
})

# Test noise function return value validation
test_that("accumulate_evidence_ddm validates noise function return length", {
  expect_error(
    accumulate_evidence_ddm(
      A = c(5),
      V = c(1),
      ndt = c(0.5),
      max_t = 20,
      dt = 0.01,
      max_reached = 1,
      noise_mechanism = "add",
      noise_func = function(n, dt) rep(0, n - 1) # Returns wrong length
    ),
    "Custom noise function signature"
  )
})

# Test with random noise function
test_that("accumulate_evidence_ddm works with random noise", {
  set.seed(123)
  result <- accumulate_evidence_ddm(
    A = c(8),
    V = c(1),
    ndt = c(0.5),
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rnorm(n, 0, 0.5)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rt" %in% names(result))
})

# Test timeout behavior with multiple items
test_that("accumulate_evidence_ddm handles timeout correctly", {
  result <- accumulate_evidence_ddm(
    A = c(100, 200), # Very high thresholds
    V = c(0.1, 0.1), # Very slow drift
    ndt = c(0.5, 0.8),
    max_t = 2, # Short timeout
    dt = 0.01,
    max_reached = 2,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rt" %in% names(result))
  expect_true(length(result$item_idx) == 0) # Should timeout before reaching
  expect_true(length(result$rt) == 0)
})

# Test max_reached limit
test_that("accumulate_evidence_ddm respects max_reached limit", {
  result <- accumulate_evidence_ddm(
    A = c(1, 2), # Low thresholds, easy to reach
    V = c(2, 2), # High drift rates
    ndt = c(0.1, 0.1),
    max_t = 20,
    dt = 0.01,
    max_reached = 1, # Limit to 1 item
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rt" %in% names(result))
  expect_true(length(result$item_idx) <= 1) # Should not exceed max_reached
  expect_true(length(result$rt) <= 1)
})

# Test item indexing (1-based)
test_that("accumulate_evidence_ddm returns 1-based item indices", {
  result <- accumulate_evidence_ddm(
    A = c(2, 3, 4), # Provide thresholds for all items
    V = c(1, 0.5, 2), # Different drift rates
    ndt = c(0.1, 0.1, 0.1),
    max_t = 20,
    dt = 0.01,
    max_reached = 3,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  if (length(result$item_idx) > 0) {
    expect_true(all(result$item_idx >= 1)) # Should be 1-based
    expect_true(all(result$item_idx <= 3)) # Should not exceed number of items
  }
})

# Test reaction times include non-decision time
test_that("accumulate_evidence_ddm reaction times include ndt", {
  result <- accumulate_evidence_ddm(
    A = c(1), # Low threshold for quick response
    V = c(10), # High drift rate
    ndt = c(2), # 2 second non-decision time
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  if (length(result$rt) > 0) {
    expect_true(result$rt[1] >= 2) # RT should include ndt
  }
})

# Test simple calculation
test_that("accumulate_evidence_ddm calculation, linear accumulation, single item", {
  result <- accumulate_evidence_ddm(
    A = c(10), # Low threshold for quick response
    V = c(1), # High drift rate
    ndt = c(2), # 2 second non-decision time
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  expect_equal(length(result$rt), 1)
  expect_equal(result$item_idx, 1)
  expect_equal(result$rt, 2 + 10 / 1, tolerance = 0.2)
})

# Test list linear accumulation
test_that("accumulate_evidence_ddm calculation, list linear accumulation", {
  A <- rep(10, 4)
  V <- c(1, 2, 3, 4)
  ndt <- rep(2, 4)
  expected_values <- 2 + A / V
  result <- accumulate_evidence_ddm(
    A = A,
    V = V,
    ndt = ndt,
    max_t = 20,
    dt = 0.01,
    max_reached = 4,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )
  expect_equal(length(result$rt), 4)
  expect_equal(result$item_idx, c(4, 3, 2, 1)) # Should be in order of fastest
  expect_equal(result$rt, expected_values[c(4, 3, 2, 1)], tolerance = 0.02)
})

# Test average rt close to prediction
test_that("accumulate_evidence_ddm average rt close to prediction", {
  set.seed(42)
  n_items <- 10
  n_trials <- 100
  A <- rep(10, n_items)
  V <- seq(0.5, 5, length.out = n_items)
  ndt <- rep(1, n_items)
  all_reaction_time <- c()
  for (i in 1:n_trials) {
    result <- accumulate_evidence_ddm(
      A = A,
      V = V,
      ndt = ndt,
      max_t = 30,
      dt = 0.01,
      max_reached = n_items,
      noise_mechanism = "add",
      noise_func = function(n, dt) rnorm(n, 0, sqrt(dt))
    )
    all_reaction_time <- c(all_reaction_time, result$rt)
  }
  predicted_reaction_time <- ndt + A / V
  avg_predicted_reaction_time <- mean(predicted_reaction_time)
  avg_simulated_reaction_time <- mean(all_reaction_time)
  expect_true(abs(avg_simulated_reaction_time - avg_predicted_reaction_time) < 1) # Within 1 second
})
