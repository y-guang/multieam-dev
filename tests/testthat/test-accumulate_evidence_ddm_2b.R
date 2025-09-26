test_that("accumulate_evidence_ddm_2b returns expected output", {
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(10),
    A_lower = c(-10),
    V = c(1),
    ndt = c(1),
    max_t = 30,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  expect_true("choices" %in% names(result))
})

test_that(
  "accumulate_evidence_ddm_2b returns expected output, never reaching threshold",
  {
    result <- accumulate_evidence_ddm_2b(
      A_upper = c(10),
      A_lower = c(-10),
      V = c(0),
      ndt = c(1),
      max_t = 5,
      dt = 0.01,
      max_reached = 1,
      noise_mechanism = "add",
      noise_func = function(n, dt) rep(0, n)
    )

    expect_true("item_idx" %in% names(result))
    expect_true("rts" %in% names(result))
    expect_true("choices" %in% names(result))
    expect_true(length(result$rts) == 0)
    expect_true(length(result$item_idx) == 0)
    expect_true(length(result$choices) == 0)
  }
)

# Test multiple items with different drift rates
test_that("accumulate_evidence_ddm_2b handles multiple items", {
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(5, 8, 10),
    A_lower = c(-5, -8, -10),
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
  expect_true("rts" %in% names(result))
  expect_true("choices" %in% names(result))
  expect_true(length(result$item_idx) <= 3)
  expect_true(length(result$rts) == length(result$item_idx))
  expect_true(length(result$choices) == length(result$item_idx))
})

# Test multiplicative evidence noise mechanism
test_that("accumulate_evidence_ddm_2b works with multiplicative evidence noise", {
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(5),
    A_lower = c(-5),
    V = c(1),
    ndt = c(0.5),
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "mult_evidence",
    noise_func = function(n, dt) rep(0.1, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  expect_true("choices" %in% names(result))
})

# Test multiplicative noise mechanism on t
test_that("accumulate_evidence_ddm_2b works with multiplicative noise on t", {
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(5),
    A_lower = c(-5),
    V = c(1),
    ndt = c(0.5),
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "mult_t",
    noise_func = function(n, dt) rep(0.1, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  expect_true("choices" %in% names(result))
})

# Test with positive noise favoring upper bound
test_that("accumulate_evidence_ddm_2b works with positive noise", {
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(10),
    A_lower = c(-10),
    V = c(0.5),
    ndt = c(1),
    max_t = 30,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0.5, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  expect_true("choices" %in% names(result))
  # With positive noise, should reach upper
  if (length(result$choices) > 0) {
    expect_true(result$choices[1] == 1) # Should reach upper bound
  }
})

# Test with negative noise favoring lower bound
test_that("accumulate_evidence_ddm_2b works with negative noise", {
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(5),
    A_lower = c(-5),
    V = c(1),
    ndt = c(0.5),
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(-1, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  expect_true("choices" %in% names(result))
  # With strong negative noise, should reach lower threshold
  if (length(result$choices) > 0) {
    expect_true(result$choices[1] == -1) # Should reach lower bound
  }
})

# Test parameter validation - invalid A_upper length
test_that("accumulate_evidence_ddm_2b validates A_upper parameter length", {
  expect_error(
    accumulate_evidence_ddm_2b(
      A_upper = c(5, 8, 10, 12), # Too many thresholds
      A_lower = c(-5, -8),
      V = c(1, 2),
      ndt = c(0.5, 0.8),
      max_t = 20,
      dt = 0.01,
      max_reached = 2,
      noise_mechanism = "add",
      noise_func = function(n, dt) rep(0, n)
    ),
    "Length of A_upper must be <= number of items"
  )
})

# Test parameter validation - A_upper and A_lower length mismatch
test_that("accumulate_evidence_ddm_2b validates A_upper and A_lower length match", {
  expect_error(
    accumulate_evidence_ddm_2b(
      A_upper = c(5, 8),
      A_lower = c(-5), # Different length from A_upper
      V = c(1, 2),
      ndt = c(0.5, 0.8),
      max_t = 20,
      dt = 0.01,
      max_reached = 1, # Changed to 1 to avoid the >= max_reached validation
      noise_mechanism = "add",
      noise_func = function(n, dt) rep(0, n)
    ),
    "A_upper and A_lower must have the same length"
  )
})

# Test parameter validation - invalid max_reached
test_that("accumulate_evidence_ddm_2b validates max_reached parameter", {
  expect_error(
    accumulate_evidence_ddm_2b(
      A_upper = c(5),
      A_lower = c(-5),
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
test_that("accumulate_evidence_ddm_2b validates ndt parameter length", {
  expect_error(
    accumulate_evidence_ddm_2b(
      A_upper = c(5),
      A_lower = c(-5),
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
test_that("accumulate_evidence_ddm_2b validates dt parameter", {
  expect_error(
    accumulate_evidence_ddm_2b(
      A_upper = c(5),
      A_lower = c(-5),
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
test_that("accumulate_evidence_ddm_2b validates noise_mechanism parameter", {
  expect_error(
    accumulate_evidence_ddm_2b(
      A_upper = c(5),
      A_lower = c(-5),
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
test_that("accumulate_evidence_ddm_2b validates noise_func parameter", {
  expect_error(
    accumulate_evidence_ddm_2b(
      A_upper = c(5),
      A_lower = c(-5),
      V = c(1),
      ndt = c(0.5),
      max_t = 20,
      dt = 0.01,
      max_reached = 1,
      noise_mechanism = "add",
      noise_func = NULL # NULL function
    )
  )
})

# Test noise function return value validation
test_that("accumulate_evidence_ddm_2b validates noise function return length", {
  expect_error(
    accumulate_evidence_ddm_2b(
      A_upper = c(5),
      A_lower = c(-5),
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
test_that("accumulate_evidence_ddm_2b works with random noise", {
  set.seed(123)
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(8),
    A_lower = c(-8),
    V = c(1),
    ndt = c(0.5),
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rnorm(n, 0, 0.5)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  expect_true("choices" %in% names(result))
})

# Test timeout behavior with multiple items
test_that("accumulate_evidence_ddm_2b handles timeout correctly", {
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(100, 200), # Very high thresholds
    A_lower = c(-100, -200), # Very low thresholds
    V = c(0.1, 0.1), # Very slow drift
    ndt = c(0.5, 0.8),
    max_t = 2, # Short timeout
    dt = 0.01,
    max_reached = 2,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  expect_true("choices" %in% names(result))
  expect_true(length(result$item_idx) == 0) # Should timeout before reaching
  expect_true(length(result$rts) == 0)
  expect_true(length(result$choices) == 0)
})

# Test max_reached limit
test_that("accumulate_evidence_ddm_2b respects max_reached limit", {
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(1, 2), # Low thresholds, easy to reach
    A_lower = c(-1, -2),
    V = c(2, 2), # High drift rates
    ndt = c(0.1, 0.1),
    max_t = 20,
    dt = 0.01,
    max_reached = 1, # Limit to 1 item
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  expect_true("choices" %in% names(result))
  expect_true(length(result$item_idx) <= 1) # Should not exceed max_reached
  expect_true(length(result$rts) <= 1)
  expect_true(length(result$choices) <= 1)
})

# Test item indexing (1-based)
test_that("accumulate_evidence_ddm_2b returns 1-based item indices", {
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(2, 3, 4), # Provide thresholds for all items
    A_lower = c(-2, -3, -4),
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
test_that("accumulate_evidence_ddm_2b reaction times include ndt", {
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(1), # Low threshold for quick response
    A_lower = c(-1),
    V = c(10), # High drift rate
    ndt = c(2), # 2 second non-decision time
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  if (length(result$rts) > 0) {
    expect_true(result$rts[1] >= 2) # RT should include ndt
  }
})

# Test choices are correct for bounds reached
test_that("accumulate_evidence_ddm_2b choices correctly indicate bound reached", {
  # Test upper bound with positive drift
  result_upper <- accumulate_evidence_ddm_2b(
    A_upper = c(5),
    A_lower = c(-50), # Make lower bound very unlikely
    V = c(2), # Positive drift toward upper
    ndt = c(0.5),
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  if (length(result_upper$choices) > 0) {
    expect_true(result_upper$choices[1] == 1) # Should reach upper bound
  }

  # Test lower bound with negative drift
  result_lower <- accumulate_evidence_ddm_2b(
    A_upper = c(50), # Make upper bound very unlikely
    A_lower = c(-5),
    V = c(-2), # Negative drift toward lower
    ndt = c(0.5),
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  if (length(result_lower$choices) > 0) {
    expect_true(result_lower$choices[1] == -1) # Should reach lower bound
  }
})

# Test simple calculation for upper bound
test_that("accumulate_evidence_ddm_2b calculation, linear accumulation, single item, upper bound", {
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(10), # Upper threshold
    A_lower = c(-100), # Very low lower threshold to avoid reaching it
    V = c(1), # Positive drift rate
    ndt = c(2), # 2 second non-decision time
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  expect_equal(length(result$rts), 1)
  expect_equal(result$item_idx, 1)
  expect_equal(result$choices, 1) # Upper bound
  expect_equal(result$rts, 2 + 10 / 1, tolerance = 0.2)
})

# Test simple calculation for lower bound
test_that("accumulate_evidence_ddm_2b calculation, linear accumulation, single item, lower bound", {
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(100), # Very high upper threshold to avoid reaching it
    A_lower = c(-10), # Lower threshold
    V = c(-1), # Negative drift rate
    ndt = c(2), # 2 second non-decision time
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  expect_equal(length(result$rts), 1)
  expect_equal(result$item_idx, 1)
  expect_equal(result$choices, -1) # Lower bound
  expect_equal(result$rts, 2 + 10 / 1, tolerance = 0.2) # Same time, different direction
})

# Test list linear accumulation with both bounds
test_that("accumulate_evidence_ddm_2b calculation, list linear accumulation", {
  A_upper <- rep(10, 4)
  A_lower <- rep(-10, 4)
  V <- c(1, 2, -3, -4) # Mix of positive and negative drifts
  ndt <- rep(2, 4)
  expected_values <- 2 + 10 / abs(V) # Time to reach either bound
  result <- accumulate_evidence_ddm_2b(
    A_upper = A_upper,
    A_lower = A_lower,
    V = V,
    ndt = ndt,
    max_t = 20,
    dt = 0.01,
    max_reached = 4,
    noise_mechanism = "add",
    noise_func = function(n, dt) rep(0, n)
  )

  expect_equal(length(result$rts), 4)
  # Should be in order of fastest (highest absolute drift)
  expect_equal(result$item_idx, c(4, 3, 2, 1))
  # Check choices match drift directions
  expect_equal(result$choices, c(-1, -1, 1, 1)) # neg drift -> lower, pos drift -> upper
  expect_equal(result$rts, expected_values[c(4, 3, 2, 1)], tolerance = 0.02)
})

# Test average rt close to prediction with two bounds
test_that("accumulate_evidence_ddm_2b average rt close to prediction", {
  set.seed(42)
  n_items <- 10
  n_trials <- 50
  A_upper <- rep(10, n_items)
  A_lower <- rep(-10, n_items)
  V <- seq(0.5, 5, length.out = n_items)
  ndt <- rep(1, n_items)
  all_rts <- c()

  for (i in 1:n_trials) {
    result <- accumulate_evidence_ddm_2b(
      A_upper = A_upper,
      A_lower = A_lower,
      V = V,
      ndt = ndt,
      max_t = 30,
      dt = 0.01,
      max_reached = n_items,
      noise_mechanism = "add",
      noise_func = function(n, dt) rnorm(n, 0, sqrt(dt))
    )
    all_rts <- c(all_rts, result$rts)
  }

  predicted_rts <- ndt + A_upper / V
  avg_predicted_rt <- mean(predicted_rts)
  avg_simulated_rt <- mean(all_rts)
  expect_true(abs(avg_simulated_rt - avg_predicted_rt) < 1) # Within 1 second
})

# Test symmetric bounds with zero drift
test_that("accumulate_evidence_ddm_2b with symmetric bounds and zero drift", {
  set.seed(456)
  result <- accumulate_evidence_ddm_2b(
    A_upper = c(5),
    A_lower = c(-5),
    V = c(0), # Zero drift
    ndt = c(0.5),
    max_t = 30,
    dt = 0.01,
    max_reached = 1,
    noise_mechanism = "add",
    noise_func = function(n, dt) rnorm(n, 0, sqrt(dt))
  )

  # With zero drift and symmetric bounds, should eventually reach one bound
  # due to random walk
  expect_true("choices" %in% names(result))
  if (length(result$choices) > 0) {
    expect_true(result$choices[1] %in% c(-1, 1)) # Should reach either bound
  }
})
