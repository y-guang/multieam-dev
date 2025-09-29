test_that("accumulate_evidence_lca_gi returns expected output", {
  result <- accumulate_evidence_lca_gi(
    A = c(10),
    V = c(1),
    ndt = c(1),
    beta = c(0.1),
    k = c(0.05),
    max_t = 30,
    dt = 0.01,
    max_reached = 1,
    noise_func = function(n, dt) rep(0, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
})

test_that(
  "accumulate_evidence_lca_gi returns expected output, never reaching threshold",
  {
    result <- accumulate_evidence_lca_gi(
      A = c(100), # Very high threshold
      V = c(0), # Zero drift
      ndt = c(1),
      beta = c(0.1),
      k = c(0.05),
      max_t = 5,
      dt = 0.01,
      max_reached = 1,
      noise_func = function(n, dt) rep(0, n)
    )

    expect_true("item_idx" %in% names(result))
    expect_true("rts" %in% names(result))
    expect_true(length(result$rts) == 0)
    expect_true(length(result$item_idx) == 0)
  }
)

# Test multiple items with different drift rates
test_that("accumulate_evidence_lca_gi handles multiple items", {
  result <- accumulate_evidence_lca_gi(
    A = c(5, 8, 10),
    V = c(2, 1, 0.5),
    ndt = c(0.5, 0.8, 1.0),
    beta = c(0.1, 0.15, 0.2),
    k = c(0.05, 0.08, 0.1),
    max_t = 20,
    dt = 0.01,
    max_reached = 3,
    noise_func = function(n, dt) rep(0, n)
  )

  expect_true(is.list(result))
  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  expect_true(length(result$item_idx) <= 3)
  expect_true(length(result$rts) == length(result$item_idx))
})

# Test with positive noise favoring accumulation
test_that("accumulate_evidence_lca_gi works with positive noise", {
  result <- accumulate_evidence_lca_gi(
    A = c(10),
    V = c(0.5),
    ndt = c(1),
    beta = c(0.1),
    k = c(0.05),
    max_t = 30,
    dt = 0.01,
    max_reached = 1,
    noise_func = function(n, dt) rep(0.5, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  # With positive noise, should help reach threshold
  if (length(result$item_idx) > 0) {
    expect_true(result$item_idx[1] == 1) # Should reach threshold
  }
})

# Test with negative noise hindering accumulation
test_that("accumulate_evidence_lca_gi works with negative noise", {
  result <- accumulate_evidence_lca_gi(
    A = c(5),
    V = c(1),
    ndt = c(0.5),
    beta = c(0.1),
    k = c(0.05),
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_func = function(n, dt) rep(-0.3, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  # With negative noise, may still reach threshold but takes longer
})

# Test parameter validation - invalid A length
test_that("accumulate_evidence_lca_gi validates A parameter length", {
  expect_error(
    accumulate_evidence_lca_gi(
      A = c(5, 8, 10, 12), # Too many thresholds
      V = c(1, 2),
      ndt = c(0.5, 0.8),
      beta = c(0.1, 0.15),
      k = c(0.05, 0.08),
      max_t = 20,
      dt = 0.01,
      max_reached = 2,
      noise_func = function(n, dt) rep(0, n)
    ),
    "Length of A must be <= number of items"
  )
})

# Test parameter validation - A length less than max_reached
test_that("accumulate_evidence_lca_gi validates A length >= max_reached", {
  expect_error(
    accumulate_evidence_lca_gi(
      A = c(5), # Only 1 threshold but max_reached is 2
      V = c(1, 2),
      ndt = c(0.5, 0.8),
      beta = c(0.1, 0.15),
      k = c(0.05, 0.08),
      max_t = 20,
      dt = 0.01,
      max_reached = 2,
      noise_func = function(n, dt) rep(0, n)
    ),
    "Length of A must be <= number of items and >= max_reached"
  )
})

# Test parameter validation - invalid max_reached
test_that("accumulate_evidence_lca_gi validates max_reached parameter", {
  expect_error(
    accumulate_evidence_lca_gi(
      A = c(5),
      V = c(1, 2),
      ndt = c(0.5, 0.8),
      beta = c(0.1, 0.15),
      k = c(0.05, 0.08),
      max_t = 20,
      dt = 0.01,
      max_reached = 0, # Invalid: must be > 0
      noise_func = function(n, dt) rep(0, n)
    ),
    "max_reached must be > 0"
  )
})

# Test parameter validation - max_reached > n_items
test_that("accumulate_evidence_lca_gi validates max_reached <= n_items", {
  expect_error(
    accumulate_evidence_lca_gi(
      A = c(5),
      V = c(1, 2),
      ndt = c(0.5, 0.8),
      beta = c(0.1, 0.15),
      k = c(0.05, 0.08),
      max_t = 20,
      dt = 0.01,
      max_reached = 3, # Invalid: > n_items (2)
      noise_func = function(n, dt) rep(0, n)
    ),
    "Length of A must be <= number of items and >= max_reached"
  )
})

# Test parameter validation - ndt length mismatch
test_that("accumulate_evidence_lca_gi validates ndt parameter length", {
  expect_error(
    accumulate_evidence_lca_gi(
      A = c(5),
      V = c(1, 2),
      ndt = c(0.5), # Length mismatch with V
      beta = c(0.1, 0.15),
      k = c(0.05, 0.08),
      max_t = 20,
      dt = 0.01,
      max_reached = 1,
      noise_func = function(n, dt) rep(0, n)
    ),
    "Length of ndt must be equal to number of items"
  )
})

# Test parameter validation - beta length mismatch
test_that("accumulate_evidence_lca_gi validates beta parameter length", {
  expect_error(
    accumulate_evidence_lca_gi(
      A = c(5),
      V = c(1, 2),
      ndt = c(0.5, 0.8),
      beta = c(0.1), # Length mismatch with V
      k = c(0.05, 0.08),
      max_t = 20,
      dt = 0.01,
      max_reached = 1,
      noise_func = function(n, dt) rep(0, n)
    ),
    "Length of beta must be equal to number of items"
  )
})

# Test parameter validation - k length mismatch
test_that("accumulate_evidence_lca_gi validates k parameter length", {
  expect_error(
    accumulate_evidence_lca_gi(
      A = c(5),
      V = c(1, 2),
      ndt = c(0.5, 0.8),
      beta = c(0.1, 0.15),
      k = c(0.05), # Length mismatch with V
      max_t = 20,
      dt = 0.01,
      max_reached = 1,
      noise_func = function(n, dt) rep(0, n)
    ),
    "Length of k must be equal to number of items"
  )
})

# Test parameter validation - invalid dt
test_that("accumulate_evidence_lca_gi validates dt parameter", {
  expect_error(
    accumulate_evidence_lca_gi(
      A = c(5),
      V = c(1),
      ndt = c(0.5),
      beta = c(0.1),
      k = c(0.05),
      max_t = 20,
      dt = 0, # Invalid: must be > 0
      max_reached = 1,
      noise_func = function(n, dt) rep(0, n)
    ),
    "dt and max_t must be > 0"
  )
})

# Test parameter validation - NULL noise function
test_that("accumulate_evidence_lca_gi validates noise_func parameter", {
  expect_error(
    accumulate_evidence_lca_gi(
      A = c(5),
      V = c(1),
      ndt = c(0.5),
      beta = c(0.1),
      k = c(0.05),
      max_t = 20,
      dt = 0.01,
      max_reached = 1,
      noise_func = NULL # NULL function
    ),
    "Cannot convert object to a function"
  )
})

# Test noise function return value validation
test_that("accumulate_evidence_lca_gi validates noise function return length", {
  expect_error(
    accumulate_evidence_lca_gi(
      A = c(5),
      V = c(1),
      ndt = c(0.5),
      beta = c(0.1),
      k = c(0.05),
      max_t = 20,
      dt = 0.01,
      max_reached = 1,
      noise_func = function(n, dt) rep(0, n - 1) # Returns wrong length
    ),
    "Custom noise function signature"
  )
})

# Test with random noise function
test_that("accumulate_evidence_lca_gi works with random noise", {
  set.seed(123)
  result <- accumulate_evidence_lca_gi(
    A = c(8),
    V = c(1),
    ndt = c(0.5),
    beta = c(0.1),
    k = c(0.05),
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_func = function(n, dt) rnorm(n, 0, 0.5)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
})

# Test timeout behavior with multiple items
test_that("accumulate_evidence_lca_gi handles timeout correctly", {
  result <- accumulate_evidence_lca_gi(
    A = c(100, 200), # Very high thresholds
    V = c(0.1, 0.1), # Very slow drift
    ndt = c(0.5, 0.8),
    beta = c(0.1, 0.15),
    k = c(0.05, 0.08),
    max_t = 2, # Short timeout
    dt = 0.01,
    max_reached = 2,
    noise_func = function(n, dt) rep(0, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  expect_true(length(result$item_idx) == 0) # Should timeout before reaching
  expect_true(length(result$rts) == 0)
})

# Test max_reached limit
test_that("accumulate_evidence_lca_gi respects max_reached limit", {
  result <- accumulate_evidence_lca_gi(
    A = c(1, 2), # Low thresholds, easy to reach
    V = c(2, 2), # High drift rates
    ndt = c(0.1, 0.1),
    beta = c(0.05, 0.05), # Low inhibition
    k = c(0.02, 0.02), # Low leakage
    max_t = 20,
    dt = 0.01,
    max_reached = 1, # Limit to 1 item
    noise_func = function(n, dt) rep(0, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  expect_true(length(result$item_idx) <= 1) # Should not exceed max_reached
  expect_true(length(result$rts) <= 1)
})

# Test item indexing (1-based)
test_that("accumulate_evidence_lca_gi returns 1-based item indices", {
  result <- accumulate_evidence_lca_gi(
    A = c(2, 3, 4), # Provide thresholds for all items
    V = c(1, 0.5, 2), # Different drift rates
    ndt = c(0.1, 0.1, 0.1),
    beta = c(0.1, 0.15, 0.2),
    k = c(0.05, 0.08, 0.1),
    max_t = 20,
    dt = 0.01,
    max_reached = 3,
    noise_func = function(n, dt) rep(0, n)
  )

  if (length(result$item_idx) > 0) {
    expect_true(all(result$item_idx >= 1)) # Should be 1-based
    expect_true(all(result$item_idx <= 3)) # Should not exceed number of items
  }
})

# Test reaction times include non-decision time
test_that("accumulate_evidence_lca_gi reaction times include ndt", {
  result <- accumulate_evidence_lca_gi(
    A = c(1), # Low threshold for quick response
    V = c(10), # High drift rate
    ndt = c(2), # 2 second non-decision time
    beta = c(0.05), # Low inhibition
    k = c(0.02), # Low leakage
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_func = function(n, dt) rep(0, n)
  )

  if (length(result$rts) > 0) {
    expect_true(result$rts[1] >= 2) # RT should include ndt
  }
})

# Test leakage effect (k parameter)
test_that("accumulate_evidence_lca_gi leakage affects accumulation", {
  # High leakage: v / k < A, never reaches threshold
  result_high_leak <- accumulate_evidence_lca_gi(
    A = c(2.5),
    V = c(10),
    ndt = c(0),
    beta = c(0),
    k = c(5), # High leakage
    max_t = 100,
    dt = 0.01,
    max_reached = 1,
    noise_func = function(n, dt) rep(0, n)
  )
  
  # Low leakage: v / k > A, should reach threshold
  result_low_leak <- accumulate_evidence_lca_gi(
    A = c(10),
    V = c(10),
    ndt = c(0),
    beta = c(0), # Low inhibition
    k = c(0.5), # Low leakage
    max_t = 100,
    dt = 0.01,
    max_reached = 1,
    noise_func = function(n, dt) rep(0, n)
  )
  expect_equal(length(result_high_leak$rts), 0) # Should not reach
  expect_equal(length(result_low_leak$rts), 1)  # Should reach
})

# Test inhibition effect (beta parameter)
test_that("accumulate_evidence_lca_gi inhibition affects competition", {
  # Test with competing items - higher beta should create stronger inhibition
  result_high_inhibition <- accumulate_evidence_lca_gi(
    A = c(10, 10),
    V = c(1.5, 1),
    ndt = c(0, 0),
    beta = c(1, 0), # High inhibition
    k = c(0, 0),
    max_t = 15,
    dt = 0.01,
    max_reached = 2, # Only one can win
    noise_func = function(n, dt) rep(0, n)
  )

  expect_equal(length(result_high_inhibition$item_idx), 1)
  expect_equal(result_high_inhibition$item_idx, 2) 
})

test_that("accumulate_evidence_lca_gi inhibition disappears after reached", {
  # Test with competing items - higher beta should create stronger inhibition
  result_high_inhibition <- accumulate_evidence_lca_gi(
    A = c(10, 10),
    V = c(1, 1),
    ndt = c(0, 0),
    beta = c(1, 0), # High inhibition
    k = c(0, 0),
    max_t = 22,
    dt = 0.01,
    max_reached = 2, # Only one can win
    noise_func = function(n, dt) rep(0, n)
  )

  expect_equal(length(result_high_inhibition$item_idx), 2)
  expect_equal(result_high_inhibition$item_idx, c(2, 1))
})

# Test evidence floor at zero (evidence cannot go below 0)
test_that("accumulate_evidence_lca_gi maintains evidence floor at zero", {
  # Use very high leakage and inhibition to try to drive evidence negative
  result <- accumulate_evidence_lca_gi(
    A = c(50), # High threshold
    V = c(0.1), # Low drift
    ndt = c(0.1),
    beta = c(2.0), # Very high inhibition
    k = c(2.0), # Very high leakage
    max_t = 10,
    dt = 0.01,
    max_reached = 1,
    noise_func = function(n, dt) rep(-1, n) # Strong negative noise
  )

  # Should handle negative influences gracefully (evidence floored at 0)
  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
})

# Test sequential accumulation with different thresholds
test_that("accumulate_evidence_lca_gi sequential thresholds work correctly", {
  result <- accumulate_evidence_lca_gi(
    A = c(2, 4, 6), # Increasing thresholds
    V = c(1, 1, 1), # Equal drift rates
    ndt = c(0.1, 0.1, 0.1),
    beta = c(0.1, 0.1, 0.1),
    k = c(0.05, 0.05, 0.05),
    max_t = 20,
    dt = 0.01,
    max_reached = 3,
    noise_func = function(n, dt) rep(0, n)
  )

  if (length(result$item_idx) > 1) {
    # First item to reach should be the one with lowest threshold requirement
    # Times should be ordered (first threshold reached first)
    expect_true(all(diff(result$rts) >= 0)) # Non-decreasing reaction times
  }
})

# Test with asymmetric parameters
test_that("accumulate_evidence_lca_gi handles asymmetric item parameters", {
  result <- accumulate_evidence_lca_gi(
    A = c(5, 10),
    V = c(0.5, 2), # Different drift rates
    ndt = c(1, 0.2), # Different non-decision times
    beta = c(0.1, 0.3), # Different inhibition strengths
    k = c(0.02, 0.15), # Different leakage rates
    max_t = 20,
    dt = 0.01,
    max_reached = 2,
    noise_func = function(n, dt) rep(0, n)
  )

  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
  # Item 2 has higher drift but also higher threshold and inhibition
  # Item 1 has lower threshold but also lower drift and higher ndt
  # The outcome depends on the balance of these factors
})

# Test with zero drift rate
test_that("accumulate_evidence_lca_gi handles zero drift rate", {
  set.seed(456)
  result <- accumulate_evidence_lca_gi(
    A = c(5),
    V = c(0), # Zero drift
    ndt = c(0.5),
    beta = c(0.1),
    k = c(0.05),
    max_t = 20,
    dt = 0.01,
    max_reached = 1,
    noise_func = function(n, dt) rnorm(n, 0, sqrt(dt))
  )

  # With zero drift, might reach threshold due to positive noise
  # or might not due to leakage and negative noise
  expect_true("item_idx" %in% names(result))
  expect_true("rts" %in% names(result))
})

# Test calculation consistency for simple case
test_that("accumulate_evidence_lca_gi calculation, single item, no competition", {
  result <- accumulate_evidence_lca_gi(
    A = c(10), # Threshold
    V = c(1), # Drift rate
    ndt = c(2), # 2 second non-decision time
    beta = c(0.1), # Low inhibition (no competition)
    k = c(0.05), # Low leakage
    max_t = 30,
    dt = 0.01,
    max_reached = 1,
    noise_func = function(n, dt) rep(0, n)
  )

  expect_equal(length(result$rts), 1)
  expect_equal(result$item_idx, 1)
  # With low leakage and no competition, should reach threshold
  # Time should be reasonable (ndt + accumulation time)
  expect_true(result$rts[1] >= 2) # At least ndt
  expect_true(result$rts[1] < 30) # Should reach before timeout
})