# Minimal test script for accumulate_evidence_ddm_opt accuracy
# Test with zero noise to verify RT = A/V relationship

# Load the compiled functions
cat("Loading compiled functions...\n")
Rcpp::sourceCpp("src/core.cpp")

# Define zero noise function
zero_noise <- function(n, dt) {
  rep(0, n)
}

# Test parameters
cat("Setting up test parameters...\n")
A <- c(10)          # Single threshold
V <- c(2.0)         # Single drift rate  
ndt <- c(0)         # Zero non-decision time
dt <- 0.01          # Time step
max_reached <- 1    # Only need one item
max_t <- 20         # Max time
noise_mechanism <- "add"

# Expected RT calculation
expected_rt <- A[1] / V[1]  # Should be 10/2 = 5.0
cat("Expected RT (A/V):", expected_rt, "\n")

# Test the optimized function
cat("Testing accumulate_evidence_ddm_opt...\n")
set.seed(42)  # For reproducibility
result <- accumulate_evidence_ddm_opt(
  A = A,
  V = V, 
  ndt = ndt,
  dt = dt,
  max_reached = max_reached,
  max_t = max_t,
  noise_mechanism = noise_mechanism,
  noise_func = zero_noise
)

cat("Results:\n")
cat("- Words recalled:", result$words, "\n")
cat("- Actual RT:", result$rts, "\n") 
cat("- Expected RT:", expected_rt, "\n")
cat("- Difference:", abs(result$rts - expected_rt), "\n")
cat("- Relative error:", abs(result$rts - expected_rt) / expected_rt * 100, "%\n")

# Test with different A/V ratios
cat("\n=== Testing multiple A/V ratios ===\n")
test_cases <- data.frame(
  A = c(5, 10, 15, 20),
  V = c(1, 2, 3, 4),
  expected = c(5, 5, 5, 5)  # All should give RT = 5
)

for (i in seq_len(nrow(test_cases))) {
  set.seed(42)
  result <- accumulate_evidence_ddm_opt(
    A = test_cases$A[i],
    V = test_cases$V[i], 
    ndt = 0,
    dt = dt,
    max_reached = 1,
    max_t = max_t,
    noise_mechanism = noise_mechanism,
    noise_func = zero_noise
  )
  
  error <- abs(result$rts - test_cases$expected[i])
  cat("A =", test_cases$A[i], ", V =", test_cases$V[i], 
      "| Expected =", test_cases$expected[i], 
      "| Actual =", round(result$rts, 4), 
      "| Error =", round(error, 4), "\n")
}

# Test with very small dt for higher precision
cat("\n=== Testing with smaller dt ===\n")
dt_small <- 0.001
set.seed(42)
result_precise <- accumulate_evidence_ddm_opt(
  A = 10,
  V = 2, 
  ndt = 0,
  dt = dt_small,
  max_reached = 1,
  max_t = max_t,
  noise_mechanism = noise_mechanism,
  noise_func = zero_noise
)

expected_precise <- 10/2
error_precise <- abs(result_precise$rts - expected_precise)
cat("With dt =", dt_small, ":\n")
cat("- Expected RT:", expected_precise, "\n")
cat("- Actual RT:", result_precise$rts, "\n")
cat("- Error:", error_precise, "\n")
cat("- Relative error:", error_precise / expected_precise * 100, "%\n")

# Test with multiple items - this is critical!
cat("\n=== Testing with MULTIPLE ITEMS ===\n")

# Test 1: Two items with same V, should both reach threshold at same time
cat("Test 1: Two identical items\n")
A_multi <- c(10, 10)
V_multi <- c(2, 2)  
ndt_multi <- c(0, 0)
expected_multi <- c(5, 5)  # Both should be RT = 10/2 = 5

set.seed(42)
result_multi <- accumulate_evidence_ddm_opt(
  A = A_multi,
  V = V_multi, 
  ndt = ndt_multi,
  dt = 0.001,  # Use small dt for precision
  max_reached = 2,
  max_t = max_t,
  noise_mechanism = noise_mechanism,
  noise_func = zero_noise
)

cat("- Words recalled:", result_multi$words, "\n")
cat("- Actual RTs:", result_multi$rts, "\n")
cat("- Expected RTs:", expected_multi, "\n")
cat("- Errors:", abs(result_multi$rts - expected_multi), "\n")

# Test 2: Two items with different V, should reach at different times
cat("\nTest 2: Two items with different drift rates\n")
A_diff <- c(10, 10)
V_diff <- c(2, 1)   # First should be faster
ndt_diff <- c(0, 0)
expected_diff <- c(5, 10)  # RT = 10/2=5, RT = 10/1=10

set.seed(42)
result_diff <- accumulate_evidence_ddm_opt(
  A = A_diff,
  V = V_diff, 
  ndt = ndt_diff,
  dt = 0.001,
  max_reached = 2,
  max_t = max_t,
  noise_mechanism = noise_mechanism,
  noise_func = zero_noise
)

cat("- Words recalled:", result_diff$words, "\n")
cat("- Actual RTs:", result_diff$rts, "\n")
cat("- Expected RTs:", expected_diff, "\n")
cat("- Errors:", abs(result_diff$rts - expected_diff), "\n")

# Test 3: Five items with increasing thresholds
cat("\nTest 3: Five items with increasing thresholds\n")
A_five <- c(5, 10, 15, 20, 25)
V_five <- c(1, 1, 1, 1, 1)   # Same drift rate
ndt_five <- c(0, 0, 0, 0, 0)
expected_five <- c(5, 10, 15, 20, 25)  # RT = A/V

set.seed(42)
result_five <- accumulate_evidence_ddm_opt(
  A = A_five,
  V = V_five, 
  ndt = ndt_five,
  dt = 0.001,
  max_reached = 5,
  max_t = 30,  # Increase max_t for longer RTs
  noise_mechanism = noise_mechanism,
  noise_func = zero_noise
)

cat("- Words recalled:", result_five$words, "\n")
cat("- Actual RTs:", round(result_five$rts, 3), "\n")
cat("- Expected RTs:", expected_five, "\n")
cat("- Errors:", round(abs(result_five$rts - expected_five), 3), "\n")
cat("- Order correct:", all(diff(result_five$rts) > 0), "\n")  # Should be increasing

# Test 4: Check if the right items are being recalled
cat("\nTest 4: Different A and V - checking recall order\n")
A_order <- c(20, 10, 15)  # Different thresholds for first 3 recalls
V_order <- c(4, 2, 3)     # Item 1: RT=5, Item 2: RT=5, Item 3: RT=5
ndt_order <- c(0, 0, 0)
expected_order <- c(5, 5, 5)  # All should reach at same time

set.seed(42)
result_order <- accumulate_evidence_ddm_opt(
  A = A_order,
  V = V_order, 
  ndt = ndt_order,
  dt = 0.001,
  max_reached = 3,
  max_t = max_t,
  noise_mechanism = noise_mechanism,
  noise_func = zero_noise
)

cat("- Words recalled:", result_order$words, "\n")
cat("- Actual RTs:", round(result_order$rts, 3), "\n")
cat("- Expected RTs:", expected_order, "\n")
cat("- All reach at same time:", max(result_order$rts) - min(result_order$rts) < 0.01, "\n")

# Test with basic noise - check if behavior is reasonable
cat("\n=== Testing with BASIC NOISE ===\n")

# Define a simple noise function
simple_noise <- function(n, dt) {
  rnorm(n, mean = 0, sd = sqrt(dt))
}

# Test with noise - should have more variability but reasonable RTs
cat("Test with normal noise (multiple runs to check variability)\n")
A_noise <- c(10)
V_noise <- c(2) 
ndt_noise <- c(0)
expected_noise <- 5  # Still expect around RT = A/V = 5

rts_with_noise <- numeric(10)
for (run in 1:10) {
  set.seed(run * 10)  # Different seeds
  result_noise <- accumulate_evidence_ddm_opt(
    A = A_noise,
    V = V_noise, 
    ndt = ndt_noise,
    dt = 0.01,
    max_reached = 1,
    max_t = max_t,
    noise_mechanism = noise_mechanism,
    noise_func = simple_noise
  )
  rts_with_noise[run] <- result_noise$rts
}

cat("- RTs with noise:", round(rts_with_noise, 3), "\n")
cat("- Mean RT:", round(mean(rts_with_noise), 3), "\n")
cat("- Expected RT:", expected_noise, "\n")
cat("- Standard deviation:", round(sd(rts_with_noise), 3), "\n")
cat("- Range:", round(range(rts_with_noise), 3), "\n")
cat("- All RTs positive:", all(rts_with_noise > 0), "\n")
cat("- Reasonable range (1-15):", all(rts_with_noise > 1 & rts_with_noise < 15), "\n")

# Test multiple items with noise
cat("\nTest: Multiple items with noise\n")
A_multi_noise <- c(8, 12, 10)
V_multi_noise <- c(2, 2, 2)
ndt_multi_noise <- c(0, 0, 0)
expected_multi_noise <- c(4, 6, 5)  # Expected RTs

set.seed(123)
result_multi_noise <- accumulate_evidence_ddm_opt(
  A = A_multi_noise,
  V = V_multi_noise, 
  ndt = ndt_multi_noise,
  dt = 0.01,
  max_reached = 3,
  max_t = max_t,
  noise_mechanism = noise_mechanism,
  noise_func = simple_noise
)

cat("- Words recalled:", result_multi_noise$words, "\n")
cat("- Actual RTs:", round(result_multi_noise$rts, 3), "\n")
cat("- Expected order (item 1, 3, 2):", paste(order(expected_multi_noise)), "\n")
cat("- All items recalled:", length(result_multi_noise$words) == 3, "\n")
cat("- RTs reasonable:", all(result_multi_noise$rts > 1 & result_multi_noise$rts < 15), "\n")

cat("\n=== Test completed ===\n")