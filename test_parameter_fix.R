# Quick test to verify noise function parameter order fix
cat("=== NOISE FUNCTION PARAMETER ORDER TEST ===\n")

# Load functions
library(Rcpp)
sourceCpp("src/core.cpp")
source("R/hello.R")

# Test parameters
A <- rep(10, 3)
V <- rep(1.0, 5) 
ndt <- rep(0, 5)
dt <- 0.01
max_reached <- 3
max_t <- 5
noise_mechanism <- "add"

# Define noise function with correct signature (n, dt)
test_noise <- function(n, dt) {
  cat("Noise function called with n =", n, ", dt =", dt, "\n")
  rnorm(n, mean = 0, sd = sqrt(dt))
}

cat("Testing C++ custom noise function...\n")
tryCatch({
  result1 <- accumulate_evidence_ddm_naive_with_custom_noise(A, V, ndt, dt, max_reached, max_t, noise_mechanism, test_noise)
  cat("✅ C++ custom noise: SUCCESS - Recalled", length(result1$words), "items\n")
}, error = function(e) {
  cat("❌ C++ custom noise: ERROR -", e$message, "\n")
})

cat("\nTesting C++ optimized function...\n")
tryCatch({
  result2 <- accumulate_evidence_ddm_opt(A, V, ndt, dt, max_reached, max_t, noise_mechanism, test_noise)
  cat("✅ C++ optimized: SUCCESS - Recalled", length(result2$words), "items\n")
}, error = function(e) {
  cat("❌ C++ optimized: ERROR -", e$message, "\n")
})

cat("\nTesting R implementation with custom noise...\n")
tryCatch({
  result3 <- run_steps(A, V, ndt, dt, max_reached, max_t, noise_mechanism, test_noise)
  cat("✅ R custom noise: SUCCESS - Recalled", length(result3$words), "items\n")
}, error = function(e) {
  cat("❌ R custom noise: ERROR -", e$message, "\n")
})

cat("\nTesting R implementation with default noise...\n")
tryCatch({
  result4 <- run_steps(A, V, ndt, dt, max_reached, max_t, noise_mechanism)
  cat("✅ R default noise: SUCCESS - Recalled", length(result4$words), "items\n")
}, error = function(e) {
  cat("❌ R default noise: ERROR -", e$message, "\n")
})

cat("\n=== PARAMETER ORDER FIX TEST COMPLETED ===\n")