# Comprehensive test to verify all noise function parameter fixes
cat("=== COMPREHENSIVE NOISE FUNCTION PARAMETER TEST ===\n")

# Load required libraries and functions
library(microbenchmark)
library(Rcpp)

# Compile C++ functions
cat("Compiling C++ functions...\n")
sourceCpp("src/core.cpp")

# Load R functions
source("R/benchmark.R")
source("R/hello.R")

# Test parameters
A <- rep(10, 3)
V <- rep(1.0, 5) 
ndt <- rep(0, 5)
dt <- 0.01
max_reached <- 3
max_t <- 10
noise_mechanism <- "add"

cat("=== TESTING ALL NOISE FUNCTION SIGNATURES ===\n")

# Test 1: Simple noise function (2 parameters)
simple_noise <- function(n, dt = 0.01) {
  rnorm(n, mean = 0, sd = sqrt(dt))
}

cat("1. Testing simple_noise function:\n")
tryCatch({
  result1 <- accumulate_evidence_ddm_naive_with_custom_noise(A, V, ndt, dt, max_reached, max_t, noise_mechanism, simple_noise)
  cat("   ✅ SUCCESS - Recalled", length(result1$words), "items\n")
}, error = function(e) {
  cat("   ❌ ERROR:", e$message, "\n")
})

# Test 2: Standard noise function (2 parameters)
standard_noise <- function(n, dt) {
  rnorm(n, 0.0, sqrt(dt))
}

cat("2. Testing standard_noise function:\n")
tryCatch({
  result2 <- accumulate_evidence_ddm_opt(A, V, ndt, dt, max_reached, max_t, noise_mechanism, standard_noise)
  cat("   ✅ SUCCESS - Recalled", length(result2$words), "items\n")
}, error = function(e) {
  cat("   ❌ ERROR:", e$message, "\n")
})

# Test 3: Scaled noise function (2 parameters)
scaled_noise <- function(n, dt) {
  rnorm(n, 0.0, 2 * sqrt(dt))
}

cat("3. Testing scaled_noise function:\n")
tryCatch({
  result3 <- accumulate_evidence_ddm_naive_with_custom_noise(A, V, ndt, dt, max_reached, max_t, noise_mechanism, scaled_noise)
  cat("   ✅ SUCCESS - Recalled", length(result3$words), "items\n")
}, error = function(e) {
  cat("   ❌ ERROR:", e$message, "\n")
})

# Test 4: Uniform noise function (2 parameters)
uniform_noise <- function(n, dt) {
  scale_factor <- sqrt(dt) * sqrt(3)
  runif(n, -scale_factor, scale_factor)
}

cat("4. Testing uniform_noise function:\n")
tryCatch({
  result4 <- accumulate_evidence_ddm_opt(A, V, ndt, dt, max_reached, max_t, noise_mechanism, uniform_noise)
  cat("   ✅ SUCCESS - Recalled", length(result4$words), "items\n")
}, error = function(e) {
  cat("   ❌ ERROR:", e$message, "\n")
})

# Test 5: R implementation with custom noise
cat("5. Testing R implementation with custom noise:\n")
tryCatch({
  result5 <- run_steps(A, V, ndt, dt, max_reached, max_t, noise_mechanism, standard_noise)
  cat("   ✅ SUCCESS - Recalled", length(result5$words), "items\n")
}, error = function(e) {
  cat("   ❌ ERROR:", e$message, "\n")
})

# Test 6: R implementation with default noise  
cat("6. Testing R implementation with default noise:\n")
tryCatch({
  result6 <- run_steps(A, V, ndt, dt, max_reached, max_t, noise_mechanism)
  cat("   ✅ SUCCESS - Recalled", length(result6$words), "items\n")
}, error = function(e) {
  cat("   ❌ ERROR:", e$message, "\n")
})

# Test 7: Benchmark function calls
cat("7. Testing benchmark functions:\n")
tryCatch({
  result7 <- benchmark_cpp_custom_quick(n_items = 20, n_timesteps = 100, custom_noise_func = simple_noise, times = 2)
  cat("   ✅ benchmark_cpp_custom_quick: SUCCESS\n")
}, error = function(e) {
  cat("   ❌ benchmark_cpp_custom_quick ERROR:", e$message, "\n")
})

tryCatch({
  result8 <- benchmark_cpp_opt_quick(n_items = 20, n_timesteps = 100, custom_noise_func = simple_noise, times = 2)
  cat("   ✅ benchmark_cpp_opt_quick: SUCCESS\n")
}, error = function(e) {
  cat("   ❌ benchmark_cpp_opt_quick ERROR:", e$message, "\n")
})

cat("\n=== NOISE FUNCTION PARAMETER TESTS COMPLETED ===\n")
cat("If all tests show SUCCESS, the parameter order issues are fixed!\n")