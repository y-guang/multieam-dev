# Example: How to compile and run the DDM benchmark
#
# This script demonstrates the complete workflow for testing the
# accumulate_evidence_ddm_naive function performance

# Step 1: Compile the Rcpp functions
cat("Step 1: Compiling Rcpp functions...\n")
Rcpp::compileAttributes(".")

# Step 2: Build and install the package (optional, for development)
# devtools::document()
# devtools::install()

# Step 3: Load the compiled functions
cat("Step 2: Loading compiled functions...\n")
Rcpp::sourceCpp("src/core.cpp")

# Step 4: Source benchmark functions
cat("Step 3: Loading benchmark functions...\n")
source("R/benchmark.R")

# Step 5: Run the benchmark comparing C++ vs R
cat("Step 4: Running benchmark with C++ vs R comparison...\n\n")

# Quick test with fewer iterations
cat("=== QUICK BENCHMARK (10 runs) - All Implementations ===\n")

# Define a simple noise function for testing
simple_noise <- function(n, dt = 0.01) {
  rnorm(n, mean = 0, sd = sqrt(dt))
}

# Run comprehensive benchmark
results <- benchmark_all_comprehensive(
  n_items = 20,
  n_timesteps = 300,
  custom_noise_func = simple_noise,
  times = 60
)

cat("\n=== BENCHMARK ANALYSIS ===\n")
cat("Testing all five implementations:\n")
cat("1. C++ Standard: Basic optimized C++ implementation\n")
cat("2. C++ Custom Noise: C++ with user-defined noise function\n")
cat("3. C++ Optimized: Advanced C++ with dynamic item removal\n")
cat("4. R Standard: Reference R implementation\n")
cat("5. R Custom Noise: R implementation with custom noise\n\n")

cat("Expected performance pattern:\n")
cat("- C++ Optimized: Fastest (dynamic optimization)\n")
cat("- C++ Standard: Fast (basic optimization)\n")
cat("- C++ Custom: Medium (function call overhead)\n")
cat("- R implementations: Slower (interpreted)\n\n")

# Print detailed results
print(results)

# Step 6: Test individual functions
cat("\n\n=== INDIVIDUAL FUNCTION TESTING ===\n")
cat("Testing each implementation individually...\n\n")

# Test parameters
A <- rep(10, 5)
V <- rep(1.0, 10)
ndt <- rep(0, 10)
dt <- 0.01
max_reached <- 5
max_t <- 20
noise_mechanism <- "add"

# Example 1: Standard C++ function
cat("Testing C++ Standard function:\n")
set.seed(123)
result_standard <- accumulate_evidence_ddm_naive(A, V, ndt, dt, max_reached, max_t, noise_mechanism)
cat("Items recalled:", length(result_standard$words), "RTs:", round(result_standard$rts[1:3], 3), "\n\n")

# Example 2: C++ with custom noise (equivalent to standard)
standard_noise <- function(n, dt) {
  rnorm(n, 0.0, sqrt(dt))
}

cat("Testing C++ Custom Noise (equivalent to standard):\n")
set.seed(123)
result_custom_standard <- accumulate_evidence_ddm_naive_with_custom_noise(A, V, ndt, dt, max_reached, max_t, noise_mechanism, standard_noise)
cat("Items recalled:", length(result_custom_standard$words), "RTs:", round(result_custom_standard$rts[1:3], 3), "\n")
cat("RTs match standard:", all.equal(result_standard$rts, result_custom_standard$rts), "\n\n")

# Example 3: C++ Optimized function
cat("Testing C++ Optimized function:\n")
set.seed(123)
result_opt <- accumulate_evidence_ddm_opt(A, V, ndt, dt, max_reached, max_t, noise_mechanism, standard_noise)
cat("Items recalled:", length(result_opt$words), "RTs:", round(result_opt$rts[1:3], 3), "\n\n")

# Example 4: R implementation with custom noise
cat("Testing R implementation:\n")
set.seed(123)
result_r_custom <- run_steps(A, V, ndt, dt, max_reached, max_t, noise_mechanism, standard_noise)
cat("Items recalled:", length(result_r_custom$words), "RTs:", round(result_r_custom$rts[1:3], 3), "\n")
cat("R vs C++ match:", all.equal(result_standard$rts, result_r_custom$rts), "\n\n")

# Step 7: Test different noise types
cat("=== DIFFERENT NOISE TYPES DEMO ===\n")

# Example 2: Scaled noise (higher variance)
scaled_noise <- function(n, dt) {
  rnorm(n, 0.0, 2 * sqrt(dt))  # Double the noise
}

# Example 3: Uniform noise
uniform_noise <- function(n, dt) {
  scale_factor <- sqrt(dt) * sqrt(3)  # Match variance to normal
  runif(n, -scale_factor, scale_factor)
}

# Test with scaled noise
cat("\nTesting with scaled noise (2x variance):\n")
set.seed(123)
result_scaled_cpp <- accumulate_evidence_ddm_naive_with_custom_noise(A, V, ndt, dt, max_reached, max_t, noise_mechanism, scaled_noise)
set.seed(123)
result_scaled_r <- run_steps(A, V, ndt, dt, max_reached, max_t, noise_mechanism, scaled_noise)

cat("C++ Scaled noise - Items recalled:", length(result_scaled_cpp$words), "\n")
cat("R Scaled noise - Items recalled:", length(result_scaled_r$words), "\n")
cat("C++ vs R scaled results match:", all.equal(result_scaled_cpp$rts, result_scaled_r$rts), "\n")
cat("C++ Average RT difference vs standard:", round(mean(result_scaled_cpp$rts) - mean(result_standard$rts), 3), "\n\n")

# Test with uniform noise
cat("Testing with uniform noise:\n")
set.seed(123)
result_uniform_cpp <- accumulate_evidence_ddm_naive_with_custom_noise(A, V, ndt, dt, max_reached, max_t, noise_mechanism, uniform_noise)
set.seed(123)
result_uniform_r <- run_steps(A, V, ndt, dt, max_reached, max_t, noise_mechanism, uniform_noise)

cat("C++ Uniform noise - Items recalled:", length(result_uniform_cpp$words), "\n")
cat("R Uniform noise - Items recalled:", length(result_uniform_r$words), "\n")
cat("C++ vs R uniform results match:", all.equal(result_uniform_cpp$rts, result_uniform_r$rts), "\n\n")

cat("\n=== PERFORMANCE COMPARISON WITH ALL IMPLEMENTATIONS ===\n")
cat("Running comprehensive benchmark with all implementations...\n")
final_results <- benchmark_performance_comparison(
  n_items = 10,
  n_timesteps = 3000,
  custom_noise_func = simple_noise,
  times = 5
)
print(final_results)

cat("\n=== EXAMPLE COMPLETED SUCCESSFULLY ===\n")
cat("All implementations tested and benchmarked!\n")
cat("- C++ Standard: Basic fast implementation\n")
cat("- C++ Custom Noise: Flexible noise with some overhead\n")
cat("- C++ Optimized: Fastest with dynamic item removal\n")
cat("- R implementations: Slower but useful for prototyping\n")
