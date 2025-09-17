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
cat("=== QUICK BENCHMARK (10 runs) - C++ vs R ===\n")
results <- benchmark_ddm(n_runs = 10, compare_with_r = TRUE)

cat("\n=== BENCHMARK ANALYSIS ===\n")
cat("The three conditions test different performance scenarios:\n")
cat("1. Early end (V=1.0): High drift rate, items recalled quickly\n")
cat("2. Mid range (V=0.01): Medium drift rate, moderate performance\n") 
cat("3. Max_t reach (V=1e-8): Very low drift, hits time limit\n\n")

# Expected performance pattern:\n")
cat("- Early end: Fastest (fewer iterations needed)\n")
cat("- Mid range: Medium speed\n")
cat("- Max_t reach: Slowest (full simulation time)\n")
cat("- C++ should outperform R significantly\n\n")

# Detailed performance analysis
analyze_performance(results)

# Step 6: Test custom noise function
cat("\n\n=== CUSTOM NOISE FUNCTION DEMO ===\n")
cat("Testing custom noise function with different noise types...\n\n")

# Example 1: Equivalent to standard noise (should give similar results)
standard_noise <- function(n_items, dt) {
  rnorm(n_items, 0.0, sqrt(dt))
}

# Example 2: Scaled noise (higher variance)
scaled_noise <- function(n_items, dt) {
  rnorm(n_items, 0.0, 2 * sqrt(dt))  # Double the noise
}

# Example 3: Uniform noise
uniform_noise <- function(n_items, dt) {
  scale_factor <- sqrt(dt) * sqrt(3)  # Match variance to normal
  runif(n_items, -scale_factor, scale_factor)
}

# Test with standard noise (should match regular function)
cat("Testing with equivalent standard noise:\n")
A <- rep(10, 5)
V <- rep(1.0, 10)
ndt <- rep(0, 10)

set.seed(123)
result_standard <- accumulate_evidence_ddm_naive(A, V, ndt, 0.01, 5, 20, "add")

set.seed(123)  
result_custom_standard <- accumulate_evidence_ddm_naive_with_custom_noise(A, V, ndt, 0.01, 5, 20, "add", standard_noise)

# Also test with R implementation
set.seed(123)
result_r_custom <- run_steps(A, V, ndt, 0.01, 5, 20, "add", standard_noise)

cat("C++ Standard function - Items recalled:", length(result_standard$words), "\n")
cat("C++ Custom (equivalent) - Items recalled:", length(result_custom_standard$words), "\n")
cat("R Custom - Items recalled:", length(result_r_custom$words), "\n")
cat("C++ vs C++ custom RTs match:", all.equal(result_standard$rts, result_custom_standard$rts), "\n")
cat("C++ vs R custom RTs match:", all.equal(result_standard$rts, result_r_custom$rts), "\n\n")

# Test with scaled noise
cat("Testing with scaled noise (2x variance):\n")
set.seed(123)
result_scaled_cpp <- accumulate_evidence_ddm_naive_with_custom_noise(A, V, ndt, 0.01, 5, 20, "add", scaled_noise)
set.seed(123)
result_scaled_r <- run_steps(A, V, ndt, 0.01, 5, 20, "add", scaled_noise)

cat("C++ Scaled noise - Items recalled:", length(result_scaled_cpp$words), "\n")
cat("R Scaled noise - Items recalled:", length(result_scaled_r$words), "\n")
cat("C++ vs R scaled results match:", all.equal(result_scaled_cpp$rts, result_scaled_r$rts), "\n")
cat("C++ Average RT difference vs standard:", mean(result_scaled_cpp$rts) - mean(result_standard$rts), "\n\n")

# Test with uniform noise  
cat("Testing with uniform noise:\n")
set.seed(123)
result_uniform_cpp <- accumulate_evidence_ddm_naive_with_custom_noise(A, V, ndt, 0.01, 5, 20, "add", uniform_noise)
set.seed(123)
result_uniform_r <- run_steps(A, V, ndt, 0.01, 5, 20, "add", uniform_noise)

cat("C++ Uniform noise - Items recalled:", length(result_uniform_cpp$words), "\n")
cat("R Uniform noise - Items recalled:", length(result_uniform_r$words), "\n")
cat("C++ vs R uniform results match:", all.equal(result_uniform_cpp$rts, result_uniform_r$rts), "\n\n")

cat("\n=== PERFORMANCE COMPARISON WITH CUSTOM NOISE ===\n")
cat("Running full benchmark including C++ vs R with custom noise...\n")
custom_results <- benchmark_all(n_runs = 10, compare_with_r = TRUE)
analyze_performance(custom_results)