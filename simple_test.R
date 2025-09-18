# Simple test to verify the benchmark system works
cat("=== SIMPLE BENCHMARK TEST ===\n")

# Load required libraries and functions
library(microbenchmark)
library(Rcpp)

# Compile C++ functions
cat("Compiling C++ functions...\n")
sourceCpp("src/core.cpp")

# Load benchmark functions  
cat("Loading benchmark functions...\n")
source("R/benchmark.R")

# Load R implementations
source("R/hello.R")

# Define a simple noise function with correct signature
simple_noise <- function(n, dt = 0.01) {
  rnorm(n, mean = 0, sd = sqrt(dt))
}

cat("Testing individual functions...\n")

# Test C++ standard function
A <- rep(10, 3)
V <- rep(1.0, 5) 
ndt <- rep(0, 5)
dt <- 0.01
max_reached <- 3
max_t <- 10
noise_mechanism <- "add"

cat("C++ Standard: ")
result1 <- accumulate_evidence_ddm_naive(A, V, ndt, dt, max_reached, max_t, noise_mechanism)
cat("OK - Recalled", length(result1$words), "items\n")

cat("C++ Custom Noise: ")
result2 <- accumulate_evidence_ddm_naive_with_custom_noise(A, V, ndt, dt, max_reached, max_t, noise_mechanism, simple_noise)
cat("OK - Recalled", length(result2$words), "items\n")

cat("C++ Optimized: ")
result3 <- accumulate_evidence_ddm_opt(A, V, ndt, dt, max_reached, max_t, noise_mechanism, simple_noise)
cat("OK - Recalled", length(result3$words), "items\n")

cat("R Standard: ")
result4 <- run_steps(A, V, ndt, dt, max_reached, max_t, noise_mechanism)
cat("OK - Recalled", length(result4$words), "items\n")

cat("R Custom: ")
result5 <- run_steps(A, V, ndt, dt, max_reached, max_t, noise_mechanism, simple_noise)
cat("OK - Recalled", length(result5$words), "items\n")

cat("\nTesting quick benchmark functions...\n")

# Test quick benchmarks
cat("C++ Standard Quick: ")
result_cpp_std <- benchmark_cpp_standard_quick(n_items = 20, n_timesteps = 100, times = 3)
cat("OK\n")

cat("C++ Custom Quick: ")
result_cpp_custom <- benchmark_cpp_custom_quick(n_items = 20, n_timesteps = 100, custom_noise_func = simple_noise, times = 3)
cat("OK\n")

cat("C++ Optimized Quick: ")
result_cpp_opt <- benchmark_cpp_opt_quick(n_items = 20, n_timesteps = 100, custom_noise_func = simple_noise, times = 3)
cat("OK\n")

cat("R Standard Quick: ")
result_r_std <- benchmark_r_standard_quick(n_items = 20, n_timesteps = 100, times = 3)
cat("OK\n")

cat("R Custom Quick: ")
result_r_custom <- benchmark_r_custom_quick(n_items = 20, n_timesteps = 100, custom_noise_func = simple_noise, times = 3)
cat("OK\n")

cat("All Quick: ")
result_all <- benchmark_all_quick(n_items = 20, n_timesteps = 100, custom_noise_func = simple_noise, times = 3)
cat("OK\n")

cat("\n=== ALL TESTS PASSED ===\n")
cat("The benchmark system is working correctly!\n")
cat("You can now run:\n")
cat("- test_new_benchmark.R for basic testing\n")  
cat("- run_example.R for comprehensive examples\n")
cat("- Any of the benchmark functions individually\n")