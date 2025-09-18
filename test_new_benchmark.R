# Test the new refactored benchmark system
library(microbenchmark)
library(Rcpp)

# Source the functions
sourceCpp("src/core.cpp")
source("R/hello.R")
source("R/benchmark.R")

# Test basic functionality
cat("Testing basic benchmark functionality...\n")

# Create a simple custom noise function
simple_noise <- function(n, dt = 0.01) {
  rnorm(n, mean = 0, sd = sqrt(dt))
}

# Test quick benchmark with all implementations
cat("\nRunning quick benchmark with all implementations:\n")
result <- benchmark_all_quick(n_items = 50, n_timesteps = 200, custom_noise_func = simple_noise)
print(result)

# Test individual implementations
cat("\nTesting individual implementations:\n")

# Test C++ standard
cat("C++ Standard: ")
result1 <- benchmark_cpp_standard_quick(n_items = 30, n_timesteps = 100)
cat("OK\n")

# Test C++ custom noise
cat("C++ Custom Noise: ")
result2 <- benchmark_cpp_custom_quick(n_items = 30, n_timesteps = 100, custom_noise_func = simple_noise)
cat("OK\n")

# Test C++ optimized
cat("C++ Optimized: ")
result3 <- benchmark_cpp_opt_quick(n_items = 30, n_timesteps = 100, custom_noise_func = simple_noise)
cat("OK\n")

# Test R implementations
cat("R Standard: ")
result4 <- benchmark_r_standard_quick(n_items = 30, n_timesteps = 100)
cat("OK\n")

cat("R Custom Noise: ")
result5 <- benchmark_r_custom_quick(n_items = 30, n_timesteps = 100, custom_noise_func = simple_noise)
cat("OK\n")

cat("\nAll benchmark functions working correctly!\n")

# Test performance comparison
cat("\nRunning performance comparison...\n")
perf_result <- benchmark_performance_comparison(
  n_items = 100,
  n_timesteps = 300,
  custom_noise_func = simple_noise,
  times = 10
)
print(perf_result)

cat("\nBenchmark system test completed successfully!\n")