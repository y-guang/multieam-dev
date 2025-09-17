# Test script for DDM benchmark
# Run this after compiling the package

# Load required libraries
if (!require(microbenchmark)) {
  install.packages("microbenchmark")
  library(microbenchmark)
}

# Source the benchmark function
source("R/benchmark.R")

# Run quick test (10 runs)
cat("Running quick test with 10 iterations...\n\n")
quick_results <- quick_test_ddm()

# Uncomment below for full benchmark (100 runs)
# cat("\n\nRunning full benchmark with 100 iterations...\n\n")
# full_results <- benchmark_ddm(n_runs = 100)

# Plot results if available
if (require(ggplot2, quietly = TRUE)) {
  cat("\nGenerating benchmark plot...\n")
  p <- autoplot(quick_results$benchmark)
  print(p)
  
  # Save plot
  ggsave("benchmark_results.png", p, width = 10, height = 6, dpi = 150)
  cat("Plot saved as 'benchmark_results.png'\n")
}