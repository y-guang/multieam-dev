# Test script for DDM benchmark - C++ vs R comparison
# Run this after compiling the package

# Load required libraries
if (!require(microbenchmark)) {
  install.packages("microbenchmark")
  library(microbenchmark)
}

# Source the benchmark function
source("R/benchmark.R")

# Run quick test comparing both implementations (10 runs)
cat("Running quick test with C++ vs R comparison (10 iterations)...\n\n")
quick_results <- quick_test_ddm(compare_with_r = TRUE)

# Analyze performance differences
analyze_performance(quick_results)

# Uncomment below for full benchmark (100 runs)
# cat("\n\nRunning full benchmark with 100 iterations...\n\n")
# full_results <- benchmark_ddm(n_runs = 100, compare_with_r = TRUE)
# analyze_performance(full_results)

# Test C++ only for pure performance measurement
cat("\n\n=== C++ ONLY BENCHMARK ===\n")
cpp_only_results <- benchmark_cpp_only(n_runs = 10)

# Plot results if available
if (require(ggplot2, quietly = TRUE)) {
  cat("\nGenerating benchmark plot...\n")
  p <- autoplot(quick_results$benchmark)
  print(p)
  
  # Save plot
  ggsave("benchmark_results.png", p, width = 12, height = 8, dpi = 150)
  cat("Plot saved as 'benchmark_results.png'\n")
}

# Summary of what we're testing
cat("\n=== BENCHMARK SUMMARY ===\n")
cat("This benchmark tests both C++ (accumulate_evidence_ddm_naive) and R (run_steps)\n")
cat("implementations under three computational scenarios:\n\n")
cat("1. Early End (V=1.0): High drift rate - items recalled quickly\n")
cat("   - Expected: Fewest iterations, fastest execution\n\n")
cat("2. Mid Range (V=0.01): Medium drift rate - moderate performance\n") 
cat("   - Expected: Medium iterations, medium execution time\n\n")
cat("3. Max_t Reach (V=1e-8): Very low drift rate - hits time limit\n")
cat("   - Expected: Full simulation time, slowest execution\n\n")
cat("Performance expectations:\n")
cat("- C++ should be significantly faster than R\n")
cat("- Within each implementation: Early < Mid < Max_t (execution time)\n")