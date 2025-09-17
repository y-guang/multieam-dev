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

# Step 5: Run the benchmark
cat("Step 4: Running benchmark...\n\n")

# Quick test with fewer iterations
cat("=== QUICK BENCHMARK (10 runs) ===\n")
results <- benchmark_ddm(n_runs = 10)

cat("\n=== BENCHMARK ANALYSIS ===\n")
cat("The three conditions test different performance scenarios:\n")
cat("1. Early end (V=1.0): High drift rate, items recalled quickly\n")
cat("2. Mid range (V=0.01): Medium drift rate, moderate performance\n") 
cat("3. Max_t reach (V=1e-8): Very low drift, hits time limit\n\n")

cat("Expected performance pattern:\n")
cat("- Early end: Fastest (fewer iterations needed)\n")
cat("- Mid range: Medium speed\n")
cat("- Max_t reach: Slowest (full simulation time)\n\n")

# Show timing comparison
median_times <- tapply(results$benchmark$time, results$benchmark$expr, median)
cat("Median execution times (nanoseconds):\n")
print(median_times)

cat("\nRelative performance (vs fastest):\n")
relative_times <- median_times / min(median_times)
print(round(relative_times, 2))