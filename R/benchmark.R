# Benchmark function for accumulate_evidence_ddm_naive vs run_steps
# Tests three conditions: early end, mid range, and max_t reach

benchmark_ddm <- function(n_runs = 100, compare_with_r = TRUE) {
  library(microbenchmark)
  
  # Source the R implementation
  if (compare_with_r) {
    source("R/hello.R")
  }
  
  # Fixed parameters
  A_fixed <- 10
  dt <- 0.01
  max_t <- 20
  max_reached <- 10
  n_items <- 10
  noise_mechanism <- "add"
  
  # Non-decision times (set to 0 for benchmark simplicity)
  ndt <- rep(0, n_items)
  
  # Condition 1: Early end - V = A / 10 = 1.0 (high drift, early finish)
  A_early <- rep(A_fixed, max_reached)
  V_early <- rep(A_fixed / 10, n_items)  # V = 1.0
  
  # Condition 2: Mid range - V = max_A / (max_t / dt / 2) = 10 / (20/0.01/2) = 10/1000 = 0.01
  A_mid <- rep(A_fixed, max_reached)
  V_mid <- rep(A_fixed / (max_t / dt / 2), n_items)  # V = 0.01
  
  # Condition 3: Max_t reach - V = 1e-8 (very low drift, hits time limit)
  A_max <- rep(A_fixed, max_reached)
  V_max <- rep(1e-8, n_items)
  
  cat("Benchmark Parameters:\n")
  cat("- A (threshold):", A_fixed, "\n")
  cat("- dt (time step):", dt, "\n")
  cat("- max_t (max time):", max_t, "\n")
  cat("- max_reached:", max_reached, "\n")
  cat("- n_items:", n_items, "\n")
  cat("- n_runs:", n_runs, "\n")
  cat("- Compare with R:", compare_with_r, "\n\n")
  
  cat("Conditions:\n")
  cat("1. Early end: V =", V_early[1], "(high drift)\n")
  cat("2. Mid range: V =", V_mid[1], "(medium drift)\n")
  cat("3. Max_t reach: V =", V_max[1], "(very low drift)\n\n")
  
  # Create benchmark expressions list
  benchmark_exprs <- list(
    cpp_early_end = quote({
      accumulate_evidence_ddm_naive(
        A = A_early,
        V = V_early,
        ndt = ndt,
        dt = dt,
        max_reached = max_reached,
        max_t = max_t,
        noise_mechanism = noise_mechanism
      )
    }),
    
    cpp_mid_range = quote({
      accumulate_evidence_ddm_naive(
        A = A_mid,
        V = V_mid,
        ndt = ndt,
        dt = dt,
        max_reached = max_reached,
        max_t = max_t,
        noise_mechanism = noise_mechanism
      )
    }),
    
    cpp_max_t_reach = quote({
      accumulate_evidence_ddm_naive(
        A = A_max,
        V = V_max,
        ndt = ndt,
        dt = dt,
        max_reached = max_reached,
        max_t = max_t,
        noise_mechanism = noise_mechanism
      )
    })
  )
  
  # Add R implementations if requested
  if (compare_with_r) {
    r_exprs <- list(
      r_early_end = quote({
        run_steps(
          A = A_early,
          V = V_early,
          ndt = ndt,
          dt = dt,
          max_reached = max_reached,
          max_t = max_t,
          noise_mechanism = noise_mechanism
        )
      }),
      
      r_mid_range = quote({
        run_steps(
          A = A_mid,
          V = V_mid,
          ndt = ndt,
          dt = dt,
          max_reached = max_reached,
          max_t = max_t,
          noise_mechanism = noise_mechanism
        )
      }),
      
      r_max_t_reach = quote({
        run_steps(
          A = A_max,
          V = V_max,
          ndt = ndt,
          dt = dt,
          max_reached = max_reached,
          max_t = max_t,
          noise_mechanism = noise_mechanism
        )
      })
    )
    benchmark_exprs <- c(benchmark_exprs, r_exprs)
  }
  
  # Run benchmark
  benchmark_results <- do.call(microbenchmark, c(benchmark_exprs, list(times = n_runs, unit = "ms")))
  
  # Print results
  print(benchmark_results)
  
  # Additional analysis
  cat("\nBenchmark Summary:\n")
  summary_stats <- summary(benchmark_results)
  print(summary_stats)
  
  # Test each condition once to verify behavior
  cat("\nTesting each condition (single run):\n")
  
  set.seed(123)
  result_cpp_early <- accumulate_evidence_ddm_naive(A_early, V_early, ndt, dt, max_reached, max_t, noise_mechanism)
  cat("C++ Early end - Items recalled:", length(result_cpp_early$words), ", Max RT:", max(result_cpp_early$rts), "\n")
  
  set.seed(123)
  result_cpp_mid <- accumulate_evidence_ddm_naive(A_mid, V_mid, ndt, dt, max_reached, max_t, noise_mechanism)
  cat("C++ Mid range - Items recalled:", length(result_cpp_mid$words), ", Max RT:", max(result_cpp_mid$rts), "\n")
  
  set.seed(123)
  result_cpp_max <- accumulate_evidence_ddm_naive(A_max, V_max, ndt, dt, max_reached, max_t, noise_mechanism)
  cat("C++ Max_t reach - Items recalled:", length(result_cpp_max$words), ", Max RT:", max(result_cpp_max$rts), "\n")
  
  test_results <- list(
    cpp_early = result_cpp_early,
    cpp_mid = result_cpp_mid,
    cpp_max_t = result_cpp_max
  )
  
  if (compare_with_r) {
    cat("\nR Implementation Results:\n")
    set.seed(123)
    result_r_early <- run_steps(A_early, V_early, ndt, dt, max_reached, max_t, noise_mechanism)
    cat("R Early end - Items recalled:", length(result_r_early$words), ", Max RT:", max(result_r_early$rts), "\n")
    
    set.seed(123)
    result_r_mid <- run_steps(A_mid, V_mid, ndt, dt, max_reached, max_t, noise_mechanism)
    cat("R Mid range - Items recalled:", length(result_r_mid$words), ", Max RT:", max(result_r_mid$rts), "\n")
    
    set.seed(123)
    result_r_max <- run_steps(A_max, V_max, ndt, dt, max_reached, max_t, noise_mechanism)
    cat("R Max_t reach - Items recalled:", length(result_r_max$words), ", Max RT:", max(result_r_max$rts), "\n")
    
    test_results <- c(test_results, list(
      r_early = result_r_early,
      r_mid = result_r_mid,
      r_max_t = result_r_max
    ))
  }
  
  return(list(
    benchmark = benchmark_results,
    summary = summary_stats,
    test_results = test_results
  ))
}

# Function to run a quick test
quick_test_ddm <- function(compare_with_r = TRUE) {
  cat("Quick test of DDM conditions:\n")
  benchmark_ddm(n_runs = 10, compare_with_r = compare_with_r)
}

# Function to compare only specific implementations
benchmark_cpp_only <- function(n_runs = 100) {
  cat("C++ only benchmark:\n")
  benchmark_ddm(n_runs = n_runs, compare_with_r = FALSE)
}

# Function to analyze performance differences
analyze_performance <- function(benchmark_result) {
  if (!"benchmark" %in% names(benchmark_result)) {
    stop("Invalid benchmark result")
  }
  
  results <- benchmark_result$benchmark
  
  # Extract median times by implementation and condition
  median_times <- tapply(results$time, results$expr, median)
  
  cat("\n=== PERFORMANCE ANALYSIS ===\n")
  cat("Median execution times (nanoseconds):\n")
  print(median_times)
  
  # Separate C++ and R results if both exist
  cpp_results <- median_times[grep("^cpp_", names(median_times))]
  r_results <- median_times[grep("^r_", names(median_times))]
  
  if (length(cpp_results) > 0 && length(r_results) > 0) {
    cat("\n=== C++ vs R COMPARISON ===\n")
    
    # Match conditions
    conditions <- c("early_end", "mid_range", "max_t_reach")
    for (cond in conditions) {
      cpp_time <- median_times[paste0("cpp_", cond)]
      r_time <- median_times[paste0("r_", cond)]
      
      if (!is.na(cpp_time) && !is.na(r_time)) {
        speedup <- r_time / cpp_time
        cat(sprintf("%s: C++ is %.1fx faster than R\n", cond, speedup))
      }
    }
    
    overall_cpp <- mean(cpp_results)
    overall_r <- mean(r_results)
    overall_speedup <- overall_r / overall_cpp
    cat(sprintf("\nOverall: C++ is %.1fx faster than R on average\n", overall_speedup))
  }
  
  cat("\n=== CONDITION COMPARISON ===\n")
  if (length(cpp_results) > 0) {
    cat("C++ relative performance (vs fastest condition):\n")
    cpp_relative <- cpp_results / min(cpp_results)
    print(round(cpp_relative, 2))
  }
  
  if (length(r_results) > 0) {
    cat("R relative performance (vs fastest condition):\n")
    r_relative <- r_results / min(r_results)
    print(round(r_relative, 2))
  }
}