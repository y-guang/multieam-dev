# =============================================================================
# DDM Benchmark Suite
# Comprehensive benchmarking for drift diffusion model implementations
# =============================================================================

library(microbenchmark)

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Setup benchmark parameters and conditions
setup_benchmark_params <- function() {
  list(
    # Fixed parameters
    A_fixed = 10,
    dt = 0.01,
    max_t = 20,
    max_reached = 10,
    n_items = 10,
    noise_mechanism = "add",
    
    # Non-decision times (set to 0 for benchmark simplicity)
    ndt = rep(0, 10),
    
    # Custom noise function for testing
    custom_noise_func = function(n_items, dt) {
      rnorm(n_items, 0.0, sqrt(dt))
    }
  )
}

#' Create test conditions for benchmarking
create_test_conditions <- function(params) {
  list(
    # Condition 1: Early end - high drift rate
    early = list(
      A = rep(params$A_fixed, params$max_reached),
      V = rep(params$A_fixed / 10, params$n_items),  # V = 1.0
      label = "Early end",
      description = "High drift rate, early finish"
    ),
    
    # Condition 2: Mid range - medium drift rate  
    mid = list(
      A = rep(params$A_fixed, params$max_reached),
      V = rep(params$A_fixed / (params$max_t / params$dt / 2), params$n_items),  # V = 0.01
      label = "Mid range", 
      description = "Medium drift rate, moderate performance"
    ),
    
    # Condition 3: Max_t reach - very low drift rate
    max_t = list(
      A = rep(params$A_fixed, params$max_reached),
      V = rep(1e-8, params$n_items),
      label = "Max_t reach",
      description = "Very low drift rate, hits time limit"
    )
  )
}

#' Print benchmark setup information
print_benchmark_info <- function(params, conditions, n_runs, compare_with_r, include_custom_noise, include_optimized) {
  cat("=== DDM BENCHMARK SETUP ===\n")
  cat("Parameters:\n")
  cat("- A (threshold):", params$A_fixed, "\n")
  cat("- dt (time step):", params$dt, "\n") 
  cat("- max_t (max time):", params$max_t, "\n")
  cat("- max_reached:", params$max_reached, "\n")
  cat("- n_items:", params$n_items, "\n")
  cat("- n_runs:", n_runs, "\n")
  cat("- Compare with R:", compare_with_r, "\n")
  cat("- Include custom noise:", include_custom_noise, "\n")
  cat("- Include optimized:", include_optimized, "\n\n")
  
  cat("Test Conditions:\n")
  for (name in names(conditions)) {
    cond <- conditions[[name]]
    cat("- ", cond$label, ": V =", cond$V[1], "(", cond$description, ")\n")
  }
  cat("\n")
}

# Main benchmark function
benchmark_ddm <- function(n_runs = 100, compare_with_r = FALSE, include_custom_noise = FALSE, include_optimized = FALSE) {
  # Setup parameters and conditions
  params <- setup_benchmark_params()
  conditions <- create_test_conditions(params)
  
  # Extract parameters for convenience
  A_fixed <- params$A_fixed
  dt <- params$dt
  max_t <- params$max_t
  max_reached <- params$max_reached
  n_items <- params$n_items
  noise_mechanism <- params$noise_mechanism
  ndt <- params$ndt
  custom_noise_func <- params$custom_noise_func
  
  # Extract condition vectors
  A_early <- conditions$early$A
  V_early <- conditions$early$V
  A_mid <- conditions$mid$A  
  V_mid <- conditions$mid$V
  A_max <- conditions$max_t$A
  V_max <- conditions$max_t$V
  
  # Print setup info
  print_benchmark_info(params, conditions, n_runs, compare_with_r, include_custom_noise, include_optimized)
  
  cat("Test Conditions:\n")
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
  
  # Add custom noise benchmarks if requested
  if (include_custom_noise) {
    custom_noise_exprs <- list(
      cpp_custom_early_end = quote({
        accumulate_evidence_ddm_naive_with_custom_noise(
          A = A_early,
          V = V_early,
          ndt = ndt,
          dt = dt,
          max_reached = max_reached,
          max_t = max_t,
          noise_mechanism = noise_mechanism,
          noise_func = custom_noise_func
        )
      }),
      
      cpp_custom_mid_range = quote({
        accumulate_evidence_ddm_naive_with_custom_noise(
          A = A_mid,
          V = V_mid,
          ndt = ndt,
          dt = dt,
          max_reached = max_reached,
          max_t = max_t,
          noise_mechanism = noise_mechanism,
          noise_func = custom_noise_func
        )
      }),
      
      cpp_custom_max_t_reach = quote({
        accumulate_evidence_ddm_naive_with_custom_noise(
          A = A_max,
          V = V_max,
          ndt = ndt,
          dt = dt,
          max_reached = max_reached,
          max_t = max_t,
          noise_mechanism = noise_mechanism,
          noise_func = custom_noise_func
        )
      })
    )
    benchmark_exprs <- c(benchmark_exprs, custom_noise_exprs)
  }
  
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
    
    # Add R custom noise implementations if custom noise is included
    if (include_custom_noise) {
      r_custom_exprs <- list(
        r_custom_early_end = quote({
          run_steps(
            A = A_early,
            V = V_early,
            ndt = ndt,
            dt = dt,
            max_reached = max_reached,
            max_t = max_t,
            noise_mechanism = noise_mechanism,
            noise_func = custom_noise_func
          )
        }),
        
        r_custom_mid_range = quote({
          run_steps(
            A = A_mid,
            V = V_mid,
            ndt = ndt,
            dt = dt,
            max_reached = max_reached,
            max_t = max_t,
            noise_mechanism = noise_mechanism,
            noise_func = custom_noise_func
          )
        }),
        
        r_custom_max_t_reach = quote({
          run_steps(
            A = A_max,
            V = V_max,
            ndt = ndt,
            dt = dt,
            max_reached = max_reached,
            max_t = max_t,
            noise_mechanism = noise_mechanism,
            noise_func = custom_noise_func
          )
        })
      )
      benchmark_exprs <- c(benchmark_exprs, r_custom_exprs)
    }
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
  
  # Test custom noise function if included
  if (include_custom_noise) {
    cat("\nCustom Noise Function Results:\n")
    set.seed(123)
    result_custom_early <- accumulate_evidence_ddm_naive_with_custom_noise(A_early, V_early, ndt, dt, max_reached, max_t, noise_mechanism, custom_noise_func)
    cat("C++ Custom Early end - Items recalled:", length(result_custom_early$words), ", Max RT:", max(result_custom_early$rts), "\n")
    
    set.seed(123)
    result_custom_mid <- accumulate_evidence_ddm_naive_with_custom_noise(A_mid, V_mid, ndt, dt, max_reached, max_t, noise_mechanism, custom_noise_func)
    cat("C++ Custom Mid range - Items recalled:", length(result_custom_mid$words), ", Max RT:", max(result_custom_mid$rts), "\n")
    
    set.seed(123)
    result_custom_max <- accumulate_evidence_ddm_naive_with_custom_noise(A_max, V_max, ndt, dt, max_reached, max_t, noise_mechanism, custom_noise_func)
    cat("C++ Custom Max_t reach - Items recalled:", length(result_custom_max$words), ", Max RT:", max(result_custom_max$rts), "\n")
    
    test_results <- c(test_results, list(
      cpp_custom_early = result_custom_early,
      cpp_custom_mid = result_custom_mid,
      cpp_custom_max_t = result_custom_max
    ))
  }
  
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
    
    # Test R custom noise function if included
    if (include_custom_noise) {
      cat("\nR Custom Noise Function Results:\n")
      set.seed(123)
      result_r_custom_early <- run_steps(A_early, V_early, ndt, dt, max_reached, max_t, noise_mechanism, custom_noise_func)
      cat("R Custom Early end - Items recalled:", length(result_r_custom_early$words), ", Max RT:", max(result_r_custom_early$rts), "\n")
      
      set.seed(123)
      result_r_custom_mid <- run_steps(A_mid, V_mid, ndt, dt, max_reached, max_t, noise_mechanism, custom_noise_func)
      cat("R Custom Mid range - Items recalled:", length(result_r_custom_mid$words), ", Max RT:", max(result_r_custom_mid$rts), "\n")
      
      set.seed(123)
      result_r_custom_max <- run_steps(A_max, V_max, ndt, dt, max_reached, max_t, noise_mechanism, custom_noise_func)
      cat("R Custom Max_t reach - Items recalled:", length(result_r_custom_max$words), ", Max RT:", max(result_r_custom_max$rts), "\n")
      
      test_results <- c(test_results, list(
        r_custom_early = result_r_custom_early,
        r_custom_mid = result_r_custom_mid,
        r_custom_max_t = result_r_custom_max
      ))
    }
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

# Function to test custom noise function
test_custom_noise <- function(n_runs = 10) {
  cat("Testing custom noise function:\n")
  benchmark_ddm(n_runs = n_runs, compare_with_r = FALSE, include_custom_noise = TRUE)
}

# Function to compare only specific implementations
benchmark_cpp_only <- function(n_runs = 100) {
  cat("C++ only benchmark:\n")
  benchmark_ddm(n_runs = n_runs, compare_with_r = FALSE)
}

# Function to benchmark all implementations including custom noise
benchmark_all <- function(n_runs = 100, compare_with_r = TRUE) {
  cat("Full benchmark including custom noise:\n")
  benchmark_ddm(n_runs = n_runs, compare_with_r = compare_with_r, include_custom_noise = TRUE)
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
  
  # Separate C++, R, and custom noise results
  cpp_results <- median_times[grep("^cpp_(?!custom)", names(median_times), perl = TRUE)]
  r_results <- median_times[grep("^r_(?!custom)", names(median_times), perl = TRUE)]
  cpp_custom_results <- median_times[grep("^cpp_custom_", names(median_times))]
  r_custom_results <- median_times[grep("^r_custom_", names(median_times))]
  
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
  
  # Compare standard C++ vs custom noise C++
  if (length(cpp_results) > 0 && length(cpp_custom_results) > 0) {
    cat("\n=== C++ STANDARD vs CUSTOM NOISE COMPARISON ===\n")
    
    conditions <- c("early_end", "mid_range", "max_t_reach")
    for (cond in conditions) {
      cpp_time <- median_times[paste0("cpp_", cond)]
      cpp_custom_time <- median_times[paste0("cpp_custom_", cond)]
      
      if (!is.na(cpp_time) && !is.na(cpp_custom_time)) {
        overhead <- (cpp_custom_time / cpp_time - 1) * 100
        cat(sprintf("%s: Custom noise has %.1f%% overhead vs standard\n", cond, overhead))
      }
    }
    
    overall_cpp <- mean(cpp_results)
    overall_cpp_custom <- mean(cpp_custom_results)
    overall_overhead <- (overall_cpp_custom / overall_cpp - 1) * 100
    cat(sprintf("\nOverall: Custom noise has %.1f%% overhead vs standard on average\n", overall_overhead))
  }
  
  # Compare C++ vs R custom noise implementations
  if (length(cpp_custom_results) > 0 && length(r_custom_results) > 0) {
    cat("\n=== C++ vs R CUSTOM NOISE COMPARISON ===\n")
    
    conditions <- c("early_end", "mid_range", "max_t_reach")
    for (cond in conditions) {
      cpp_custom_time <- median_times[paste0("cpp_custom_", cond)]
      r_custom_time <- median_times[paste0("r_custom_", cond)]
      
      if (!is.na(cpp_custom_time) && !is.na(r_custom_time)) {
        speedup <- r_custom_time / cpp_custom_time
        cat(sprintf("%s: C++ custom noise is %.1fx faster than R custom noise\n", cond, speedup))
      }
    }
    
    overall_cpp_custom <- mean(cpp_custom_results)
    overall_r_custom <- mean(r_custom_results)
    overall_custom_speedup <- overall_r_custom / overall_cpp_custom
    cat(sprintf("\nOverall: C++ custom noise is %.1fx faster than R custom noise on average\n", overall_custom_speedup))
  }
  
  cat("\n=== CONDITION COMPARISON ===\n")
  if (length(cpp_results) > 0) {
    cat("C++ standard relative performance (vs fastest condition):\n")
    cpp_relative <- cpp_results / min(cpp_results)
    print(round(cpp_relative, 2))
  }
  
  if (length(cpp_custom_results) > 0) {
    cat("C++ custom noise relative performance (vs fastest condition):\n")
    cpp_custom_relative <- cpp_custom_results / min(cpp_custom_results)
    print(round(cpp_custom_relative, 2))
  }
  
  if (length(r_results) > 0) {
    cat("R standard relative performance (vs fastest condition):\n")
    r_relative <- r_results / min(r_results)
    print(round(r_relative, 2))
  }
  
  if (length(r_custom_results) > 0) {
    cat("R custom noise relative performance (vs fastest condition):\n")
    r_custom_relative <- r_custom_results / min(r_custom_results)
    print(round(r_custom_relative, 2))
  }
}