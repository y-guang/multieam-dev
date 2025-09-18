# =============================================================================
# DDM Benchmark Suite
# Comprehensive benchmarking for drift diffusion model implementations
# =============================================================================

library(microbenchmark)

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Setup benchmark parameters and conditions
setup_benchmark_params <- function(n_items = 10) {
  list(
    # Fixed parameters
    A_fixed = 10,
    dt = 0.01,
    max_t = 30,
    max_reached = 10,
    n_items = n_items,
    noise_mechanism = "add",

    # Non-decision times (set to 0 for benchmark simplicity)
    ndt = rep(0, n_items),

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
      V = rep(params$A_fixed / (params$max_t / params$dt / 200), params$n_items),  # V = 0.267 (reasonable mid-range)
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

# -----------------------------------------------------------------------------
# Benchmark Expression Builders
# -----------------------------------------------------------------------------

#' Build C++ standard benchmark expressions
build_cpp_standard_exprs <- function(conditions, params) {
  exprs <- list()

  for (cond_name in names(conditions)) {
    cond <- conditions[[cond_name]]
    expr_name <- paste0("cpp_", cond_name)

    exprs[[expr_name]] <- substitute({
      accumulate_evidence_ddm_naive(
        A = A_VAL,
        V = V_VAL,
        ndt = ndt_VAL,
        dt = dt_VAL,
        max_reached = max_reached_VAL,
        max_t = max_t_VAL,
        noise_mechanism = noise_mechanism_VAL
      )
    }, list(
      A_VAL = cond$A,
      V_VAL = cond$V,
      ndt_VAL = params$ndt,
      dt_VAL = params$dt,
      max_reached_VAL = params$max_reached,
      max_t_VAL = params$max_t,
      noise_mechanism_VAL = params$noise_mechanism
    ))
  }

  return(exprs)
}

#' Build C++ custom noise benchmark expressions
build_cpp_custom_exprs <- function(conditions, params) {
  exprs <- list()

  for (cond_name in names(conditions)) {
    cond <- conditions[[cond_name]]
    expr_name <- paste0("cpp_custom_", cond_name)

    exprs[[expr_name]] <- substitute({
      accumulate_evidence_ddm_naive_with_custom_noise(
        A = A_VAL,
        V = V_VAL,
        ndt = ndt_VAL,
        dt = dt_VAL,
        max_reached = max_reached_VAL,
        max_t = max_t_VAL,
        noise_mechanism = noise_mechanism_VAL,
        noise_func = custom_noise_func_VAL
      )
    }, list(
      A_VAL = cond$A,
      V_VAL = cond$V,
      ndt_VAL = params$ndt,
      dt_VAL = params$dt,
      max_reached_VAL = params$max_reached,
      max_t_VAL = params$max_t,
      noise_mechanism_VAL = params$noise_mechanism,
      custom_noise_func_VAL = params$custom_noise_func
    ))
  }

  return(exprs)
}

#' Build C++ optimized benchmark expressions
build_cpp_opt_exprs <- function(conditions, params) {
  exprs <- list()

  for (cond_name in names(conditions)) {
    cond <- conditions[[cond_name]]
    expr_name <- paste0("cpp_opt_", cond_name)

    exprs[[expr_name]] <- substitute({
      accumulate_evidence_ddm_opt(
        A = A_VAL,
        V = V_VAL,
        ndt = ndt_VAL,
        dt = dt_VAL,
        max_reached = max_reached_VAL,
        max_t = max_t_VAL,
        noise_mechanism = noise_mechanism_VAL,
        noise_func = custom_noise_func_VAL
      )
    }, list(
      A_VAL = cond$A,
      V_VAL = cond$V,
      ndt_VAL = params$ndt,
      dt_VAL = params$dt,
      max_reached_VAL = params$max_reached,
      max_t_VAL = params$max_t,
      noise_mechanism_VAL = params$noise_mechanism,
      custom_noise_func_VAL = params$custom_noise_func
    ))
  }

  return(exprs)
}

#' Build R standard benchmark expressions
build_r_standard_exprs <- function(conditions, params) {
  exprs <- list()

  for (cond_name in names(conditions)) {
    cond <- conditions[[cond_name]]
    expr_name <- paste0("r_", cond_name)

    exprs[[expr_name]] <- substitute({
      run_steps(
        A = A_VAL,
        V = V_VAL,
        ndt = ndt_VAL,
        dt = dt_VAL,
        max_reached = max_reached_VAL,
        max_t = max_t_VAL,
        noise_mechanism = noise_mechanism_VAL
      )
    }, list(
      A_VAL = cond$A,
      V_VAL = cond$V,
      ndt_VAL = params$ndt,
      dt_VAL = params$dt,
      max_reached_VAL = params$max_reached,
      max_t_VAL = params$max_t,
      noise_mechanism_VAL = params$noise_mechanism
    ))
  }

  return(exprs)
}

#' Build R custom noise benchmark expressions
build_r_custom_exprs <- function(conditions, params) {
  exprs <- list()

  for (cond_name in names(conditions)) {
    cond <- conditions[[cond_name]]
    expr_name <- paste0("r_custom_", cond_name)

    exprs[[expr_name]] <- substitute({
      run_steps(
        A = A_VAL,
        V = V_VAL,
        ndt = ndt_VAL,
        dt = dt_VAL,
        max_reached = max_reached_VAL,
        max_t = max_t_VAL,
        noise_mechanism = noise_mechanism_VAL,
        noise_func = custom_noise_func_VAL
      )
    }, list(
      A_VAL = cond$A,
      V_VAL = cond$V,
      ndt_VAL = params$ndt,
      dt_VAL = params$dt,
      max_reached_VAL = params$max_reached,
      max_t_VAL = params$max_t,
      noise_mechanism_VAL = params$noise_mechanism,
      custom_noise_func_VAL = params$custom_noise_func
    ))
  }

  return(exprs)
}

# -----------------------------------------------------------------------------
# Testing Functions
# -----------------------------------------------------------------------------

#' Run single test of each condition to verify behavior
run_verification_tests <- function(conditions, params, compare_with_r, include_custom_noise, include_optimized) {
  cat("=== VERIFICATION TESTS ===\n")

  test_results <- list()

  # Test C++ standard implementation
  cat("C++ Standard Implementation:\n")
  for (cond_name in names(conditions)) {
    cond <- conditions[[cond_name]]
    set.seed(123)
    result <- accumulate_evidence_ddm_naive(cond$A, cond$V, params$ndt, params$dt,
                                           params$max_reached, params$max_t, params$noise_mechanism)
    cat("- ", cond$label, ": Items recalled =", length(result$words),
        ", Max RT =", ifelse(length(result$rts) > 0, max(result$rts), 0), "\n")
    test_results[[paste0("cpp_", cond_name)]] <- result
  }

  # Test C++ custom noise implementation
  if (include_custom_noise) {
    cat("\nC++ Custom Noise Implementation:\n")
    for (cond_name in names(conditions)) {
      cond <- conditions[[cond_name]]
      set.seed(123)
      result <- accumulate_evidence_ddm_naive_with_custom_noise(
        cond$A, cond$V, params$ndt, params$dt, params$max_reached, params$max_t,
        params$noise_mechanism, params$custom_noise_func)
      cat("- ", cond$label, ": Items recalled =", length(result$words),
          ", Max RT =", ifelse(length(result$rts) > 0, max(result$rts), 0), "\n")
      test_results[[paste0("cpp_custom_", cond_name)]] <- result
    }
  }

  # Test C++ optimized implementation
  if (include_optimized) {
    cat("\nC++ Optimized Implementation:\n")
    for (cond_name in names(conditions)) {
      cond <- conditions[[cond_name]]
      set.seed(123)
      result <- accumulate_evidence_ddm_opt(
        cond$A, cond$V, params$ndt, params$dt, params$max_reached, params$max_t,
        params$noise_mechanism, params$custom_noise_func)
      cat("- ", cond$label, ": Items recalled =", length(result$words),
          ", Max RT =", ifelse(length(result$rts) > 0, max(result$rts), 0), "\n")
      test_results[[paste0("cpp_opt_", cond_name)]] <- result
    }
  }

  # Test R implementations
  if (compare_with_r) {
    cat("\nR Standard Implementation:\n")
    for (cond_name in names(conditions)) {
      cond <- conditions[[cond_name]]
      set.seed(123)
      result <- run_steps(cond$A, cond$V, params$ndt, params$dt,
                         params$max_reached, params$max_t, params$noise_mechanism)
      cat("- ", cond$label, ": Items recalled =", length(result$words),
          ", Max RT =", ifelse(length(result$rts) > 0, max(result$rts), 0), "\n")
      test_results[[paste0("r_", cond_name)]] <- result
    }

    if (include_custom_noise) {
      cat("\nR Custom Noise Implementation:\n")
      for (cond_name in names(conditions)) {
        cond <- conditions[[cond_name]]
        set.seed(123)
        result <- run_steps(cond$A, cond$V, params$ndt, params$dt, params$max_reached,
                           params$max_t, params$noise_mechanism, params$custom_noise_func)
        cat("- ", cond$label, ": Items recalled =", length(result$words),
            ", Max RT =", ifelse(length(result$rts) > 0, max(result$rts), 0), "\n")
        test_results[[paste0("r_custom_", cond_name)]] <- result
      }
    }
  }

  cat("\n")
  return(test_results)
}

# -----------------------------------------------------------------------------
# Main Benchmark Function
# -----------------------------------------------------------------------------

#' Main benchmark function with all options
benchmark_ddm <- function(n_runs = 100, compare_with_r = TRUE, include_custom_noise = FALSE, include_optimized = FALSE) {

  # Source R implementation if needed
  if (compare_with_r) {
    source("R/hello.R")
  }

  # Setup parameters and conditions
  params <- setup_benchmark_params()
  conditions <- create_test_conditions(params)

  # Print setup information
  print_benchmark_info(params, conditions, n_runs, compare_with_r, include_custom_noise, include_optimized)

  # Build benchmark expressions
  benchmark_exprs <- list()

  # Always include C++ standard
  benchmark_exprs <- c(benchmark_exprs, build_cpp_standard_exprs(conditions, params))

  # Add C++ custom noise if requested
  if (include_custom_noise) {
    benchmark_exprs <- c(benchmark_exprs, build_cpp_custom_exprs(conditions, params))
  }

  # Add C++ optimized if requested
  if (include_optimized) {
    benchmark_exprs <- c(benchmark_exprs, build_cpp_opt_exprs(conditions, params))
  }

  # Add R implementations if requested
  if (compare_with_r) {
    benchmark_exprs <- c(benchmark_exprs, build_r_standard_exprs(conditions, params))

    if (include_custom_noise) {
      benchmark_exprs <- c(benchmark_exprs, build_r_custom_exprs(conditions, params))
    }
  }

  # Run verification tests
  test_results <- run_verification_tests(conditions, params, compare_with_r, include_custom_noise, include_optimized)

  # Run benchmark
  cat("=== RUNNING BENCHMARK ===\n")
  benchmark_results <- do.call(microbenchmark, c(benchmark_exprs, list(times = n_runs, unit = "ms")))

  # Print results
  print(benchmark_results)

  # Additional analysis
  cat("\n=== BENCHMARK SUMMARY ===\n")
  summary_stats <- summary(benchmark_results)
  print(summary_stats)

  return(list(
    benchmark = benchmark_results,
    summary = summary_stats,
    test_results = test_results,
    params = params,
    conditions = conditions
  ))
}

# -----------------------------------------------------------------------------
# Performance Analysis Functions
# -----------------------------------------------------------------------------

#' Analyze performance differences between implementations
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

  # Separate different implementation types
  cpp_results <- median_times[grep("^cpp_(?!custom|opt)", names(median_times), perl = TRUE)]
  r_results <- median_times[grep("^r_(?!custom)", names(median_times), perl = TRUE)]
  cpp_custom_results <- median_times[grep("^cpp_custom_", names(median_times))]
  cpp_opt_results <- median_times[grep("^cpp_opt_", names(median_times))]
  r_custom_results <- median_times[grep("^r_custom_", names(median_times))]

  # C++ vs R comparison
  if (length(cpp_results) > 0 && length(r_results) > 0) {
    cat("\n=== C++ vs R COMPARISON ===\n")

    conditions <- c("early", "mid", "max_t")
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

  # Custom noise overhead analysis
  if (length(cpp_results) > 0 && length(cpp_custom_results) > 0) {
    cat("\n=== C++ STANDARD vs CUSTOM NOISE COMPARISON ===\n")

    conditions <- c("early", "mid", "max_t")
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

  # Optimized vs standard comparison
  if (length(cpp_results) > 0 && length(cpp_opt_results) > 0) {
    cat("\n=== C++ STANDARD vs OPTIMIZED COMPARISON ===\n")

    conditions <- c("early", "mid", "max_t")
    for (cond in conditions) {
      cpp_time <- median_times[paste0("cpp_", cond)]
      cpp_opt_time <- median_times[paste0("cpp_opt_", cond)]

      if (!is.na(cpp_time) && !is.na(cpp_opt_time)) {
        speedup <- cpp_time / cpp_opt_time
        cat(sprintf("%s: Optimized is %.1fx faster than standard\n", cond, speedup))
      }
    }

    overall_cpp <- mean(cpp_results)
    overall_cpp_opt <- mean(cpp_opt_results)
    overall_speedup <- overall_cpp / overall_cpp_opt
    cat(sprintf("\nOverall: Optimized is %.1fx faster than standard on average\n", overall_speedup))
  }

  # Cross-implementation custom noise comparison
  if (length(cpp_custom_results) > 0 && length(r_custom_results) > 0) {
    cat("\n=== C++ vs R CUSTOM NOISE COMPARISON ===\n")

    conditions <- c("early", "mid", "max_t")
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

  # Relative performance within implementation types
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

  if (length(cpp_opt_results) > 0) {
    cat("C++ optimized relative performance (vs fastest condition):\n")
    cpp_opt_relative <- cpp_opt_results / min(cpp_opt_results)
    print(round(cpp_opt_relative, 2))
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

# -----------------------------------------------------------------------------
# Convenience Functions
# -----------------------------------------------------------------------------

#' Quick test with few iterations
quick_test_ddm <- function(compare_with_r = TRUE) {
  cat("=== QUICK TEST ===\n")
  benchmark_ddm(n_runs = 10, compare_with_r = compare_with_r)
}

#' Test custom noise functionality
test_custom_noise <- function(n_runs = 10) {
  cat("=== CUSTOM NOISE TEST ===\n")
  benchmark_ddm(n_runs = n_runs, compare_with_r = FALSE, include_custom_noise = TRUE)
}

#' Test optimized implementation
test_optimized <- function(n_runs = 10) {
  cat("=== OPTIMIZED IMPLEMENTATION TEST ===\n")
  benchmark_ddm(n_runs = n_runs, compare_with_r = FALSE, include_optimized = TRUE)
}

#' C++ only benchmark
benchmark_cpp_only <- function(n_runs = 100) {
  cat("=== C++ ONLY BENCHMARK ===\n")
  benchmark_ddm(n_runs = n_runs, compare_with_r = FALSE)
}

#' Full benchmark with all implementations
benchmark_all <- function(n_runs = 100, compare_with_r = TRUE) {
  cat("=== FULL BENCHMARK ===\n")
  benchmark_ddm(n_runs = n_runs, compare_with_r = compare_with_r, include_custom_noise = TRUE, include_optimized = TRUE)
}

# -----------------------------------------------------------------------------
# Quick Benchmark Functions
# -----------------------------------------------------------------------------

#' Quick benchmark for C++ standard implementation
benchmark_cpp_standard_quick <- function(n_items = 100, n_timesteps = 300, times = 5) {
  params <- setup_benchmark_params(n_items)
  # Use the default max_t from setup_benchmark_params instead of overriding it
  # params$max_t <- n_timesteps * params$dt  # This was causing max_t = 3

  conditions <- create_test_conditions(params)
  exprs <- build_cpp_standard_exprs(conditions, params)

  microbenchmark(list = exprs, times = times, unit = "ms")
}

#' Quick benchmark for C++ custom noise implementation
benchmark_cpp_custom_quick <- function(n_items = 100, n_timesteps = 300, custom_noise_func, times = 5) {
  params <- setup_benchmark_params(n_items)
  # Use the default max_t from setup_benchmark_params instead of overriding it
  # params$max_t <- n_timesteps * params$dt  # This was causing max_t = 3
  params$custom_noise_func <- custom_noise_func

  conditions <- create_test_conditions(params)
  exprs <- build_cpp_custom_exprs(conditions, params)

  microbenchmark(list = exprs, times = times, unit = "ms")
}

#' Quick benchmark for C++ optimized implementation
benchmark_cpp_opt_quick <- function(n_items = 100, n_timesteps = 300, custom_noise_func, times = 5) {
  params <- setup_benchmark_params(n_items)
  # Use the default max_t from setup_benchmark_params instead of overriding it
  # params$max_t <- n_timesteps * params$dt  # This was causing max_t = 3
  params$custom_noise_func <- custom_noise_func

  conditions <- create_test_conditions(params)
  exprs <- build_cpp_opt_exprs(conditions, params)

  microbenchmark(list = exprs, times = times, unit = "ms")
}

#' Quick benchmark for R standard implementation
benchmark_r_standard_quick <- function(n_items = 100, n_timesteps = 300, times = 5) {
  params <- setup_benchmark_params(n_items)
  # Use the default max_t from setup_benchmark_params instead of overriding it
  # params$max_t <- n_timesteps * params$dt  # This was causing max_t = 3

  conditions <- create_test_conditions(params)
  exprs <- build_r_standard_exprs(conditions, params)

  microbenchmark(list = exprs, times = times, unit = "ms")
}

#' Quick benchmark for R custom noise implementation
benchmark_r_custom_quick <- function(n_items = 100, n_timesteps = 300, custom_noise_func, times = 5) {
  params <- setup_benchmark_params(n_items)
  # Use the default max_t from setup_benchmark_params instead of overriding it
  # params$max_t <- n_timesteps * params$dt  # This was causing max_t = 3
  params$custom_noise_func <- custom_noise_func

  conditions <- create_test_conditions(params)
  exprs <- build_r_custom_exprs(conditions, params)

  microbenchmark(list = exprs, times = times, unit = "ms")
}

#' Quick benchmark testing all implementations
benchmark_all_quick <- function(n_items = 100, n_timesteps = 300, custom_noise_func, times = 5) {
  cat("=== QUICK BENCHMARK - ALL IMPLEMENTATIONS ===\n")

  params <- setup_benchmark_params(n_items)
  # Use the default max_t from setup_benchmark_params instead of overriding it
  # params$max_t <- n_timesteps * params$dt  # This was causing max_t = 3
  params$custom_noise_func <- custom_noise_func

  conditions <- create_test_conditions(params)

  # Build all expressions
  exprs <- list()
  exprs <- c(exprs, build_cpp_standard_exprs(conditions, params))
  exprs <- c(exprs, build_cpp_custom_exprs(conditions, params))
  exprs <- c(exprs, build_cpp_opt_exprs(conditions, params))
  exprs <- c(exprs, build_r_standard_exprs(conditions, params))
  exprs <- c(exprs, build_r_custom_exprs(conditions, params))

  microbenchmark(list = exprs, times = times, unit = "ms")
}

#' Comprehensive benchmark with all implementations and analysis
benchmark_all_comprehensive <- function(n_items = 100, n_timesteps = 300, custom_noise_func, times = 20) {
  cat("=== COMPREHENSIVE BENCHMARK - ALL IMPLEMENTATIONS ===\n")

  params <- setup_benchmark_params(n_items)
  # Use the default max_t from setup_benchmark_params instead of overriding it
  # params$max_t <- n_timesteps * params$dt  # This was causing max_t = 3
  params$custom_noise_func <- custom_noise_func

  conditions <- create_test_conditions(params)
  print_benchmark_info(params, conditions, times, TRUE, TRUE, TRUE)

  # Build all expressions
  exprs <- list()
  exprs <- c(exprs, build_cpp_standard_exprs(conditions, params))
  exprs <- c(exprs, build_cpp_custom_exprs(conditions, params))
  exprs <- c(exprs, build_cpp_opt_exprs(conditions, params))
  exprs <- c(exprs, build_r_standard_exprs(conditions, params))
  exprs <- c(exprs, build_r_custom_exprs(conditions, params))

  # Run benchmark
  result <- microbenchmark(list = exprs, times = times, unit = "ms")
  print(result)

  # Run verification tests
  cat("\n=== VERIFICATION TESTS ===\n")
  run_verification_tests(conditions, params, TRUE, TRUE, TRUE)

  # Analyze performance
  cat("\n=== PERFORMANCE ANALYSIS ===\n")
  analyze_performance(list(benchmark = result))

  return(result)
}

#' Performance comparison between implementations
benchmark_performance_comparison <- function(n_items = 100, n_timesteps = 300, custom_noise_func, times = 10) {
  cat("=== PERFORMANCE COMPARISON ===\n")

  params <- setup_benchmark_params(n_items)
  # Use the default max_t from setup_benchmark_params instead of overriding it
  # params$max_t <- n_timesteps * params$dt  # This was causing max_t = 3
  params$custom_noise_func <- custom_noise_func

  conditions <- create_test_conditions(params)

  # Test each implementation separately for clearer comparison
  cat("Running individual benchmarks...\n")

  cpp_std <- benchmark_cpp_standard_quick(n_items, n_timesteps, times)
  cpp_custom <- benchmark_cpp_custom_quick(n_items, n_timesteps, custom_noise_func, times)
  cpp_opt <- benchmark_cpp_opt_quick(n_items, n_timesteps, custom_noise_func, times)
  r_std <- benchmark_r_standard_quick(n_items, n_timesteps, times)
  r_custom <- benchmark_r_custom_quick(n_items, n_timesteps, custom_noise_func, times)

  # Combine results
  all_results <- rbind(
    transform(cpp_std, implementation = "C++ Standard"),
    transform(cpp_custom, implementation = "C++ Custom"),
    transform(cpp_opt, implementation = "C++ Optimized"),
    transform(r_std, implementation = "R Standard"),
    transform(r_custom, implementation = "R Custom")
  )

  return(all_results)
}
