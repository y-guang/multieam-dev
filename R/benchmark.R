# Benchmark function for accumulate_evidence_ddm_naive
# Tests three conditions: early end, mid range, and max_t reach

benchmark_ddm <- function(n_runs = 100) {
  library(microbenchmark)
  
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
  cat("- n_runs:", n_runs, "\n\n")
  
  cat("Conditions:\n")
  cat("1. Early end: V =", V_early[1], "(high drift)\n")
  cat("2. Mid range: V =", V_mid[1], "(medium drift)\n")
  cat("3. Max_t reach: V =", V_max[1], "(very low drift)\n\n")
  
  # Benchmark function
  benchmark_results <- microbenchmark(
    early_end = {
      accumulate_evidence_ddm_naive(
        A = A_early,
        V = V_early,
        ndt = ndt,
        dt = dt,
        max_reached = max_reached,
        max_t = max_t,
        noise_mechanism = noise_mechanism
      )
    },
    
    mid_range = {
      accumulate_evidence_ddm_naive(
        A = A_mid,
        V = V_mid,
        ndt = ndt,
        dt = dt,
        max_reached = max_reached,
        max_t = max_t,
        noise_mechanism = noise_mechanism
      )
    },
    
    max_t_reach = {
      accumulate_evidence_ddm_naive(
        A = A_max,
        V = V_max,
        ndt = ndt,
        dt = dt,
        max_reached = max_reached,
        max_t = max_t,
        noise_mechanism = noise_mechanism
      )
    },
    
    times = n_runs,
    unit = "ms"
  )
  
  # Print results
  print(benchmark_results)
  
  # Additional analysis
  cat("\nBenchmark Summary:\n")
  summary_stats <- summary(benchmark_results)
  print(summary_stats)
  
  # Test each condition once to verify behavior
  cat("\nTesting each condition (single run):\n")
  
  set.seed(123)
  result_early <- accumulate_evidence_ddm_naive(A_early, V_early, ndt, dt, max_reached, max_t, noise_mechanism)
  cat("Early end - Items recalled:", length(result_early$words), ", Max RT:", max(result_early$rts), "\n")
  
  set.seed(123)
  result_mid <- accumulate_evidence_ddm_naive(A_mid, V_mid, ndt, dt, max_reached, max_t, noise_mechanism)
  cat("Mid range - Items recalled:", length(result_mid$words), ", Max RT:", max(result_mid$rts), "\n")
  
  set.seed(123)
  result_max <- accumulate_evidence_ddm_naive(A_max, V_max, ndt, dt, max_reached, max_t, noise_mechanism)
  cat("Max_t reach - Items recalled:", length(result_max$words), ", Max RT:", max(result_max$rts), "\n")
  
  return(list(
    benchmark = benchmark_results,
    summary = summary_stats,
    test_results = list(
      early = result_early,
      mid = result_mid,
      max_t = result_max
    )
  ))
}

# Function to run a quick test
quick_test_ddm <- function() {
  cat("Quick test of DDM conditions:\n")
  benchmark_ddm(n_runs = 10)
}