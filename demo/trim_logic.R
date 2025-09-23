library(dplyr)
library(tidyr)


choose_best_trim <- function(
    flat_df,
    trims = seq(0, 0.2, by = 0.01),
    lambda = 0.02) {
  flat_df %>%
    # filter out invalid rt
    filter(
      !is.na(rt) & rt > 0 & is.finite(rt)
    ) %>%
    # expand to grid format
    expand_grid(
      trim = trims
    ) %>%
    group_by(
      condition_idx,
      rank_idx,
      trim
    ) %>%
    mutate(
      qutile_upper = quantile(rt, probs = 1 - trim, na.rm = TRUE)
    ) %>%
    filter(
      rt < qutile_upper
    ) %>%
    filter(
      n() >= 2
    ) %>%
    mutate(
      rt_log = log(rt),
    ) %>%
    summarise(
      mu = mean(rt_log, na.rm = TRUE),
      sigma = sd(rt_log, na.rm = TRUE),
      ks_D = suppressWarnings(
        ks.test(
          rt, "plnorm",
          meanlog = mu, sdlog = sigma
        )$statistic
      ),
      n_used = n(),
      q_lower = 0,
      q_upper = first(qutile_upper),
      .groups = "drop_last" # note this
    ) %>%
    mutate(
      score = ks_D + lambda * trim,
    ) %>%
    rename(
      mu_hat = mu,
      sigma_hat = sigma,
    ) %>%
    arrange(score, trim) %>%
    slice(1) %>%
    ungroup()
}

choose_best_trim_parallel <- function(
    flat_df,
    trims = seq(0, 0.2, by = 0.01),
    lambda = 0.02,
    chunk_size = NULL,
    n_cores = NULL) {
  # Load required libraries
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package 'parallel' is required for parallel processing")
  }

  # Set default values
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
  }

  # Get unique condition indices
  unique_conditions <- unique(flat_df$condition_idx)

  # Set chunk size if not provided
  if (is.null(chunk_size)) {
    chunk_size <- ceiling(length(unique_conditions) / n_cores)
  }

  # Split conditions into chunks (ensuring condition_idx is not split)
  condition_chunks <- split(unique_conditions, ceiling(seq_along(unique_conditions) / chunk_size))

  # Split the actual data in main session to avoid OOM
  data_chunks <- lapply(condition_chunks, function(condition_indices) {
    flat_df[flat_df$condition_idx %in% condition_indices, ]
  })

  # Function to process each chunk (now receives actual data, not indices)
  process_chunk <- function(chunk_data) {
    return(choose_best_trim(chunk_data, trims = trims, lambda = lambda))
  }

  # Setup parallel cluster
  cl <- parallel::makeCluster(min(n_cores, length(data_chunks)))
  on.exit(parallel::stopCluster(cl))

  # Export env
  parallel::clusterExport(cl, c("choose_best_trim", "trims", "lambda"), envir = environment())
  parallel::clusterEvalQ(cl, {
    library(dplyr)
    library(tidyr)
  })

  # Run parallel processing with progress bar
  if (requireNamespace("pbapply", quietly = TRUE)) {
    chunk_results <- pbapply::pblapply(
      data_chunks,
      process_chunk,
      cl = cl
    )
  } else {
    message("Install 'pbapply' package for progress bar support")
    chunk_results <- parallel::parLapply(cl, data_chunks, process_chunk)
  }

  # Combine results
  result <- do.call(rbind, chunk_results)

  return(result)
}

apply_trim <- function(flat_df, best_trim, min_n_used = 0) {
  best_trim %>%
    filter(
      n_used >= min_n_used
    ) %>%
    select(
      condition_idx,
      rank_idx,
      trim,
      q_lower,
      q_upper
    ) %>%
    inner_join(
      flat_df,
      by = c("condition_idx", "rank_idx")
    ) %>%
    filter(
      rt >= q_lower & rt <= q_upper
    )
}

tidy_data <- flat_result
# best_trim <- choose_best_trim(flat_df = tidy_data)
best_trim_by_parallel <- choose_best_trim_parallel(flat_df = tidy_data, chunk_size = 100)
trimmed_data <- apply_trim(flat_df = tidy_data, best_trim = best_trim_by_parallel, min_n_used = 80)
