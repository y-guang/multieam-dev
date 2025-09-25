choose_best_trim <- function(
    flat_df,
    trims = seq(0, 0.2, by = 0.01),
    lambda = 0.02) {
  flat_df %>%
    dplyr::select(condition_idx, rank_idx, rt) %>%
    # filter out invalid rt
    dplyr::filter(
      !is.na(rt) & rt > 0 & is.finite(rt)
    ) %>%
    # expand to grid format
    dplyr::cross_join(
      dplyr::tibble(trim = trims)
    ) %>%
    dplyr::group_by(
      condition_idx,
      rank_idx,
      trim
    ) %>%
    dplyr::mutate(
      quantile_upper = quantile(rt, probs = 1 - trim, na.rm = TRUE)
    ) %>%
    dplyr::filter(
      rt < quantile_upper
    ) %>%
    dplyr::filter(
      dplyr::n() >= 2
    ) %>%
    dplyr::mutate(
      log_rt = log(rt),
    ) %>%
    dplyr::summarise(
      mu = mean(log_rt, na.rm = TRUE),
      sigma = sd(log_rt, na.rm = TRUE),
      ks_statistic = suppressWarnings(
        stats::ks.test(
          rt, "plnorm",
          meanlog = mu, sdlog = sigma
        )$statistic
      ),
      n_used = dplyr::n(),
      q_lower = 0,
      q_upper = dplyr::first(quantile_upper),
      .groups = "drop_last" # note this
    ) %>%
    dplyr::mutate(
      score = ks_statistic + lambda * trim,
    ) %>%
    dplyr::rename(
      mu_hat = mu,
      sigma_hat = sigma,
    ) %>%
    dplyr::arrange(score, trim) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
}

choose_best_trim_large <- function(
    flat_df,
    trims = seq(0, 0.2, by = 0.01),
    lambda = 0.02,
    chunk_size = NULL,
    n_cores = NULL) {
  # Load required libraries
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package 'parallel' is required for parallel processing")
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required for dataset partitioning")
  }

  # Set default values
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
  }

  # Heuristic for chunk size
  if (is.null(chunk_size)) {
    unique_conditions <- unique(flat_df$condition_idx)
    n_unique <- length(unique_conditions)
    n_partitions <- ceiling(sqrt(n_unique))
    n_partitions <- max(n_cores, min(n_partitions, n_cores * 10))
    chunk_size <- ceiling(n_unique / n_partitions)
  }

  # Set up temporary folder for intermediate files
  temp_dir <- file.path(tempdir(), "choose_best_trim_large")
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
  dir.create(temp_dir, recursive = TRUE)

  # Create partition indices and write partitioned dataset
  flat_df <- flat_df %>%
    # Optimize storage
    dplyr::select(condition_idx, rank_idx, rt) %>%
    dplyr::mutate(
      partition_idx = (as.integer(factor(condition_idx)) - 1) %/% chunk_size
    )
  partition_indices <- unique(flat_df$partition_idx)

  # Store temporary data and clean up memory
  flat_df %>%
    arrow::write_dataset(
      temp_dir,
      format = "parquet",
      partitioning = "partition_idx"
    )
  remove(flat_df)
  gc()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Function to process each partition
  process_partition <- function(partition_idx) {
    # Load corresponding partition
    arrow::set_cpu_count(1)
    worker_dataset <- arrow::open_dataset(temp_dir, format = "parquet")

    # Filter dataset for this partition and collect to memory
    partition_data <- worker_dataset %>%
      dplyr::filter(partition_idx == !!partition_idx) %>%
      dplyr::collect()

    # Process this partition with the original function
    return(choose_best_trim(partition_data, trims = trims, lambda = lambda))
  }

  # Setup parallel cluster
  cl <- parallel::makeCluster(min(n_cores, length(partition_indices)))
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Export required objects and functions to cluster
  parallel::clusterExport(cl,
    c("choose_best_trim", "trims", "lambda", "temp_dir", "process_partition"),
    envir = environment()
  )
  parallel::clusterEvalQ(cl, {
    library(dplyr)
    library(arrow)
  })

  # Run parallel processing with progress bar
  if (requireNamespace("pbapply", quietly = TRUE)) {
    partition_results <- pbapply::pblapply(
      partition_indices,
      process_partition,
      cl = cl
    )
  } else {
    message("Install 'pbapply' package for progress bar support")
    partition_results <- parallel::parLapply(
      cl,
      partition_indices,
      process_partition
    )
  }

  # Combine results
  result <- do.call(rbind, partition_results)

  return(result)
}

apply_trim <- function(flat_df, best_trim, min_n_used = 0) {
  best_trim %>%
    dplyr::filter(
      n_used >= min_n_used
    ) %>%
    dplyr::select(
      condition_idx,
      rank_idx,
      trim,
      q_lower,
      q_upper
    ) %>%
    dplyr::inner_join(
      flat_df,
      by = c("condition_idx", "rank_idx")
    ) %>%
    dplyr::filter(
      rt >= q_lower & rt <= q_upper
    )
}

tidy_data <- flat_result

# Option 1: Original function (for small datasets)
# best_trim <- choose_best_trim(flat_df = tidy_data)

# Option 3: Large dataset processing with temporary files
best_trim_large <- choose_best_trim_large(flat_df = tidy_data)

# Apply the trim using the results
trimmed_data <- apply_trim(flat_df = tidy_data, best_trim = best_trim_large, min_n_used = 80)
