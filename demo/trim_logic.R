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
      qutile_upper = quantile(rt, probs = 1 - trim, na.rm = TRUE)
    ) %>%
    dplyr::filter(
      rt < qutile_upper
    ) %>%
    dplyr::filter(
      dplyr::n() >= 2
    ) %>%
    dplyr::mutate(
      rt_log = log(rt),
    ) %>%
    dplyr::summarise(
      mu = mean(rt_log, na.rm = TRUE),
      sigma = sd(rt_log, na.rm = TRUE),
      ks_D = suppressWarnings(
        stats::ks.test(
          rt, "plnorm",
          meanlog = mu, sdlog = sigma
        )$statistic
      ),
      n_used = dplyr::n(),
      q_lower = 0,
      q_upper = dplyr::first(qutile_upper),
      .groups = "drop_last" # note this
    ) %>%
    dplyr::mutate(
      score = ks_D + lambda * trim,
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

  # heuristic for chunk size
  if (is.null(chunk_size)) {
    unique_conditions <- unique(flat_df$condition_idx)
    n_unique <- length(unique_conditions)
    n_partitions <- ceiling(sqrt(n_unique))
    n_partitions <- max(n_cores, min(n_partitions, n_cores * 10))
    chunk_size <- ceiling(n_unique / n_partitions)
  }

  # Set up temporary folder for intermediate files
  tmp_folder <- file.path(tempdir(), "choose_best_trim_large")
  if (dir.exists(tmp_folder)) {
    unlink(tmp_folder, recursive = TRUE)
  }
  dir.create(tmp_folder, recursive = TRUE)

  # Create partition indices and write partitioned dataset
  flat_df <- flat_df %>%
    # optimise storage
    dplyr::select(condition_idx, rank_idx, rt) %>%
    dplyr::mutate(
      partition_idx = (as.integer(factor(condition_idx)) - 1) %/% chunk_size
    )
  partition_indices <- unique(flat_df$partition_idx)

  # store temporary data and clean up memory
  flat_df %>%
    arrow::write_dataset(
      tmp_folder,
      format = "parquet",
      partitioning = "partition_idx"
    )
  remove(flat_df)
  gc()
  on.exit(unlink(tmp_folder, recursive = TRUE), add = TRUE)

  # Function to process each partition
  process_partition <- function(partition_idx) {
    # load corresponding partition
    arrow::set_cpu_count(1)
    worker_dataset <- arrow::open_dataset(tmp_folder, format = "parquet")

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
    c("choose_best_trim", "trims", "lambda", "tmp_folder", "process_partition"),
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
