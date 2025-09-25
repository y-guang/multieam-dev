library(dplyr)


choose_best_trim <- function(
    flat_df,
    trims = seq(0, 0.2, by = 0.01),
    lambda = 0.02) {
  flat_df %>%
    select(condition_idx, rank_idx, rt) %>%
    # filter out invalid rt
    filter(
      !is.na(rt) & rt > 0 & is.finite(rt)
    ) %>%
    # expand to grid format
    cross_join(
      tibble(trim = trims)
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
    select(condition_idx, rank_idx, rt) %>%
    mutate(
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
      filter(partition_idx == !!partition_idx) %>%
      collect()

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

# Option 1: Original function (for small datasets)
# best_trim <- choose_best_trim(flat_df = tidy_data)

# Option 2: Parallel processing in memory (medium datasets)
# best_trim_by_parallel <- choose_best_trim_parallel(flat_df = tidy_data, chunk_size = 100, n_cores = 6)

# Option 3: Large dataset processing with temporary files (OOM solution)
best_trim_large <- choose_best_trim_large(flat_df = tidy_data)

# Apply the trim using the results
trimmed_data <- apply_trim(flat_df = tidy_data, best_trim = best_trim_large, min_n_used = 80)
