find_lognormal_trim_threshold_serial <- function(
    flat_df,
    trims = seq(0, 0.2, by = 0.01),
    lambda = 0.02) {
  flat_df |>
    dplyr::select(condition_idx, rank_idx, rt) |>
    # filter out invalid rt
    dplyr::filter(
      !is.na(rt) & rt > 0 & is.finite(rt)
    ) |>
    # expand to grid format
    dplyr::cross_join(
      dplyr::tibble(trim = trims)
    ) |>
    dplyr::group_by(
      condition_idx,
      rank_idx,
      trim
    ) |>
    dplyr::mutate(
      quantile_upper = quantile(rt, probs = 1 - trim, na.rm = TRUE)
    ) |>
    dplyr::filter(
      rt < quantile_upper
    ) |>
    dplyr::filter(
      dplyr::n() >= 2
    ) |>
    dplyr::mutate(
      log_rt = log(rt),
    ) |>
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
    ) |>
    dplyr::mutate(
      score = ks_statistic + lambda * trim,
    ) |>
    dplyr::rename(
      mu_hat = mu,
      sigma_hat = sigma,
    ) |>
    dplyr::arrange(score, trim) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
}

find_lognormal_trim_threshold_parallel <- function(
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
  temp_dir <- file.path(tempdir(), "find_lognormal_trim_threshold_parallel")
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
  dir.create(temp_dir, recursive = TRUE)

  # Create partition indices and write partitioned dataset
  flat_df <- flat_df |>
    # Optimize storage
    dplyr::select(condition_idx, rank_idx, rt) |>
    dplyr::mutate(
      partition_idx = (as.integer(factor(condition_idx)) - 1) %/% chunk_size
    )
  partition_indices <- unique(flat_df$partition_idx)

  # Store temporary data and clean up memory
  flat_df |>
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
    partition_data <- worker_dataset |>
      dplyr::filter(partition_idx == !!partition_idx) |>
      dplyr::collect()

    # Process this partition with the original function
    return(find_lognormal_trim_threshold_serial(
      partition_data,
      trims = trims, lambda = lambda
    ))
  }

  # Setup parallel cluster
  cl <- parallel::makeCluster(min(n_cores, length(partition_indices)))
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Export required objects and functions to cluster
  parallel::clusterExport(cl,
    c(
      "find_lognormal_trim_threshold_serial",
      "trims", "lambda", "temp_dir", "process_partition"
    ),
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

#' Find Optimal Lognormal Trim Threshold
#'
#' Determines the optimal trimming threshold for response time data by fitting
#' lognormal distributions and minimizing a penalized Kolmogorov-Smirnov statistic.
#' The function automatically chooses between serial and parallel processing based
#' on data size or user specification.
#'
#' @param flat_df A data frame containing flattened simulation results with columns:
#'   \code{condition_idx}, \code{rank_idx}, and \code{rt} (response time).
#' @param trims A numeric vector of trim values to test. Default is
#'   \code{seq(0, 0.2, by = 0.01)} (0% to 20% trimming in 1% increments).
#' @param lambda A numeric penalty parameter for the optimization score. Higher values
#'   penalize larger trim values more heavily. Default is 0.02.
#' @param parallel Logical or NULL. Controls processing method:
#'   \itemize{
#'     \item \code{NULL} (default): Auto-decide based on data size (>50,000 rows)
#'     \item \code{TRUE}: Force parallel processing
#'     \item \code{FALSE}: Force serial processing
#'   }
#' @param chunk_size Integer. Number of conditions per partition for parallel processing.
#'   If NULL, determined automatically. Only used when \code{parallel = TRUE}.
#' @param n_cores Integer. Number of CPU cores to use for parallel processing.
#'   If NULL, uses \code{detectCores() - 1}. Only used when \code{parallel = TRUE}.
#'
#' @return A data frame with optimal trimming parameters for each condition-rank
#'   combination, containing columns.
#' @seealso \code{\link{apply_trim}} for applying the computed thresholds
#' @export
find_lognormal_trim_threshold <- function(
    flat_df,
    trims = seq(0, 0.2, by = 0.01),
    lambda = 0.02,
    parallel = NULL,
    chunk_size = NULL,
    n_cores = NULL) {
  
  # Determine whether to use parallel processing
  if (is.null(parallel)) {
    # Auto-decide based on data size
    parallel <- nrow(flat_df) > 50000
  }
  
  # Check for unused parallel parameters when not using parallel processing
  if (!parallel && (!is.null(chunk_size) || !is.null(n_cores))) {
    warning("chunk_size and n_cores are ignored when parallel = FALSE")
  }

  if (parallel) {
    # Use parallel processing
    return(find_lognormal_trim_threshold_parallel(
      flat_df = flat_df,
      trims = trims,
      lambda = lambda,
      chunk_size = chunk_size,
      n_cores = n_cores
    ))
  } else {
    # Use serial processing
    return(find_lognormal_trim_threshold_serial(
      flat_df = flat_df,
      trims = trims,
      lambda = lambda
    ))
  }
}

#' Apply Trimming Thresholds to Response Time Data
#'
#' Applies pre-computed trimming thresholds to response time data, filtering out
#' observations that fall outside the specified quantile bounds for each
#' condition-rank combination.
#'
#' @param flat_df A data frame containing the original response time data with columns:
#'   \code{condition_idx}, \code{rank_idx}, and \code{rt} (response time).
#' @param threshold_df A data frame containing trimming thresholds, typically output
#'   from \code{\link{find_lognormal_trim_threshold}}. Must contain columns:
#'   \code{condition_idx}, \code{rank_idx}, \code{trim}, \code{q_lower},
#'   \code{q_upper}, and \code{n_used}.
#' @param min_n_used Integer. Minimum number of observations required for a
#'   condition-rank combination to be included in the output. Combinations with
#'   fewer observations are excluded entirely. Default is 0.
#'
#' @return A data frame containing the trimmed response time data.
#' @export
apply_trim <- function(flat_df, threshold_df, min_n_used = 0) {
  threshold_df |>
    dplyr::filter(
      n_used >= min_n_used
    ) |>
    dplyr::select(
      condition_idx,
      rank_idx,
      trim,
      q_lower,
      q_upper
    ) |>
    dplyr::inner_join(
      flat_df,
      by = c("condition_idx", "rank_idx")
    ) |>
    dplyr::filter(
      rt >= q_lower & rt <= q_upper
    )
}
