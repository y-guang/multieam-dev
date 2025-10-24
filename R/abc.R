#' Build input for Approximate Bayesian Computation (ABC)
#'
#' Prepares simulation output, summary statistics, and target data for ABC
#' analysis using the \code{abc} package. Extracts parameters and summary
#' statistics from simulation results and formats them into matrices suitable
#' for ABC parameter estimation.
#'
#' @param simulation_output A multieam_simulation_output object containing the
#'   simulation dataset and configuration
#' @param simulation_summary A data frame containing summary statistics for each
#'   simulated condition. Should have a 'condition_idx' column and be created by
#'   \code{summarise_by} to include a 'wider_by' attribute for proper column handling.
#' @param target_summary A data frame containing target summary statistics to match
#'   against simulation results. Should have the same summary statistic columns as
#'   simulation_summary (excluding 'wider_by' columns).
#' @param param Character vector of parameter names to extract from simulation_output.
#'   These parameters will be used as the parameter space for ABC estimation.
#'
#' @return A list with components suitable for \code{abc::abc}
#' @export
build_abc_input <- function(
    simulation_output,
    simulation_summary,
    target_summary,
    param) {
  # Validate inputs
  if (!inherits(simulation_output, "multieam_simulation_output")) {
    stop("simulation_output must be a multieam_simulation_output object")
  }

  if (!is.data.frame(simulation_summary)) {
    stop("simulation_summary must be a data frame or tibble")
  }

  if (!is.data.frame(target_summary)) {
    stop("target_summary must be a data frame or tibble")
  }

  if (!is.character(param) || length(param) == 0) {
    stop("param must be a non-empty character vector")
  }

  # NSE variable bindings for R CMD check
  condition_idx <- chunk_idx <- NULL

  # Get the dataset
  dataset <- simulation_output$open_dataset()

  select_cols <- c("chunk_idx", "condition_idx", param)

  # Check if all requested parameters exist in the dataset
  available_cols <- names(dataset)
  missing_params <- setdiff(param, available_cols)
  if (length(missing_params) > 0) {
    stop(
      "The following parameters are not available in the output:\n  ",
      paste(missing_params, collapse = ", "),
      "\n\nAvailable columns:\n  ",
      paste(setNames(available_cols, NULL), collapse = ", ")
    )
  }

  # Extract parameters - get one row per condition
  param_df <- dataset |>
    dplyr::select(dplyr::all_of(select_cols)) |>
    dplyr::distinct(chunk_idx, condition_idx, .keep_all = TRUE) |>
    dplyr::select(-chunk_idx) |>
    dplyr::arrange(condition_idx) |>
    dplyr::collect()

  # Process simulation summary statistics
  # Get wider_by attribute to determine which columns to exclude
  wider_by <- attr(simulation_summary, "wider_by")
  if (is.null(wider_by)) {
    # Default to condition_idx if no wider_by attribute
    wider_by <- "condition_idx"
    warning(
      "simulation_summary does not have a 'wider_by' attribute. ",
      "Defaulting to excluding 'condition_idx' column only."
    )
  }

  # Validate that simulation_summary has condition_idx for alignment
  if (!"condition_idx" %in% names(simulation_summary)) {
    stop("simulation_summary must contain a 'condition_idx' column for join")
  }

  # Sort simulation_summary by condition_idx
  summary_sorted <- simulation_summary |>
    dplyr::arrange(condition_idx)

  # Filter to only include conditions that exist in both
  # Use semi_join for efficient filtering (keeps only matching rows)
  param_df <- param_df |>
    dplyr::semi_join(summary_sorted, by = "condition_idx")

  summary_sorted <- summary_sorted |>
    dplyr::semi_join(param_df, by = "condition_idx")

  # Convert parameters to matrix (exclude condition_idx)
  param_matrix <- as.matrix(param_df[, param, drop = FALSE])
  rownames(param_matrix) <- param_df$condition_idx

  # Extract summary statistics (exclude wider_by columns)
  sumstat_cols <- setdiff(names(summary_sorted), wider_by)

  if (length(sumstat_cols) == 0) {
    stop(
      "No summary statistic columns found after excluding wider_by columns.\n",
      "  wider_by: ", paste(wider_by, collapse = ", "), "\n",
      "  All columns: ", paste(names(summary_sorted), collapse = ", ")
    )
  }

  # Convert summary statistics to matrix
  sumstat_matrix <- as.matrix(summary_sorted[, sumstat_cols, drop = FALSE])
  rownames(sumstat_matrix) <- summary_sorted$condition_idx

  # Process target summary
  # Get wider_by attribute from target_summary (should match simulation_summary)
  target_wider_by <- attr(target_summary, "wider_by")
  if (is.null(target_wider_by)) {
    target_wider_by <- "condition_idx"
    warning(
      "target_summary does not have a 'wider_by' attribute. ",
      "Defaulting to excluding 'condition_idx' column only."
    )
  }

  # Extract target summary statistics (exclude wider_by columns)
  target_sumstat_cols <- setdiff(names(target_summary), target_wider_by)

  # Check if target has the same summary statistics as simulation
  if (!identical(sort(target_sumstat_cols), sort(sumstat_cols))) {
    missing_in_target <- setdiff(sumstat_cols, target_sumstat_cols)
    extra_in_target <- setdiff(target_sumstat_cols, sumstat_cols)

    warning_msg <- "target_summary and simulation_summary have
    different columns - ABC will NOT WORK!"
    if (length(missing_in_target) > 0) {
      warning_msg <- paste0(
        warning_msg, "\n  Missing in target: ",
        paste(missing_in_target, collapse = ", ")
      )
    }
    if (length(extra_in_target) > 0) {
      warning_msg <- paste0(
        warning_msg, "\n  Extra in target: ",
        paste(extra_in_target, collapse = ", ")
      )
    }
    warning(warning_msg)
  }

  # Convert target to vector (ensure same order as sumstat_matrix columns)
  target_vector <- as.numeric(target_summary[1, sumstat_cols])
  names(target_vector) <- sumstat_cols

  # Return list suitable for abc::abc
  result <- list(
    param = param_matrix,
    sumstat = sumstat_matrix,
    target = target_vector,
    param_names = param,
    sumstat_names = sumstat_cols
  )
  return(result)
}
