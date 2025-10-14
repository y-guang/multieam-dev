#' Create a multieam_simulation_output object
#'
#' @keywords internal
new_simulation_output <- function(
    simulation_config,
    output_dir) {
  # validate inputs
  if (!inherits(simulation_config, "multieam_simulation_config")) {
    stop("simulation_config must be a multieam_simulation_config object")
  }

  if (!dir.exists(output_dir)) {
    stop("output_dir does not exist: ", output_dir)
  }

  # Get the dataset directory
  simulation_dataset_dir <- simulation_output_dir_to_dataset_dir(output_dir)

  # Create the output object
  ret <- list(
    simulation_config = simulation_config,
    output_dir = output_dir,
    open_dataset = local({
      dataset_dir <- simulation_dataset_dir
      function() {
        arrow::open_dataset(dataset_dir)
      }
    })
  )

  # Create S3 object
  structure(ret, class = "multieam_simulation_output")
}

#' Get the dataset directory from the simulation output directory
#'
#' @keywords internal
simulation_output_dir_to_dataset_dir <- function(output_dir) {
  file.path(output_dir, "simulation_dataset")
}


#' Map a function by condition across simulation output chunks
#'
#' This function processes simulation output by gathering all chunks, iterating
#' through them one by one, filtering and collecting data by chunk, then
#' applying a user-defined function by condition within each chunk.
#'
#' @param simulation_output A multieam_simulation_output object containing the
#' dataset and configuration
#' @param .f A function to apply to each condition's data. The function should
#' accept a data frame representing one condition's results
#' @param ... Additional arguments passed to the function .f
#' @param .combine Function to combine results (default: dplyr::bind_rows)
#' @param .progress Logical, whether to show a progress bar (default: TRUE)
#' @return A list containing the results of applying .f to each condition,
#' with names corresponding to condition indices
#' @export
map_by_condition <- function(
    simulation_output,
    .f,
    ...,
    .combine = dplyr::bind_rows,
    .progress = FALSE) {
  # TODO: persist results to disk if too large
  # TODO: parallel processing option

  # Validate input
  if (!inherits(simulation_output, "multieam_simulation_output")) {
    stop("simulation_output must be a multieam_simulation_output object")
  }

  if (!is.function(.f)) {
    stop(".f must be a function")
  }

  # Get the dataset
  dataset <- simulation_output$open_dataset()

  # Gather all chunk indices
  chunk_indices <- dataset |>
    dplyr::select(chunk_idx) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull(chunk_idx) |>
    sort()

  # Set up progress bar if requested
  if (.progress && requireNamespace("pbapply", quietly = TRUE)) {
    pb_fun <- pbapply::pblapply
  } else {
    pb_fun <- lapply
    if (.progress) {
      message("Install 'pbapply' package for progress bar support")
    }
  }

  # Process each chunk and collect all condition results
  all_condition_results <- pb_fun(chunk_indices, function(chunk_idx) {
    # Load this chunk's data
    chunk_data <- dataset |>
      dplyr::filter(chunk_idx == !!chunk_idx) |>
      dplyr::collect()

    # Split data by condition and get condition indices
    condition_groups <- chunk_data |>
      dplyr::group_by(condition_idx) |>
      dplyr::group_split(.keep = TRUE)

    # Extract condition indices for naming
    condition_indices <- sapply(
      condition_groups,
      function(x) unique(x$condition_idx)
    )

    # Apply function to each condition's data
    chunk_results <- lapply(condition_groups, .f, ...)

    # Name the results with condition indices
    names(chunk_results) <- as.character(condition_indices)

    return(chunk_results)
  })

  # Concatenate all chunk results while preserving condition indexing
  all_results <- do.call(c, all_condition_results)

  # If .combine is provided and we have results, combine them
  if (length(all_results) > 0 && !is.null(.combine)) {
    return(do.call(.combine, all_results))
  } else {
    return(all_results)
  }
}
