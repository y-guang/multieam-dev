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
  # Validate input
  if (!inherits(simulation_output, "multieam_simulation_output")) {
    stop("simulation_output must be a multieam_simulation_output object")
  }

  if (!is.function(.f)) {
    stop(".f must be a function")
  }

  # Get the dataset
  dataset <- simulation_output$dataset

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

    # Use dplyr group_split and lapply for cleaner function signature
    chunk_results <- chunk_data |>
      dplyr::group_by(condition_idx) |>
      dplyr::group_split() |>
      lapply(.f, ...)

    return(chunk_results)
  })

  # Flatten all chunk results into a single list
  all_results <- unlist(all_condition_results, recursive = FALSE)

  # If .combine is provided and we have multiple results, combine them
  if (length(all_results) > 1 && !is.null(.combine)) {
    return(do.call(.combine, all_results))
  } else {
    return(all_results)
  }
}
