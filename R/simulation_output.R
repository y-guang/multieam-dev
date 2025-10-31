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

  simulation_dataset_dir <- file.path(
    output_dir,
    simulation_output_fs_proto$dataset_dir
  )

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
#' @param .parallel Logical or NULL.
#' @param .n_cores Integer. Number of CPU cores to use for parallel processing.
#'   If NULL, uses \code{detectCores() - 1}. Only used when
#' \code{.parallel = TRUE}.
#' @param .progress Logical, whether to show a progress bar (default: FALSE)
#' @return A list containing the results of applying .f to each condition,
#' with names corresponding to condition indices
#' @export
map_by_condition <- function(
    simulation_output,
    .f,
    ...,
    .combine = dplyr::bind_rows,
    .parallel = NULL,
    .n_cores = NULL,
    .progress = FALSE) {
  # TODO: persist results to disk if too large

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

  # Apply heuristic to determine parallel processing
  if (is.null(.parallel)) {
    .parallel <- map_by_condition.parallel.heuristic(chunk_indices)
  }

  # Set up progress bar if requested
  if (.progress && requireNamespace("pbapply", quietly = TRUE)) {
    pb_fun <- pbapply::pblapply
  } else {
    pb_fun <- lapply
    if (.progress) {
      message("Install 'pbapply' package for progress bar support")
    }
  }

  # Define the chunk processing function
  process_chunk <- map_by_condition.process_chunk(dataset, .f, ...)

  # Process chunks based on parallel preference
  if (.parallel) {
    # Setup parallel cluster
    if (is.null(.n_cores)) {
      .n_cores <- max(1, parallel::detectCores(logical = FALSE) - 1)
    }

    cl <- parallel::makeCluster(min(.n_cores, length(chunk_indices)))
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Note: arrow dataset is not thread-safe, export the open_dataset
    open_dataset_fn <- simulation_output$open_dataset
    process_chunk_parallel <- map_by_condition.process_chunk(
      open_dataset_fn, .f, ...
    )

    # Export required objects and functions to cluster
    f_globals <- codetools::findGlobals(.f, merge = FALSE)
    f_env <- environment(.f)
    all_globals <- c(f_globals$functions, f_globals$variables)
    detected_deps <- all_globals[
      sapply(all_globals, exists, envir = f_env, inherits = TRUE)
    ]
    export_vars <- unique(c(
      "open_dataset_fn",
      ".f",
      "process_chunk_parallel",
      "map_by_condition.process_chunk",
      detected_deps
    ))

    parallel::clusterExport(cl, export_vars, envir = environment())

    # Load required packages on cluster nodes
    parallel::clusterEvalQ(cl, {
      library(dplyr)
      library(arrow)
      library(multieam)
    })

    # Run parallel processing
    if (.progress && requireNamespace("pbapply", quietly = TRUE)) {
      all_condition_results <- pbapply::pblapply(
        chunk_indices,
        process_chunk_parallel,
        cl = cl
      )
    } else {
      if (.progress) {
        message("Install 'pbapply' package for progress bar support")
      }
      all_condition_results <- parallel::parLapply(
        cl,
        chunk_indices,
        process_chunk_parallel
      )
    }
  } else {
    # Sequential processing
    all_condition_results <- pb_fun(chunk_indices, process_chunk)
  }

  # Concatenate all chunk results while preserving condition indexing
  all_results <- do.call(c, all_condition_results)

  # If .combine is provided and we have results, combine them
  if (length(all_results) > 0 && !is.null(.combine)) {
    return(do.call(.combine, all_results))
  } else {
    return(all_results)
  }
}


#' Heuristic to determine if parallel processing should be used
#'
#' @param chunk_indices Vector of chunk indices
#' @return Logical value indicating whether to use parallel processing
#' @keywords internal
map_by_condition.parallel.heuristic <- function(chunk_indices) {
  # Check if parallel package is available and has more than 2 cores
  # then check if chunk_indices > 5 times the number of cores
  if (requireNamespace("parallel", quietly = TRUE)) {
    num_cores <- parallel::detectCores(logical = FALSE)
    if (!is.na(num_cores) && num_cores > 2 &&
      length(chunk_indices) > 5 * num_cores) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}


#' Process a single chunk for map_by_condition
#'
#' @param open_dataset_fn Arrow dataset object or function that returns a dataset
#' @param .f Function to apply to each condition's data
#' @param ... Additional arguments passed to .f
#' @return Function that processes a chunk_idx
#' @keywords internal
map_by_condition.process_chunk <- function(open_dataset_fn, .f, ...) {
  function(chunk_idx) {
    # Get the dataset (either use directly or call function)
    dataset <- if (is.function(open_dataset_fn)) {
      open_dataset_fn()
    } else {
      open_dataset_fn
    }

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
  }
}

simulation_output_fs_proto <- list(
  config_file = "simulation_config.rds",
  dataset_dir = "simulation_dataset",
  evaluated_conditions_dir = "evaluated_conditions"
)

#' Initialize simulation output directory structure
#'
#' Creates and validates the output directory structure for a simulation.
#' This function ensures the directory is empty (or creates it), then creates
#' the required subdirectories based on simulation_output_fs_proto.
#'
#' @param output_dir The base output directory path
#' @return The output_dir path (invisibly for chaining)
#' @keywords internal
init_simulation_output_dir <- function(output_dir) {
  # check empty output directory
  if (dir.exists(output_dir) &&
    length(list.files(output_dir, all.files = FALSE, no.. = TRUE)) > 0
  ) {
    stop(
      "The specified simulation output directory must be empty: ",
      output_dir
    )
  }

  # Create subdirectories based on fs_proto
  evaluated_conditions_dir <- file.path(
    output_dir,
    simulation_output_fs_proto$evaluated_conditions_dir
  )
  simulation_dataset_dir <- file.path(
    output_dir,
    simulation_output_fs_proto$dataset_dir
  )

  if (!dir.exists(evaluated_conditions_dir)) {
    dir.create(evaluated_conditions_dir, recursive = TRUE)
  }
  if (!dir.exists(simulation_dataset_dir)) {
    dir.create(simulation_dataset_dir, recursive = TRUE)
  }

  invisible(output_dir)
}
