#' Extract column names from simulation results
#'
#' This function extracts trial column names and condition parameter names
#' from simulation results structure.
#'
#' @param sim_results The output from run_simulation(), a list of conditions
#' @return A list with trial_col_names and cond_param_names
get_column_names <- function(sim_results) {
  # Get first condition and first trial to determine structure
  first_condition <- sim_results[[1]]
  first_trial <- first_condition$result[[1]]

  # Get trial column names (excluding .item_params)
  trial_col_names <- names(first_trial)
  trial_col_names <- trial_col_names[
    !grepl("^\\.item_params$", trial_col_names)
  ]

  # Get condition parameter names
  cond_param_names <- names(first_condition$cond_params)

  return(list(
    trial_col_names = trial_col_names,
    cond_param_names = cond_param_names
  ))
}

#' Calculate total number of rows needed for flattened data
#'
#' This function counts the total number of items across all conditions and
#' trials to determine the size needed for pre-allocation.
#'
#' @param sim_results The output from run_simulation(), a list of conditions
#' @param first_trial_col Name of the first trial column to use for counting
#' @return Integer, total number of rows needed
calculate_total_rows <- function(sim_results, first_trial_col) {
  total_rows <- 0
  for (cond_idx in seq_along(sim_results)) {
    condition <- sim_results[[cond_idx]]
    trials <- condition$result
    for (trial_idx in seq_along(trials)) {
      trial <- trials[[trial_idx]]
      total_rows <- total_rows + length(trial[[first_trial_col]])
    }
  }
  return(total_rows)
}

#' Pre-allocate data.table columns with appropriate data types
#'
#' This function creates pre-allocated vectors for all columns in the final
#' data.table, determining data types from the first trial and condition.
#'
#' @param sim_results The output from run_simulation(), a list of conditions
#' @param trial_col_names Character vector of trial column names
#' @param cond_param_names Character vector of condition parameter names
#' @param total_rows Integer, total number of rows to pre-allocate
#' @return Named list of pre-allocated vectors for each column
preallocate_columns <- function(
    sim_results,
    trial_col_names,
    cond_param_names,
    total_rows) {
  first_condition <- sim_results[[1]]
  first_trial <- first_condition$result[[1]]

  # Define all column names
  all_cols <- c(
    "condition_idx",
    "trial_idx",
    "rank_idx",
    trial_col_names,
    cond_param_names
  )

  # Initialize lists for each column
  dt_lists <- vector("list", length(all_cols))
  names(dt_lists) <- all_cols

  # Pre-allocate basic index vectors
  dt_lists$condition_idx <- integer(total_rows)
  dt_lists$trial_idx <- integer(total_rows)
  dt_lists$rank_idx <- integer(total_rows)

  # Pre-allocate trial result columns based on first trial
  for (col_name in trial_col_names) {
    first_val <- first_trial[[col_name]][1]
    if (is.numeric(first_val)) {
      dt_lists[[col_name]] <- numeric(total_rows)
    } else if (is.integer(first_val)) {
      dt_lists[[col_name]] <- integer(total_rows)
    } else if (is.logical(first_val)) {
      dt_lists[[col_name]] <- logical(total_rows)
    } else {
      dt_lists[[col_name]] <- character(total_rows)
    }
  }

  # Pre-allocate cond_param columns based on first condition
  for (param_name in cond_param_names) {
    first_val <- first_condition$cond_params[[param_name]][1]
    if (is.numeric(first_val)) {
      dt_lists[[param_name]] <- numeric(total_rows)
    } else if (is.integer(first_val)) {
      dt_lists[[param_name]] <- integer(total_rows)
    } else if (is.logical(first_val)) {
      dt_lists[[param_name]] <- logical(total_rows)
    } else {
      dt_lists[[param_name]] <- character(total_rows)
    }
  }

  return(dt_lists)
}

#' Fill pre-allocated data.table with simulation results
#'
#' This function fills the pre-allocated data.table vectors with data from
#' simulation results, iterating through all conditions and trials.
#'
#' @param sim_results The output from run_simulation(), a list of conditions
#' @param dt_lists Named list of pre-allocated vectors for each column
#' @param trial_col_names Character vector of trial column names
#' @param cond_param_names Character vector of condition parameter names
#' @param first_trial_col Name of the first trial column to use for item
#' counting
#' @return Named list of filled vectors ready for data.table creation
fill_data_table <- function(
    sim_results,
    dt_lists,
    trial_col_names,
    cond_param_names,
    first_trial_col) {
  current_row <- 1

  for (cond_idx in seq_along(sim_results)) {
    condition <- sim_results[[cond_idx]]
    trials <- condition$result
    cond_params <- condition$cond_params

    for (trial_idx in seq_along(trials)) {
      trial <- trials[[trial_idx]]
      # Use the first trial column to determine item count
      n_items_this_trial <- length(trial[[first_trial_col]])

      # Skip trials with no items
      if (n_items_this_trial == 0) {
        next
      }

      # Calculate row indices for this trial
      row_indices <- current_row:(current_row + n_items_this_trial - 1)

      # Fill basic index columns
      dt_lists$condition_idx[row_indices] <- cond_idx
      dt_lists$trial_idx[row_indices] <- trial_idx
      dt_lists$rank_idx[row_indices] <- seq_len(n_items_this_trial)

      # Fill trial result columns (excluding .item_params)
      for (col_name in trial_col_names) {
        dt_lists[[col_name]][row_indices] <- trial[[col_name]]
      }

      # Fill condition parameter columns
      for (param_name in cond_param_names) {
        dt_lists[[param_name]][row_indices] <-
          cond_params[[param_name]][trial_idx]
      }

      current_row <- current_row + n_items_this_trial
    }
  }

  return(dt_lists)
}

#' Convert simulation results to a tidy data.table
#'
#' This function takes the nested list output from run_simulation() and converts
#' it into a tidy data.table where each row represents one item response.
#' The function pre-allocates the data.table to the exact size needed and then
#' fills it efficiently. Column names are dynamically determined from the first
#' trial result, excluding any .item_params verbose output.
#'
#' @param sim_results The output from run_simulation(), a list of conditions
#' @return A data.table with columns: condition_idx, trial_idx, rank_idx,
#' all columns from trial results (excluding .item_params), and all variables
#' from cond_params
#' @export
flatten_simulation_results <- function(sim_results) {
  # Extract column names from simulation structure
  column_info <- get_column_names(sim_results)
  trial_col_names <- column_info$trial_col_names
  cond_param_names <- column_info$cond_param_names

  # Calculate total number of rows needed
  first_trial_col <- trial_col_names[1]
  total_rows <- calculate_total_rows(sim_results, first_trial_col)

  # Pre-allocate data.table columns with appropriate data types
  dt_lists <- preallocate_columns(
    sim_results,
    trial_col_names,
    cond_param_names,
    total_rows
  )

  # Fill the pre-allocated vectors with data
  dt_lists <- fill_data_table(
    sim_results,
    dt_lists,
    trial_col_names,
    cond_param_names,
    first_trial_col
  )

  # Create data.table from lists - use do.call to expand the list properly
  result_dt <- do.call(data.table::data.table, dt_lists)

  return(result_dt)
}
