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
#'   all columns from trial results (excluding .item_params), and all variables from cond_params
#' @export
flatten_simulation_results <- function(sim_results) {
  # Get first condition and first trial to determine structure
  first_condition <- sim_results[[1]]
  first_trial <- first_condition$result[[1]]
  
  # Get trial column names (excluding .item_params)
  trial_col_names <- names(first_trial)
  trial_col_names <- trial_col_names[!grepl("^\\.item_params$", trial_col_names)]
  
  # Get condition parameter names
  cond_param_names <- names(first_condition$cond_params)

  # First pass: calculate total number of rows needed
  # Use the first column from trial results to determine item count
  first_trial_col <- trial_col_names[1]
  total_rows <- 0
  for (cond_idx in seq_along(sim_results)) {
    condition <- sim_results[[cond_idx]]
    trials <- condition$result
    for (trial_idx in seq_along(trials)) {
      trial <- trials[[trial_idx]]
      total_rows <- total_rows + length(trial[[first_trial_col]])
    }
  }

  # Pre-allocate data.table with exact size
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

  # Second pass: fill the pre-allocated vectors
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

  # Create data.table from lists - use do.call to expand the list properly
  result_dt <- do.call(data.table::data.table, dt_lists)

  return(result_dt)
}