summarise_parameter_posterior <- function(data, ...) {
  UseMethod("summarise_parameter_posterior")
}

#' @export
summarise_parameter_posterior.abc <- function(data, ..., ci_level = 0.95) {
  # check the parameters
  dots <- rlang::list2(...)

  # Extract any custom summary functions from dots
  # Functions passed directly are treated as custom summaries
  summary_funs <- dots[sapply(dots, is.function)]

  # Remove functions from dots so we don't process them elsewhere
  dots <- dots[!sapply(dots, is.function)]

  # Extract values - prefer adjusted over unadjusted
  if (!is.null(data$adj.values)) {
    df <- as.data.frame(data$adj.values)
  } else if (!is.null(data$unadj.values)) {
    df <- as.data.frame(data$unadj.values)
  } else {
    stop("Neither `adj.values` nor `unadj.values` found in the abc object.")
  }

  # Get parameter names
  param_names <- colnames(df)
  if (is.null(param_names)) {
    param_names <- paste0("param_", seq_len(ncol(df)))
    colnames(df) <- param_names
  }

  # Calculate summaries for each parameter
  results <- list()

  for (param in param_names) {
    values <- df[[param]]
    values <- values[is.finite(values)]

    # Create dynamic column names with quantile values
    alpha <- 1 - ci_level
    ci_lower_name <- sprintf("ci_lower_%.3f", alpha / 2)
    ci_upper_name <- sprintf("ci_upper_%.3f", 1 - alpha / 2)

    if (length(values) == 0) {
      results[[param]] <- list(
        mean = NA_real_,
        median = NA_real_
      )
      results[[param]][[ci_lower_name]] <- NA_real_
      results[[param]][[ci_upper_name]] <- NA_real_
    } else {
      # Basic summaries
      alpha <- 1 - ci_level
      ci_lower <- quantile(values, probs = alpha / 2, na.rm = TRUE)
      ci_upper <- quantile(values, probs = 1 - alpha / 2, na.rm = TRUE)

      # Create dynamic column names with quantile values
      ci_lower_name <- sprintf("ci_lower_%.3f", alpha / 2)
      ci_upper_name <- sprintf("ci_upper_%.3f", 1 - alpha / 2)

      results[[param]] <- list(
        mean = mean(values, na.rm = TRUE),
        median = median(values, na.rm = TRUE)
      )
      results[[param]][[ci_lower_name]] <- as.numeric(ci_lower)
      results[[param]][[ci_upper_name]] <- as.numeric(ci_upper)

      # Apply custom summary functions if provided
      if (length(summary_funs) > 0) {
        for (fun_name in names(summary_funs)) {
          fun <- summary_funs[[fun_name]]
          results[[param]][[fun_name]] <- fun(values)
        }
      }
    }
  }

  # Convert to data frame
  summary_df <- do.call(rbind, lapply(names(results), function(param) {
    row <- as.data.frame(results[[param]])
    row$parameter <- param
    row
  }))

  # Reorder columns to put parameter first
  col_order <- c("parameter", setdiff(names(summary_df), "parameter"))
  summary_df <- summary_df[, col_order]
  rownames(summary_df) <- NULL

  # Add attributes
  attr(summary_df, "ci_level") <- ci_level
  attr(summary_df, "n_samples") <- nrow(df)

  return(summary_df)
}
