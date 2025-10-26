#' Summarise data by groups with optional pivoting
#'
#' Groups data by specified columns, applies summary expressions, and optionally
#' pivots the results wider. This function is designed for flexible aggregation
#' and reshaping of simulation results.
#'
#' When called without `.data` (or with `.data = NULL`), returns a
#' `multieam_summarise_by_spec` object that can be:
#' - Combined with other specs using the `+` operator
#' - Applied to data later by calling it as a function
#'
#' @param .data A data frame to summarise, or NULL to create a delayed spec
#' @param ... Summary expressions to evaluate for each group, using dplyr-style
#'   syntax. Named arguments become column names in the output.
#' @param .by Character vector of column names to group by (default: "condition_idx")
#' @param .wider_by Character vector of column names to keep as identifying columns
#'   after pivoting wider (default: "condition_idx"). Must be a subset of .by.
#'   If different from .by, the remaining columns in .by will be pivoted across
#'   the result.
#' @return If `.data` is provided: A data frame with class "multieam_summarise_by_tbl"
#'   containing the summarised and potentially pivoted results.
#'   If `.data` is NULL: A `multieam_summarise_by_spec` object for delayed evaluation.
#' @export
summarise_by <- function(
    .data = NULL,
    ...,
    .by = c("condition_idx"),
    .wider_by = c("condition_idx")) {
  dots <- rlang::enquos(...)
  
  # If .data is missing or NULL, return a spec object for delayed evaluation
  if (is.null(.data)) {
    spec <- list(
      list(
        dots = dots,
        .by = .by,
        .wider_by = .wider_by
      )
    )
    class(spec) <- "multieam_summarise_by_spec"
    return(spec)
  }

  # Use the shared implementation
  summarise_by_impl(.data, dots, .by, .wider_by)
}


#' Internal function to perform the core summarise_by logic
#'
#' @param .data A data frame to summarise
#' @param dots Quosures containing the summary expressions
#' @param .by Character vector of column names to group by
#' @param .wider_by Character vector of column names to keep as identifying columns
#' @return A data frame with class "multieam_summarise_by_tbl"
#' @keywords internal
summarise_by_impl <- function(.data, dots, .by, .wider_by) {
  # Validate that .wider_by is a subset of .by
  if (!all(.wider_by %in% .by)) {
    stop(
      ".wider_by must be a subset of .by.\n",
      "  .by = c(", paste0('"', .by, '"', collapse = ", "), ")\n",
      "  .wider_by = c(", paste0('"', .wider_by, '"', collapse = ", "), ")\n",
      "  Invalid columns in .wider_by: ",
      paste0('"', setdiff(.wider_by, .by), '"', collapse = ", ")
    )
  }

  # group_by
  grouped <- if (is.null(.by)) {
    list(.data)
  } else {
    dplyr::group_split(.data, dplyr::across(dplyr::all_of(.by)))
  }

  # evaluate - use lapply for better performance, then bind efficiently
  result_list <- lapply(grouped, function(sub_df) {
    # Extract key values efficiently (group_split returns ungrouped data)
    key_vals <- if (is.null(.by)) {
      list()
    } else {
      # Direct extraction from first row - much faster
      stats::setNames(
        lapply(.by, function(col) sub_df[[col]][1]),
        .by
      )
    }

    vals <- purrr::imap(dots, function(expr, name) {
      # Use the assigned name directly if provided, not the expression text
      colname <- name

      val <- rlang::eval_tidy(expr, data = sub_df)

      # Check length first (faster for atomic vectors)
      if (length(val) > 1 || is.list(val)) {
        nm <- names(val)
        if (is.null(nm) || any(!nzchar(nm))) {
          # No names or empty names: use X1, X2, etc.
          nm <- paste0(colname, "_X", seq_along(val))
        } else {
          # Has names: use assigned_name_original_name
          nm <- paste0(colname, "_", nm)
        }
        stats::setNames(as.list(val), nm)
      } else {
        stats::setNames(list(val), colname)
      }
    }) |> purrr::flatten()

    c(key_vals, vals)
  })
  result_df <- dplyr::bind_rows(result_list)

  # Pivot wider if .by and .wider_by are different
  pivot_cols <- setdiff(.by, .wider_by)

  if (length(pivot_cols) > 0) {
    # Get all value columns (not in .by)
    value_cols <- setdiff(names(result_df), .by)

    # Create a combined column for pivoting with structured names
    # e.g., "item_idx_1", "item_idx_2"
    result_df$.pivot_key <- do.call(
      paste,
      c(
        lapply(pivot_cols, function(col) paste0(col, "_", result_df[[col]])),
        list(sep = "_")
      )
    )

    # Pivot wider: spread pivot_cols across columns
    result_df <- tidyr::pivot_wider(
      result_df,
      id_cols = dplyr::all_of(.wider_by),
      names_from = ".pivot_key",
      values_from = dplyr::all_of(value_cols),
      names_sep = "_"
    )
  }

  # assign the class and store .wider_by as attribute
  class(result_df) <- c("multieam_summarise_by_tbl", class(result_df))
  attr(result_df, "wider_by") <- .wider_by

  result_df
}

#' Join two multieam_summarise_by_tbl objects
#'
#' S3 method for the + operator to join two summary tables created by
#' \code{summarise_by}. Tables must have identical .wider_by attributes
#' to be joined.
#'
#' @param e1 First multieam_summarise_by_tbl object
#' @param e2 Second multieam_summarise_by_tbl object
#' @return A joined data frame with class "multieam_summarise_by_tbl",
#'   preserving the .wider_by attribute from the input tables
#' @export
`+.multieam_summarise_by_tbl` <- function(e1, e2) {
  # Only process if both are multieam_summarise_by_tbl
  if (!inherits(e1, "multieam_summarise_by_tbl") ||
    !inherits(e2, "multieam_summarise_by_tbl")) {
    # Not our class duty - fall back to default
    return(NextMethod("+"))
  }

  # Get .wider_by from both tables
  wider_by_1 <- attr(e1, "wider_by")
  wider_by_2 <- attr(e2, "wider_by")

  # Check if .wider_by attributes are identical
  if (!identical(wider_by_1, wider_by_2)) {
    stop(
      "Cannot join tables with different .wider_by attributes.\n",
      "  Table 1 .wider_by: c(",
      paste0('"', wider_by_1, '"', collapse = ", "), ")\n",
      "  Table 2 .wider_by: c(",
      paste0('"', wider_by_2, '"', collapse = ", "), ")\n",
      "  Both tables must have the same .wider_by for joining."
    )
  }

  # Join the two tables by the .wider_by columns
  result <- dplyr::full_join(e1, e2, by = wider_by_1)

  # Preserve the class and .wider_by attribute
  class(result) <- c("multieam_summarise_by_tbl", class(result))
  attr(result, "wider_by") <- wider_by_1

  result
}

#' Apply a summarise_by spec to data
#'
#' This function is called when a `multieam_summarise_by_spec` object is used
#' as a function. It applies all the stored summarise_by operations to the
#' provided data.
#'
#' @param spec A multieam_summarise_by_spec object
#' @param .data A data frame to apply the spec to
#' @param ... Additional arguments (ignored)
#' @return A data frame with class "multieam_summarise_by_tbl"
#' @export
`$.multieam_summarise_by_spec` <- function(spec, name) {
  if (name == "apply") {
    return(function(.data) {
      apply_summarise_by_spec(spec, .data)
    })
  }
  NextMethod("$")
}

#' @export
print.multieam_summarise_by_spec <- function(x, ...) {
  cat("<multieam_summarise_by_spec>\n")
  cat("Number of summarise operations:", length(x), "\n")
  for (i in seq_along(x)) {
    cat("\nOperation", i, ":\n")
    cat("  .by:", paste(x[[i]]$.by, collapse = ", "), "\n")
    cat("  .wider_by:", paste(x[[i]]$.wider_by, collapse = ", "), "\n")
    cat("  Summary expressions:", length(x[[i]]$dots), "\n")
  }
  invisible(x)
}

#' Internal function to apply a spec to data
#'
#' @param spec A multieam_summarise_by_spec object
#' @param .data A data frame
#' @return A data frame with class "multieam_summarise_by_tbl"
#' @keywords internal
apply_summarise_by_spec <- function(spec, .data) {
  if (!inherits(spec, "multieam_summarise_by_spec")) {
    stop("spec must be a multieam_summarise_by_spec object")
  }
  
  # Apply each summarise_by operation and combine with +
  results <- lapply(spec, function(op) {
    # Use the shared implementation
    summarise_by_impl(.data, op$dots, op$.by, op$.wider_by)
  })
  
  # Combine results using the + operator
  if (length(results) == 1) {
    return(results[[1]])
  }
  
  result <- results[[1]]
  for (i in 2:length(results)) {
    result <- result + results[[i]]
  }
  
  result
}

#' Make multieam_summarise_by_spec callable as a function
#'
#' @param x A multieam_summarise_by_spec object
#' @param ... Arguments passed to the function (expects .data as first arg)
#' @return Result of applying the spec to data
#' @keywords internal
as.function.multieam_summarise_by_spec <- function(x, ...) {
  function(.data) {
    apply_summarise_by_spec(x, .data)
  }
}

#' Add two summarise_by specs together
#'
#' S3 method for the + operator to combine two `multieam_summarise_by_spec`
#' objects into a single spec that will apply both operations.
#'
#' @param e1 First multieam_summarise_by_spec or multieam_summarise_by_tbl object
#' @param e2 Second multieam_summarise_by_spec or multieam_summarise_by_tbl object
#' @return A combined multieam_summarise_by_spec object
#' @export
`+.multieam_summarise_by_spec` <- function(e1, e2) {
  # Handle spec + spec
  if (inherits(e1, "multieam_summarise_by_spec") &&
      inherits(e2, "multieam_summarise_by_spec")) {
    result <- c(e1, e2)
    class(result) <- "multieam_summarise_by_spec"
    return(result)
  }
  
  # If one is not a spec, fall back to default
  NextMethod("+")
}

