###############
# model setup #
###############
n_items <- 10
prior_formulas <- list(
  n_items ~ 10,
  # parameters with distributions
  A_beta_0 ~ distributional::dist_uniform(1.00, 10.00),
  A_beta_1 ~ distributional::dist_uniform(1.00, 10.00),
  # V
  V_beta_1 ~ distributional::dist_uniform(-0.50, -0.01),
  V_beta_0 ~ distributional::dist_uniform(1.0, 5.0),
  # ndt
  ndt ~ distributional::dist_uniform(0.01, 3.00),
  # noise param
  noise_coef ~ 1,
  # between trial varibaility
  sigma ~ distributional::dist_uniform(0.1, 3.00)
)

between_trial_formulas <- list(
  V_var ~ distributional::dist_normal(0, sigma)
)

item_formulas <- list(
  A ~ A_beta_0 + seq(1, n_items) * A_beta_1,
  V ~ V_beta_0 + seq(1, n_items) * V_beta_1 + V_var
)

noise_factory <- function(context) {
  noise_coef <- context$noise_coef

  function(n, dt) {
    noise_coef * rnorm(n, mean = 0, sd = sqrt(dt))
  }
}

####################
# simulation setup #
####################
sim_config <- new_simulation_config(
  prior_formulas = prior_formulas,
  between_trial_formulas = between_trial_formulas,
  item_formulas = item_formulas,
  n_conditions_per_chunk = NULL, # automatic chunking
  n_conditions = 200,
  n_trials_per_condition = 100,
  n_items = n_items,
  max_reached = n_items,
  max_t = 100,
  dt = 0.01,
  noise_mechanism = "add",
  noise_factory = noise_factory,
  model = "ddm",
  parallel = TRUE,
  n_cores = NULL, # Will use default: detectCores() - 1
  rand_seed = NULL # Will use default random seed
)
print(sim_config)

# output temporary path setup
temp_output_path <- tempfile("multieam_demo_output")
# remove if exists
if (dir.exists(temp_output_path)) {
  unlink(temp_output_path, recursive = TRUE)
}
cat("Temporary output path:\n")
cat(temp_output_path, "\n")

##################
# run simulation #
##################
sim_output <- run_simulation(
  config = sim_config,
  output_dir = temp_output_path
)

#####################
# output processing #
#####################
# preview the output data
head(sim_output$open_dataset())

summarise_by <- function(
    .data,
    ...,
    .by = c("condition_idx"),
    .wider_by = c("condition_idx")) {
  dots <- rlang::enquos(...)

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

  # Efficient row binding
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

# Define + operator for multieam_summarise_by_tbl
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

prepare_abc_input <- function(
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



# summarise
summary_pipe <- function(df) {
  summarise_by(
    df,
    .by = c("condition_idx"),
    rt_mean = mean(rt),
    rt_quantiles = quantile(rt, probs = c(0.1, 0.5, 0.9)),
    lm_coef = lm(rt ~ item_idx)$coefficients
  ) +
    summarise_by(
      df,
      .by = c("condition_idx", "item_idx"),
      rt_mean = mean(rt),
      rt_quantiles = quantile(rt, probs = c(0.1, 0.5, 0.9))
    )
}

simulation_sumstat <- map_by_condition(
  sim_output,
  .progress = TRUE,
  function(cond_df) {
    # clean data here
    complete_df <- cond_df |>
      dplyr::filter(!is.na(rt))

    # extract the summary
    summary_pipe(complete_df)
  }
)

# pretend observed data is condition 1
observed_data <- sim_output$open_dataset() |>
  dplyr::filter(chunk_idx == 1, condition_idx == 1) |>
  dplyr::collect()

target_summary <- summary_pipe(observed_data)

# Prepare data for ABC fitting
abc_input <- prepare_abc_input(
  simulation_output = sim_output,
  simulation_summary = simulation_sumstat,
  target_summary = target_summary,
  param = c("A_beta_0", "A_beta_1", "V_beta_0", "V_beta_1", "ndt", "sigma")
)

abc_rejection <- abc::abc(
  target = abc_input$target,
  param = abc_input$param,
  sumstat = abc_input$sumstat,
  tol = 0.05,
  method = "rejection"
)