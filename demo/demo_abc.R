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

  result_df
}

# summarise
condition_summary <- map_by_condition(
  sim_output,
  function(cond_df) {
    # clean data here
    complete_df <- cond_df |>
      dplyr::filter(!is.na(rt))

    # extract the summary
    summarise_by(
      complete_df,
      .by = c("condition_idx", "item_idx"),
      rt_mean = mean(rt),
      rt_quantiles = quantile(rt, probs = c(0.1, 0.5, 0.9)),
    )
  }
)
