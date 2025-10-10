#' Create a new simulation configuration
#'
#' This function creates a new multieam simulation configuration object that
#' contains all parameters needed to run a simulation.
#'
#' @param prior_formulas A list of formulas defining prior distributions for
#'   condition-level parameters
#' @param between_trial_formulas A list of formulas defining between-trial
#'   parameters
#' @param item_formulas A list of formulas defining item-level parameters
#' @param n_conditions_per_chunk Number of conditions to process per chunk
#' @param n_conditions Total number of conditions to simulate
#' @param n_trials_per_condition Number of trials per condition
#' @param n_items Number of items per trial
#' @param max_reached Maximum number of items that can be recalled (default: n_items)
#' @param max_t Maximum simulation time
#' @param dt Time step size (default: 0.001)
#' @param noise_mechanism Noise mechanism ("add", "mult", "mult_evidence", or "mult_t")
#' @param noise_factory Function that creates noise functions
#' @param model Model type ("ddm", "ddm-2b", or "lca-gi")
#' @param parallel Whether to run in parallel (default: FALSE)
#' @param n_cores Number of cores for parallel processing (default: NULL)
#' @param rand_seed Random seed for parallel processing (default: NULL)
#' @return A multieam_simulation_config object
#' @export
new_simulation_config <- function(
    prior_formulas = list(),
    between_trial_formulas = list(),
    item_formulas = list(),
    n_conditions_per_chunk,
    n_conditions,
    n_trials_per_condition,
    n_items,
    max_reached = n_items,
    max_t,
    dt = 0.001,
    noise_mechanism = "add",
    noise_factory = NULL,
    model = "ddm",
    parallel = FALSE,
    n_cores = NULL,
    rand_seed = NULL) {
  # default noise factory
  if (is.null(noise_factory)) {
    noise_factory <- function(condition_setting) {
      function(n, dt) rep(0, n)
    }
  }

  # default number of cores
  if (parallel && is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
    if (n_cores < 1) {
      n_cores <- 1
    }
  }

  # default chunk size
  if (is.null(n_conditions_per_chunk)) {
    if (parallel) {
      n_partitions <- ceiling(sqrt(n_conditions))
      n_partitions <- max(n_cores, min(n_partitions, n_cores * 10))
      n_conditions_per_chunk <- ceiling(n_conditions / n_partitions)
    } else {
      n_conditions_per_chunk <- n_conditions
    }
  }

  # default random seed for parallel processing
  if (parallel && is.null(rand_seed)) {
    rand_seed <- sample.int(.Machine$integer.max, 1)
  }

  # Create configuration list
  config <- list(
    prior_formulas = prior_formulas,
    between_trial_formulas = between_trial_formulas,
    item_formulas = item_formulas,
    n_conditions_per_chunk = n_conditions_per_chunk,
    n_conditions = n_conditions,
    n_trials_per_condition = n_trials_per_condition,
    n_items = n_items,
    max_reached = max_reached,
    max_t = max_t,
    dt = dt,
    noise_mechanism = noise_mechanism,
    noise_factory = noise_factory,
    model = model,
    parallel = parallel,
    n_cores = n_cores,
    rand_seed = rand_seed
  )

  # Validate the configuration
  validate_simulation_config(config)

  # Create S3 object
  structure(config, class = "multieam_simulation_config")
}

validate_simulation_config <- function(config) {
  # Validate required fields exist (following function signature order)
  required_fields <- c(
    "prior_formulas",
    "between_trial_formulas",
    "item_formulas",
    "n_conditions_per_chunk",
    "n_conditions",
    "n_trials_per_condition",
    "n_items",
    "max_reached",
    "max_t",
    "dt",
    "noise_mechanism",
    "noise_factory",
    "model",
    "parallel",
    "n_cores",
    "rand_seed"
  )

  missing_fields <- setdiff(required_fields, names(config))
  if (length(missing_fields) > 0) {
    stop(
      "Missing required configuration fields: ",
      paste(missing_fields, collapse = ", ")
    )
  }

  # Validate formulas are lists
  formula_params <- c(
    "prior_formulas",
    "between_trial_formulas",
    "item_formulas"
  )
  for (param_name in formula_params) {
    if (!is.list(config[[param_name]])) {
      stop(param_name, " must be a list")
    }
  }

  # Validate numeric parameters (following signature order)
  numeric_params <- c(
    "n_conditions_per_chunk",
    "n_conditions",
    "n_trials_per_condition",
    "n_items",
    "max_reached",
    "max_t",
    "dt"
  )

  for (param_name in numeric_params) {
    param_value <- config[[param_name]]
    if (!is.numeric(param_value) ||
      length(param_value) != 1 ||
      is.na(param_value)
    ) {
      stop(param_name, " must be a single numeric value")
    }
  }

  # Validate positive integers (following signature order)
  positive_int_params <- c(
    "n_conditions_per_chunk",
    "n_conditions",
    "n_trials_per_condition",
    "n_items"
  )
  for (param_name in positive_int_params) {
    param_value <- config[[param_name]]
    if (param_value <= 0 || param_value != floor(param_value)) {
      stop(param_name, " must be a positive integer")
    }
  }

  # Validate max_reached
  if (config$max_reached <= 0 ||
    config$max_reached != floor(config$max_reached)
  ) {
    stop("max_reached must be a positive integer")
  }
  if (config$max_reached > config$n_items) {
    stop("max_reached cannot be greater than n_items")
  }

  # Validate positive numeric values
  positive_params <- c("max_t", "dt")
  for (param_name in positive_params) {
    param_value <- config[[param_name]]
    if (param_value <= 0) {
      stop(param_name, " must be positive")
    }
  }

  # Validate noise mechanism
  valid_noise_mechanisms <- c("add", "mult", "mult_evidence", "mult_t")
  if (!config$noise_mechanism %in% valid_noise_mechanisms) {
    stop(
      "noise_mechanism must be one of: ",
      paste(valid_noise_mechanisms, collapse = ", "),
      ". Got: ", config$noise_mechanism
    )
  }

  # Validate noise_factory is a function
  if (!is.function(config$noise_factory)) {
    stop("noise_factory must be a function")
  }

  # Validate model
  valid_models <- c("ddm", "ddm-2b", "lca-gi")
  if (!config$model %in% valid_models) {
    stop("model must be one of: ", paste(valid_models, collapse = ", "))
  }

  # Validate boolean parameters
  if (!is.logical(config$parallel) ||
    length(config$parallel) != 1 ||
    is.na(config$parallel)
  ) {
    stop("parallel must be a single logical value (TRUE or FALSE)")
  }

  # Validate parallel-specific parameters
  if (config$parallel) {
    if (!is.null(config$n_cores)) {
      if (!is.numeric(config$n_cores) ||
        length(config$n_cores) != 1 ||
        is.na(config$n_cores) ||
        config$n_cores <= 0 ||
        config$n_cores != floor(config$n_cores)
      ) {
        stop("n_cores must be a positive integer when specified")
      }
    }

    if (!is.null(config$rand_seed)) {
      if (
        !is.numeric(config$rand_seed) ||
          length(config$rand_seed) != 1 ||
          is.na(config$rand_seed) ||
          config$rand_seed != floor(config$rand_seed)
      ) {
        stop("rand_seed must be an integer when specified")
      }
    }
  }

  invisible(config)
}

#' Print method for multieam simulation configuration
#'
#' @param x A multieam_simulation_config object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the input object
#' @export
print.multieam_simulation_config <- function(x, ...) {
  cat("Multieam Simulation Configuration\n")
  cat("=================================\n")
  cat("Model:", x$model, "\n")
  cat("Conditions:", x$n_conditions, "\n")
  cat("Trials per condition:", x$n_trials_per_condition, "\n")
  cat("Items per trial:", x$n_items, "\n")
  cat("Max reached:", x$max_reached, "\n")
  cat("Max time:", x$max_t, "\n")
  cat("Time step:", x$dt, "\n")
  cat("Noise mechanism:", x$noise_mechanism, "\n")
  if (!is.null(x$n_conditions_per_chunk)) {
    cat("Conditions per chunk:", x$n_conditions_per_chunk, "\n")
  }
  if (!is.null(x$parallel)) {
    cat("Parallel:", x$parallel, "\n")
  }
  if (!is.null(x$n_cores)) {
    cat("Number of cores:", x$n_cores, "\n")
  }

  # Show formula counts
  cat("\nFormulas:\n")
  cat("  Prior formulas:", length(x$prior_formulas), "\n")
  cat("  Between-trial formulas:", length(x$between_trial_formulas), "\n")
  cat("  Item formulas:", length(x$item_formulas), "\n")

  invisible(x)
}
