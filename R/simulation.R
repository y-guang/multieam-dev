#' Helper to resolved defined symbols in our formulas
#'
#' This function evaluates an expression in a given environment.
#' @param expr An expression to evaluate
#' @param env An environment to evaluate the expression in
#' @param n The number of values to generate if the expression is a distribution
#' @return The evaluated value as it is, no assumption on its type
#' @keywords internal
resolve_symbol <- function(expr, env, n) {
  # evaluate expression once in the given environment
  val <- eval(expr, env = env)

  if (base::inherits(val, "distribution")) {
    out <- distributional::generate(val, n)[[1]]
    return(base::as.numeric(out))
  } else {
    return(val)
  }
}


#' Evaluate a list of formulas sequentially with data
#'
#' This function evaluates a list of formulas sequentially, allowing later
#' formulas to reference
#' @param formulas A list of formulas to evaluate
#' @param data A list of named values to use as the initial environment
#' @param n The number of values to generate for each formula
#' @return A named list of evaluated values with length n
#' @keywords internal
evaluate_with_dt <- function(formulas, data = list(), n) {
  # validate data
  if (!is.list(data)) {
    stop("Data must be a list.")
  }

  # prepare for evaluation
  lhs_names <- sapply(formulas, function(f) as.character(rlang::f_lhs(f)))
  rhs_exprs <- lapply(formulas, function(f) rlang::f_rhs(f))

  # new data list
  res_list <- vector("list", length(rhs_exprs))
  names(res_list) <- lhs_names
  env <- list2env(data, parent = baseenv())

  # sequential evaluation
  for (i in seq_along(rhs_exprs)) {
    val <- resolve_symbol(rhs_exprs[[i]], env = env, n)

    # input validation and recycling
    if (length(val) == 1) {
      val <- rep(val, n)
    } else if (length(val) != n) {
      stop(
        paste0(
          "The length of the evaluated result for '",
          lhs_names[i],
          "' must be either 1 or n (", n, ")."
        )
      )
    }

    # environment update
    env[[lhs_names[i]]] <- val

    res_list[[i]] <- val
  }

  res_list <- modifyList(data, res_list)

  for (i in seq_along(res_list)) {
    val <- res_list[[i]]
    len <- length(val)
    if (len == n) {
      next
    } else if (n %% len == 0) {
      res_list[[i]] <- rep(val, n / len)
    } else {
      stop(
        paste0(
          "The length of '",
          names(res_list)[i],
          "' must be either 1 or a multiple of n (",
          n,
          ").",
          "But got length ",
          len,
          "."
        )
      )
    }
  }

  return(res_list)
}

#' Run a single trial of the DDM simulation
#'
#' This function runs a single trial of the DDM simulation using the provided
#' item formulas and trial settings. It's a wrapper around the core C++ function
#' @param trial_setting A list of named values representing the trial settings
#' @param item_formulas A list of formulas defining the item parameters
#' @param n_items The number of items to simulate
#' @param max_reached The threshold for evidence accumulation
#' @param max_t The maximum time to simulate
#' @param dt The step size for each increment
#' @param noise_mechanism The noise mechanism to use ("add" or "mult")
#' @param noise_factory A function that takes trial_setting and returns a noise
#' function with signature function(n, dt)
#' @param trajectories Whether to return full output including trajectories.
#' @return A list containing the simulation results
#' @note After evaluation, parameters A, V, and ndt are expected to be
#' numeric vectors of length n_items. And they are matched by position. So,
#' the first element of A, V, and ndt corresponds to the first item, and so on.
#' @keywords internal
run_trial <- function(
    trial_setting,
    item_formulas,
    n_items,
    max_reached,
    max_t,
    dt,
    noise_mechanism,
    noise_factory,
    trajectories = FALSE) {
  # prepare
  item_params <- evaluate_with_dt(
    item_formulas,
    data = trial_setting,
    n = n_items
  )
  noise_fun <- noise_factory(trial_setting)

  sim_result <- accumulate_evidence_ddm(
    item_params$A,
    item_params$V,
    item_params$ndt,
    max_t,
    dt,
    max_reached,
    noise_mechanism,
    noise_fun
  )

  if (trajectories) {
    sim_result$.item_params <- item_params
  }

  sim_result
}


#' Run a given condition with multiple trials
#'
#' This function runs multiple trials for a given condition using the specified
#' @param condition_setting A list of named values representing the condition
#' settings
#' @param between_trial_formulas A list of formulas defining the between-trial
#' parameters
#' @param item_formulas A list of formulas defining the item parameters
#' @param n_trials The number of trials to simulate
#' @param n_items The number of items per trial
#' @param max_reached The threshold for evidence accumulation
#' @param max_t The maximum time to simulate
#' @param dt The step size for each increment
#' @param noise_mechanism The noise mechanism to use ("add" or "mult")
#' @param noise_factory A function that takes condition_setting and returns a
#' noise function with signature function(n, dt)
#' @param trajectories Whether to return full output including trajectories.
#' @return A list containing the simulation results and condition parameters
#' @keywords internal
run_condition <- function(
    condition_setting,
    between_trial_formulas,
    item_formulas,
    n_trials,
    n_items,
    max_reached,
    max_t,
    dt,
    noise_mechanism,
    noise_factory,
    trajectories = FALSE) {
  # prepare
  cond_params <- evaluate_with_dt(
    formulas = between_trial_formulas,
    data = condition_setting,
    n = n_trials
  )

  trial_params_list <- vector("list", n_trials)
  for (i in seq_len(n_trials)) {
    trial_params_list[[i]] <- lapply(cond_params, function(x) x[i])
  }

  # run trials
  cond_res <- lapply(
    trial_params_list,
    function(trial_setting) {
      run_trial(
        trial_setting = trial_setting,
        item_formulas = item_formulas,
        n_items = n_items,
        max_reached = max_reached,
        max_t = max_t,
        dt = dt,
        noise_mechanism = noise_mechanism,
        noise_factory = noise_factory,
        trajectories = trajectories
      )
    }
  )

  # Return a list containing both results and condition parameters
  return(list(
    result = cond_res,
    cond_params = cond_params
  ))
}


#' Run a full simulation across multiple conditions
#'
#' This function runs a complete simulation across multiple conditions, with
#' each condition having multiple trials and items. It uses the hierarchical
#' structure: prior -> condition -> trial -> item.
#' @param prior_formulas A list of formulas defining the prior parameters
#' for conditions
#' @param between_trial_formulas A list of formulas defining the between-trial
#' parameters
#' @param item_formulas A list of formulas defining the item parameters
#' @param n_condition The number of conditions to simulate
#' @param n_trial_per_condition The number of trials per condition
#' @param n_items The number of items per trial
#' @param max_reached The threshold for evidence accumulation
#' @param max_t The maximum time to simulate (default: 100)
#' @param dt The step size for each increment (default: 0.01)
#' @param noise_mechanism The noise mechanism to use ("add" or "mult", default:
#'  "add")
#' @param noise_factory A function that takes condition_setting and returns a
#' noise function with signature function(n, dt). Default returns zero noise.
#' @param trajectories Whether to return full output including trajectories
#' (default: FALSE)
#' @return A list containing the simulation results for all conditions
#' @export
run_simulation <- function(
    prior_formulas,
    between_trial_formulas = list(),
    item_formulas = list(),
    n_condition,
    n_trial_per_condition,
    n_items,
    max_reached = n_items,
    max_t = 100,
    dt = 0.01,
    noise_mechanism = "add",
    noise_factory = function(condition_setting) {
      function(n, dt) rep(0, n)
    },
    trajectories = FALSE) {
  # validate inputs
  if (!is.list(prior_formulas)) {
    stop("prior_formulas must be a list of formulas")
  }
  if (!is.list(between_trial_formulas)) {
    stop("between_trial_formulas must be a list of formulas")
  }
  if (!is.list(item_formulas)) {
    stop("item_formulas must be a list of formulas")
  }
  if (n_condition < 1) {
    stop("n_condition must be at least 1")
  }
  if (n_trial_per_condition < 1) {
    stop("n_trial_per_condition must be at least 1")
  }
  if (n_items < 1) {
    stop("n_items must be at least 1")
  }

  # generate condition parameters from prior formulas
  prior_params <- evaluate_with_dt(
    formulas = prior_formulas,
    data = list(),
    n = n_condition
  )

  # create condition settings list
  condition_params_list <- vector("list", n_condition)
  for (i in seq_len(n_condition)) {
    condition_params_list[[i]] <- lapply(prior_params, function(x) x[i])
  }

  # run each condition
  sim_results <- lapply(
    condition_params_list,
    function(condition_setting) {
      run_condition(
        condition_setting = condition_setting,
        between_trial_formulas = between_trial_formulas,
        item_formulas = item_formulas,
        n_trials = n_trial_per_condition,
        n_items = n_items,
        max_reached = max_reached,
        max_t = max_t,
        dt = dt,
        noise_mechanism = noise_mechanism,
        noise_factory = noise_factory,
        trajectories = trajectories
      )
    }
  )

  # Return a list containing both results and prior parameters
  return(sim_results)
}


#' Run a full simulation across multiple conditions in parallel
#'
#' This function runs a complete simulation across multiple conditions using
#' parallel processing. It splits the conditions into chunks and processes
#' each chunk on separate cores. Each condition has multiple trials and items.
#' It uses the hierarchical structure: prior -> condition -> trial -> item.
#' @param prior_formulas A list of formulas defining the prior parameters
#' for conditions
#' @param between_trial_formulas A list of formulas defining the between-trial
#' parameters
#' @param item_formulas A list of formulas defining the item parameters
#' @param n_condition The number of conditions to simulate
#' @param n_trial_per_condition The number of trials per condition
#' @param n_items The number of items per trial
#' @param max_reached The threshold for evidence accumulation (default: n_items)
#' @param max_t The maximum time to simulate (default: 100)
#' @param dt The step size for each increment (default: 0.01)
#' @param noise_mechanism The noise mechanism to use ("add" or "mult", default:
#'  "add")
#' @param noise_factory A function that takes condition_setting and returns a
#' noise function with signature function(n, dt). Default returns zero noise.
#' @param trajectories Whether to return full output including trajectories
#' (default: FALSE)
#' @param chunk The size of chunks to split conditions into for parallel
#' processing (default: ceiling(n_condition / cores))
#' @param cores The number of cores to use for parallel processing
#' (default: parallel::detectCores() - 1)
#' @return A list containing the simulation results for all conditions
#' @export
run_simulation_parallel <- function(
    prior_formulas,
    between_trial_formulas = list(),
    item_formulas = list(),
    n_condition,
    n_trial_per_condition,
    n_items,
    max_reached = n_items,
    max_t = 100,
    dt = 0.01,
    noise_mechanism = "add",
    noise_factory = function(condition_setting) {
      function(n, dt) rep(0, n)
    },
    trajectories = FALSE,
    chunk = NULL,
    n_cores = parallel::detectCores() - 1) {
  # validate inputs
  if (!is.list(prior_formulas)) {
    stop("prior_formulas must be a list of formulas")
  }
  if (!is.list(between_trial_formulas)) {
    stop("between_trial_formulas must be a list of formulas")
  }
  if (!is.list(item_formulas)) {
    stop("item_formulas must be a list of formulas")
  }
  if (n_condition < 1) {
    stop("n_condition must be at least 1")
  }
  if (n_trial_per_condition < 1) {
    stop("n_trial_per_condition must be at least 1")
  }
  if (n_items < 1) {
    stop("n_items must be at least 1")
  }
  if (n_cores < 1) {
    stop("cores must be at least 1")
  }

  # set default chunk size if not provided
  if (is.null(chunk)) {
    chunk <- ceiling(n_condition / n_cores)
  }

  # generate condition parameters from prior formulas
  prior_params <- evaluate_with_dt(
    formulas = prior_formulas,
    data = list(),
    n = n_condition
  )

  # split prior_params into chunks
  condition_indices <- seq_len(n_condition)
  chunk_indices <- split(condition_indices, ceiling(condition_indices / chunk))

  # create chunked prior parameters
  chunked_prior_params <- lapply(chunk_indices, function(indices) {
    lapply(prior_params, function(param) param[indices])
  })

  # function to process a single chunk
  process_chunk <- function(chunk_prior_params) {
    n_conditions_in_chunk <- length(chunk_prior_params[[1]])

    # create condition settings list for this chunk
    condition_params_list <- vector("list", n_conditions_in_chunk)
    for (i in seq_len(n_conditions_in_chunk)) {
      condition_params_list[[i]] <- lapply(chunk_prior_params, function(x) x[i])
    }

    # run each condition in this chunk
    chunk_results <- lapply(
      condition_params_list,
      function(condition_setting) {
        run_condition(
          condition_setting = condition_setting,
          between_trial_formulas = between_trial_formulas,
          item_formulas = item_formulas,
          n_trials = n_trial_per_condition,
          n_items = n_items,
          max_reached = max_reached,
          max_t = max_t,
          dt = dt,
          noise_mechanism = noise_mechanism,
          noise_factory = noise_factory,
          trajectories = trajectories
        )
      }
    )

    return(chunk_results)
  }

  # setup parallel cluster
  cl <- parallel::makeCluster(min(n_cores, length(chunked_prior_params)))
  on.exit(parallel::stopCluster(cl))

  # export necessary objects to cluster
  parallel::clusterExport(cl, c(
    "run_condition", "run_trial", "evaluate_with_dt",
    "resolve_symbol", "accumulate_evidence_ddm",
    "between_trial_formulas", "item_formulas", "n_trial_per_condition",
    "n_items", "max_reached", "max_t", "dt", "noise_mechanism",
    "noise_factory", "trajectories"
  ),
  envir = environment()
  )

  # run parallel processing with progress bar
  if (requireNamespace("pbapply", quietly = TRUE)) {
    parallel_results <- pbapply::pblapply(
      chunked_prior_params,
      process_chunk,
      cl = cl
    )
  } else {
    message("Install 'pbapply' package for progress bar support")
    parallel_results <- parallel::parLapply(
      cl,
      chunked_prior_params,
      process_chunk
    )
  }

  # combine results from all chunks (ensure unnamed list like serial version)
  sim_results <- unname(do.call(c, parallel_results))

  # Return the combined results
  return(sim_results)
}
