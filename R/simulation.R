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

  return(res_list)
}

#' Run a single trial of the DDM simulation
#'
#' This function runs a single trial of the DDM simulation using the provided
#' item formulas and trial settings. It's a wrapper around the core C++ function
#' @param trial_setting A list of named values representing the trial settings
#' @param item_formulas A list of formulas defining the item parameters
#' @param n_items The number of items to simulate
#' @param dt The step size for each increment
#' @param max_reached The threshold for evidence accumulation
#' @param max_t The maximum time to simulate
#' @param noise_mechanism The noise mechanism to use ("add" or "mult")
#' @param noise_factory A function that takes trial_setting and returns a noise
#' function with signature function(n, dt)
#' @param trajectories Whether to return full output including trajectories.
#' @return A list containing the simulation results
#' @note After evaluation, parameters A, V, and ndt are expected to be
#' numeric vectors of length n_item. And they are matched by position. So,
#' the first element of A, V, and ndt corresponds to the first item, and so on.
#' @keywords internal
run_trial <- function(
    trial_setting,
    item_formulas,
    n_items,
    max_reached,
    dt,
    max_t,
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
    dt,
    max_reached,
    max_t,
    noise_mechanism,
    noise_fun
  )

  if (trajectories) {
    sim_result$.item_params <- item_params
  }

  sim_result
}


