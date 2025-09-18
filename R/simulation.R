# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


eval_once <- function(expr, env, n) {
  # evaluate expression once in the given environment
  val <- eval(expr, env = env)

  if (base::inherits(val, "distribution")) {
    out <- distributional::generate(val, n)[[1]]
    return(base::as.numeric(out))
  } else {
    return(val)
  }
}


evaluate_with_dt <- function(formulas, data = list(), n) {
  # validate data
  if (!is.list(data)) {
    stop("Data must be a list.")
  }

  # prepare for evaluation
  lhs_names <- sapply(formulas, function(f) as.character(rlang::f_lhs(f)))
  rhs_exprs <- lapply(formulas, function(f) rlang::f_rhs(f))
  env <- rlang::env(!!!data)

  # new data list
  res_list <- vector("list", length(rhs_exprs))
  names(res_list) <- lhs_names

  # sequential evaluation
  for (i in seq_along(rhs_exprs)) {
    val <- eval_once(rhs_exprs[[i]], env = data, n)

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

