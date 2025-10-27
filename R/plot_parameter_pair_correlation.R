#' Plot parameter pair correlation
#'
#' Create a correlation matrix plot with scatter plots, correlations, and densities.
#'
#' @param data An object containing parameter estimates
#' @param ... Additional arguments passed to methods
#' @export
plot_parameter_pair_correlation <- function(data, ...) {
  UseMethod("plot_parameter_pair_correlation")
}

#' @export
plot_parameter_pair_correlation.cv4abc <- function(data, ...) {
  # check the parameters
  dots <- rlang::list2(...)
  interactive <- dots$interactive %||% FALSE
  dots$interactive <- rlang::zap()

  # Get parameter names
  param_names <- data$names$parameter.names
  n_params <- ncol(data$true)
  if (is.null(param_names)) {
    param_names <- paste0("param_", 1:n_params)
  }

  # Get tolerance names from data$estim
  tol_names <- names(data$estim)
  n_tols <- length(tol_names)
  
  # Define panel functions
  panel_cor <- function(x, y, digits = 3, cex.cor = NULL, ...) {
    usr <- graphics::par("usr"); on.exit(graphics::par(usr = usr))
    graphics::par(usr = c(0, 1, 0, 1))
    r <- stats::cor(x, y, use = "pairwise.complete.obs", method = "spearman")
    rtxt <- if (is.finite(r)) formatC(r, format = "f", digits = digits) else "NA"
    cex <- if (is.null(cex.cor)) 0.8 else cex.cor
    cex <- cex * (0.6 + 0.8 * abs(ifelse(is.finite(r), r, 0)))
    graphics::text(0.5, 0.5, rtxt, cex = cex,
                   col = ifelse(is.finite(r) && r >= 0, "blue3", "firebrick"))
  }
  
  panel_smooth_pts <- function(x, y, ...) {
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
    graphics::points(x, y,
                     pch = 16, cex = 0.7,
                     col = grDevices::adjustcolor("black", alpha.f = 0.6)
    )
    if (length(x) > 1) graphics::lines(stats::lowess(x, y), lwd = 2, col = "dodgerblue3")
  }
  
  panel_hist <- function(x, ...) {
    usr <- graphics::par("usr"); on.exit(graphics::par(usr = usr))
    graphics::par(usr = c(usr[1:2], 0, 1))
    x <- x[is.finite(x)]
    if (length(x) == 0) { graphics::box(); return(invisible()) }
    h <- graphics::hist(x, plot = FALSE)
    y <- if (max(h$counts) > 0) h$counts / max(h$counts) else h$counts
    graphics::rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], y,
                   col = "grey90", border = "white")
  }
  
  # Loop through each tolerance level
  for (i in 1:n_tols) {
    tol_name <- tol_names[i]
    
    # Get estimates for this tolerance
    estimates <- data$estim[[tol_name]]
    
    # Remove non-finite values
    base_keep <- apply(is.finite(estimates), 1, all)
    estimates <- estimates[base_keep, , drop = FALSE]
    
    # Set column names
    colnames(estimates) <- param_names
    
    # Create the pairs plot
    main_title <- sprintf("%s - Parameter Pair Correlations [n=%d]",
                         tol_name, nrow(estimates))
    
    graphics::pairs(estimates,
                    lower.panel = panel_smooth_pts,
                    upper.panel = panel_cor,
                    diag.panel  = panel_hist,
                    main = main_title
    )
    
    # interactive mode
    if (interactive && i < n_tols) {
      readline(prompt = "Press [Enter] to continue to the next tolerance...")
    }
  }
  
  invisible(NULL)
}