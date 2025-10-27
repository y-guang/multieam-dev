#' Plot parameter recovery
#'
#' Plotting parameter recovery from cross-validation results.
#'
#' @param data An object containing recovery results
#' @param ... Additional arguments passed to methods
#' @export
plot_parameter_recovery <- function(data, ...) {
  UseMethod("plot_parameter_recovery")
}

theme_multieam <- ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(color = "black"),
    axis.ticks = ggplot2::element_line(color = "black")
  )

#' @export
plot_parameter_recovery.cv4abc <- function(data, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting. Please install it.")
  }
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("gridExtra package is required for plotting. Please install it.")
  }

  plot_per_parameter <- 2
  # check the parameters
  dots <- rlang::list2(...)
  n_rows <- dots$n_rows %||% 3
  dots$n_rows <- rlang::zap()
  n_cols <- (dots$n_cols %||% 1) * plot_per_parameter
  dots$n_cols <- rlang::zap()
  method <- dots$method %||% "lm"
  dots$method <- rlang::zap()
  formula <- dots$formula %||% (y ~ x)
  dots$formula <- rlang::zap()
  resid_tol <- dots$resid_tol %||% NULL
  dots$resid_tol <- rlang::zap()
  interactive <- dots$interactive %||% FALSE
  dots$interactive <- rlang::zap()

  # dim check
  n_tols <- length(data$tols)
  n_params <- ncol(data$true)
  plots_per_tol <- n_params * plot_per_parameter
  plots_per_page <- n_rows * n_cols

  # Get parameter names
  param_names <- data$names$parameter.names
  if (is.null(param_names)) {
    param_names <- paste0("param_", 1:n_params)
  }

  # Get tolerance names from data$estim
  tol_names <- names(data$estim)

  # Loop through each tolerance level
  for (i in 1:n_tols) {
    tol_name <- tol_names[i]

    # Get estimates for this tolerance
    estimates <- data$estim[[tol_name]]

    # Create list to store plots for this tolerance
    tol_plot_list <- list()
    plot_idx <- 1

    # Loop through each parameter
    for (j in 1:n_params) {
      param_name <- param_names[j]

      # Prepare data for plotting
      true_vals <- data$true[, j]
      est_vals <- estimates[, j]
      residuals <- est_vals - true_vals
      
      # Filter by residual tolerance if specified
      if (!is.null(resid_tol)) {
        threshold <- quantile(abs(residuals), resid_tol, na.rm = TRUE)
        keep_idx <- abs(residuals) <= threshold
        true_vals <- true_vals[keep_idx]
        est_vals <- est_vals[keep_idx]
        residuals <- residuals[keep_idx]
      }
      
      # Calculate correlation
      cor_value <- cor(true_vals, est_vals, use = "complete.obs")

      plot_df <- data.frame(
        true = true_vals,
        estimate = est_vals,
        residual = residuals
      )

      # Plot 1: Estimate vs True
      p1 <- ggplot2::ggplot(plot_df, ggplot2::aes(x = true, y = estimate)) +
        ggplot2::geom_point() +
        ggplot2::geom_abline(
          intercept = 0, 
          slope = 1, 
          linetype = "dashed", 
          color = "red", 
          alpha = 0.5
          ) +
        ggplot2::geom_smooth(
          method = method, 
          formula = formula, 
          se = FALSE, 
          color = scales::alpha("blue", 0.5), 
          alpha = 0.5, 
          linewidth = 0.8
          ) +
        ggplot2::labs(
          title = paste0(param_name),
          x = "True",
          y = "Estimated"
        ) +
        ggplot2::annotate(
          "text",
          x = -Inf,
          y = Inf,
          label = sprintf("r = %.4f", cor_value),
          hjust = -0.1,
          vjust = 1.5,
          size = 3
        ) +
        theme_multieam

      # Plot 2: Density of residuals (estimate - true)
      p2 <- ggplot2::ggplot(plot_df, ggplot2::aes(x = residual)) +
        ggplot2::geom_density(
          color = "blue",
        ) +
        ggplot2::geom_vline(xintercept = 0,           
        linetype = "dashed", 
          color = "red", 
          alpha = 0.5) +
        ggplot2::labs(
          title = paste0("Residuals"),
          x = "Estimate - True",
          y = "Density"
        ) +
        theme_multieam


      # Add plots to list for this tolerance
      tol_plot_list[[plot_idx]] <- p1
      tol_plot_list[[plot_idx + 1]] <- p2
      plot_idx <- plot_idx + plot_per_parameter
    }

    # Calculate pages needed for this tolerance
    n_pages_tol <- ceiling(plots_per_tol / plots_per_page)

    # Render pages for this tolerance
    for (page in 1:n_pages_tol) {
      start_idx <- (page - 1) * plots_per_page + 1
      end_idx <- min(page * plots_per_page, plots_per_tol)

      page_plots <- tol_plot_list[start_idx:end_idx]

      # Arrange plots for this page
      gridExtra::grid.arrange(
        grobs = page_plots,
        ncol = n_cols,
        nrow = n_rows,
        top = grid::textGrob(
          paste0(tol_name, " (page ", page, "/", n_pages_tol, ")"),
          gp = grid::gpar(fontsize = 16, fontface = "bold")
        )
      )

      # interactive mode
      if (interactive) {
        readline(prompt = "Press [Enter] to continue to the next page...")
      }
    }
  }

  invisible(NULL)
}
