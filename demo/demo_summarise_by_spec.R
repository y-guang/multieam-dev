# Demo of new summarise_by delayed evaluation syntax

library(multieam)

# Create sample data
set.seed(123)
df <- data.frame(
  condition_idx = rep(1:3, each = 100),
  item_idx = rep(1:10, 30),
  rt = rnorm(300, mean = 5, sd = 1)
)

##################################
# Old syntax (still works)
##################################
cat("=== Old syntax (still works) ===\n")
summary_pipe_old <- function(df) {
  summarise_by(
    df,
    .by = c("condition_idx"),
    rt_mean = mean(rt),
    rt_quantiles = quantile(rt, probs = c(0.1, 0.5, 0.9))
  ) +
    summarise_by(
      df,
      .by = c("condition_idx", "item_idx"),
      rt_mean = mean(rt),
      rt_quantiles = quantile(rt, probs = c(0.1, 0.5, 0.9))
    )
}

result_old <- summary_pipe_old(df)
print(head(result_old))

##################################
# New syntax (delayed evaluation)
##################################
cat("\n=== New syntax (delayed evaluation) ===\n")

# Define the summary spec without data
summary_pipe <- 
  summarise_by(
    .by = c("condition_idx"),
    rt_mean = mean(rt),
    rt_quantiles = quantile(rt, probs = c(0.1, 0.5, 0.9))
  ) +
  summarise_by(
    .by = c("condition_idx", "item_idx"),
    rt_mean = mean(rt),
    rt_quantiles = quantile(rt, probs = c(0.1, 0.5, 0.9))
  )

# Inspect the spec
print(summary_pipe)

# Apply it to data later
cat("\n=== Applying spec to data ===\n")
result_new <- apply_summarise_by_spec(summary_pipe, df)
print(head(result_new))

# Or use it as a function-like object
cat("\n=== Using spec as a function ===\n")
result_fn <- summary_pipe$apply(df)
print(head(result_fn))

##################################
# Compare results
##################################
cat("\n=== Comparison ===\n")
cat("Results are identical:", identical(result_old, result_new), "\n")
cat("Results are identical (fn):", identical(result_old, result_fn), "\n")
