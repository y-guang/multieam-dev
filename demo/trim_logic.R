library(dplyr)
library(tidyr)

trims = seq(0, 0.2, by = 0.01)
lambda = 0.5

choose_best_trim <- function(
    flat_df,
    trims = seq(0, 0.2, by = 0.01),
    lambda = 0.02
) {
  flat_df %>%
    # filter out invalid rt
    filter(
      !is.na(rt) & rt > 0 & is.finite(rt)
    ) %>%
    # expand to grid format
    expand_grid (
      trim = trims
    ) %>%
    group_by(
      condition_idx,
      rank_idx,
      trim
    ) %>%
    mutate(
      qutile_upper = quantile(rt, probs = 1 - trim, na.rm = TRUE)
    ) %>%
    filter(
      rt < qutile_upper
    ) %>%
    filter(
      n() >= 2
    ) %>%
    mutate(
      rt_log = log(rt),
    ) %>%
    summarise(
      mu = mean(rt_log, na.rm = TRUE),
      sigma = sd(rt_log, na.rm = TRUE),
      ks_D = suppressWarnings(
        ks.test(
          rt, "plnorm", meanlog = mu, sdlog = sigma
        )$statistic
      ),
      n_used = n(),
      q_lower = 0,
      q_upper = first(qutile_upper),
      .groups = "drop_last" # note this
    ) %>%
    mutate(
      score = ks_D + lambda * trim,
    ) %>%
    rename(
      mu_hat = mu,
      sigma_hat = sigma,
    ) %>%
    arrange(score, trim) %>%
    slice(1) %>%
    ungroup()
}

best_trim <- choose_best_trim(flat_df = tidy_data)



