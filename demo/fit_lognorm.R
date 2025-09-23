fit_lognorm_per_input <- function(
    input,
    rt, # 秒
    N = 10, # 只保留 input ≤ N
    xlim_sec = 60, # 画图 x 轴上限（秒）
    use_auto = TRUE, # TRUE=自动搜索上尾截断；FALSE=固定截断比例
    trim_upper = 0.00, # 固定截断比例（模式A）
    trim_grid = seq(0, .20, .01), # 自动模式候选上尾比例（模式B）
    lambda = 0.02, # 自动模式对截断比例的惩罚系数
    density_n = 512, # 经验密度采样点数
    return_data = TRUE # 是否返回中间数据
    ) {
  # ---------------- 内部工具函数 ----------------
  choose_best_trim <- function(rt_vec, trim_grid, lambda) {
    rt_vec <- rt_vec[is.finite(rt_vec) & rt_vec > 0]
    n_total <- length(rt_vec)
    if (n_total < 2) {
      return(tibble(
        trim = NA_real_, mu_hat = NA_real_, sigma_hat = NA_real_,
        ks_D = NA_real_, score = Inf, n_used = n_total,
        q_lower = NA_real_, q_upper = NA_real_
      ))
    }

    purrr::map_dfr(trim_grid, function(p) {
      ql <- 0
      qu <- as.numeric(quantile(rt_vec, 1 - p, na.rm = TRUE))
      rt_trim <- rt_vec[rt_vec >= ql & rt_vec <= qu]
      n <- length(rt_trim)
      if (n < 2) {
        return(tibble(
          trim = p, mu_hat = NA_real_, sigma_hat = NA_real_,
          ks_D = Inf, score = Inf, n_used = n,
          q_lower = ql, q_upper = qu
        ))
      }
      lg <- log(rt_trim)
      s <- sd(lg)
      if (!is.finite(s) || s == 0) {
        return(tibble(
          trim = p, mu_hat = NA_real_, sigma_hat = NA_real_,
          ks_D = Inf, score = Inf, n_used = n,
          q_lower = ql, q_upper = qu
        ))
      }
      mu <- mean(lg)
      sigma <- s
      suppressWarnings({
        ks <- stats::ks.test(rt_trim, "plnorm", meanlog = mu, sdlog = sigma)
      })
      D <- unname(ks$statistic)
      tibble(
        trim = p, mu_hat = mu, sigma_hat = sigma,
        ks_D = D, score = D + lambda * p, n_used = n,
        q_lower = ql, q_upper = qu
      )
    }) %>%
      arrange(score, trim) %>%
      slice(1)
  }

  fixed_trim_fit <- function(rt_vec, trim_upper) {
    rt_vec <- rt_vec[is.finite(rt_vec) & rt_vec > 0]
    n_total <- length(rt_vec)
    qu <- as.numeric(quantile(rt_vec, 1 - trim_upper, na.rm = TRUE))
    rt_trim <- rt_vec[rt_vec <= qu]
    n <- length(rt_trim)
    if (n < 2) {
      return(tibble(
        trim = trim_upper, mu_hat = NA_real_, sigma_hat = NA_real_,
        ks_D = NA_real_, score = NA_real_, n_used = n,
        q_lower = 0, q_upper = qu
      ))
    }
    lg <- log(rt_trim)
    mu <- mean(lg)
    sigma <- sd(lg)
    suppressWarnings({
      ks <- stats::ks.test(rt_trim, "plnorm", meanlog = mu, sdlog = sigma)
    })
    tibble(
      trim = trim_upper, mu_hat = mu, sigma_hat = sigma,
      ks_D = unname(ks$statistic), score = NA_real_, n_used = n,
      q_lower = 0, q_upper = qu
    )
  }

  # ---------------- 数据清洗 ----------------
  df <- tibble(input = as.integer(input), rt = as.numeric(rt)) %>%
    filter(is.finite(rt), rt > 0, is.finite(input), input > 0, input <= N)

  if (nrow(df) == 0) {
    stop("没有可用数据：请检查 input 和 rt，或放宽 N。")
  }

  # ---------------- 分 input 拟合（自动/固定） ----------------
  fit_by_input <- df %>%
    group_by(input) %>%
    summarise(rt_list = list(rt), .groups = "drop") %>%
    mutate(fit = purrr::map(
      rt_list,
      ~ if (use_auto) {
        choose_best_trim(.x, trim_grid = trim_grid, lambda = lambda)
      } else {
        fixed_trim_fit(.x, trim_upper = trim_upper)
      }
    )) %>%
    dplyr::select(-rt_list) %>%
    unnest(fit) %>%
    filter(is.finite(mu_hat), is.finite(sigma_hat), sigma_hat > 0)

  if (nrow(fit_by_input) == 0) {
    stop("每个 input 的样本都不足以拟合（或 sigma=0）。")
  }

  # ---------------- 经验密度（基于截断后的样本） ----------------
  dens_emp <- df %>%
    inner_join(fit_by_input %>% dplyr::select(input, q_lower, q_upper), by = "input") %>%
    filter(rt >= q_lower, rt <= q_upper) %>%
    group_by(input) %>%
    group_modify(~ {
      if (nrow(.x) < 2) {
        return(tibble(rt = numeric(0), density_emp = numeric(0)))
      }
      d <- density(.x$rt, n = density_n, na.rm = TRUE)
      tibble(rt = d$x, density_emp = d$y)
    }) %>%
    ungroup()

  # ---------------- 拟合曲线（Lognormal，0~xlim 网格） ----------------
  dens_fit <- fit_by_input %>%
    rowwise() %>%
    mutate(
      rt = list(seq(1e-6, xlim_sec, length.out = 600)),
      density_fit = list(dlnorm(rt, meanlog = mu_hat, sdlog = sigma_hat))
    ) %>%
    tidyr::unnest(c(rt, density_fit)) %>%
    ungroup()

  # ---------------- 标签（截断比例、n、KS） ----------------
  lab_df <- fit_by_input %>%
    transmute(
      input,
      label = sprintf("trim=%.0f%%, n=%d, KS=%.3f", trim * 100, n_used, ks_D)
    ) %>%
    group_by(input) %>%
    summarise(label = dplyr::first(label), .groups = "drop") %>%
    mutate(x = xlim_sec * 0.65, y = Inf)

  # ---------------- 画图 ----------------
  p <- ggplot() +
    geom_line(data = dens_emp, aes(x = rt, y = density_emp), linewidth = 0.65) +
    geom_line(data = dens_fit, aes(x = rt, y = density_fit), linetype = 2, linewidth = 0.75) +
    facet_wrap(~input) +
    coord_cartesian(xlim = c(0, xlim_sec)) +
    geom_text(
      data = lab_df, aes(x = x, y = y, label = label),
      vjust = 1.2, hjust = 0, size = 3.3
    ) +
    labs(
      title = if (use_auto) {
        "每个 input 的 RT（秒）经验密度与 Lognormal 拟合（自动上尾截断）"
      } else {
        sprintf("每个 input 的 RT（秒）经验密度与 Lognormal 拟合（上尾截断 %.0f%%）", trim_upper * 100)
      },
      subtitle = "实线：截断后的经验密度；虚线：按 μ=mean(log RT), σ=sd(log RT) 的对数正态拟合",
      x = "RT (s)", y = "Density"
    ) +
    theme_minimal(base_size = 12)

  if (return_data) {
    return(list(
      plot = p,
      fits = fit_by_input, # 每个 input 的 mu_hat、sigma_hat、截断比例等
      dens_emp = dens_emp,
      dens_fit = dens_fit,
      labels = lab_df
    ))
  } else {
    return(p)
  }
}



trim_p <- 0.05 # 截掉最大的 5%

trimmed_with_stats <- flat_result %>%
  filter(!is.na(rt), rt > 0) %>%
  group_by(condition_idx, rank_idx) %>%
  mutate(
    n_total  = n(), # 分组原始样本数
    cutoff95 = quantile(rt, probs = 1 - trim_p, type = 7) # 95%分位阈值（按原始rt）
  ) %>%
  filter(rt <= cutoff95) %>% # 行级别截断，其他列都保留
  mutate(
    log_rt = log(rt), # 加log列
    n_used = n(), # 截断后样本数（窗口函数）
    mu_hat = mean(log_rt), # 组内log均值
    sigma_hat = sd(log_rt), # 组内log标准差
    pct_kept = n_used / n_total
  ) %>%
  ungroup() %>%
  arrange(condition_idx, rank_idx)
