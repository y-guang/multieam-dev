# obtain a basic flat_result
source("demo/demo_parallel.R")

# basic usage
rt_threshold <- find_lognormal_trim_threshold(flat_result) # auto parallel
cleaned_result <- apply_trim(flat_result, rt_threshold)

# additional arguments
rt_threshold <- find_lognormal_trim_threshold(
  flat_result,
  trims = seq(0, 0.2, by = 0.01),
  lambda = 0.02,
  parallel = TRUE,
  chunk_size = 80,
  n_cores = parallel::detectCores() - 1
)
cleaned_result <- apply_trim(
  flat_result,
  rt_threshold,
  min_n_used = 10
)
