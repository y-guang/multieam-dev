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

deault_noise_func <- function(n, dt) {
  rnorm(n, mean = 0, sd = sqrt(dt))
}

run_steps <- function(
    A,             # thresholds, length <= max_reached
    V,             # drift rates, length = n_items
    ndt,           # non-decision times, length = n_items
    dt,            # time step
    max_reached,   # max items recalled
    max_t,         # max simulation time
    noise_mechanism = "add",
    noise_func = deault_noise_func    # function to generate noise
) {
  n_items <- length(V)
  stopifnot(length(A) <= n_items)
  stopifnot(max_reached > 0 && max_reached <= n_items)
  stopifnot(dt > 0 && max_t > 0)

  V[V<0] <- 1e-8

  evidence <- rep(0, n_items)
  reached <- rep(FALSE, n_items)

  # Pre-allocate arrays for better performance
  words <- integer(max_reached)
  rts <- numeric(max_reached)
  n_recalled <- 0L
  t <- 0

  while (n_recalled < max_reached && t <= max_t) {
    recall_idx <- n_recalled + 1L
    A_current <- A[recall_idx]

    t <- t + dt
    if (noise_mechanism == "add") {
      noise <- noise_func(n_items, dt)
      evidence <- evidence + V * dt + noise
    } else if (noise_mechanism == "mult") {
      noise <- noise_func(n_items, dt)
      evidence <- evidence * noise + V * dt
    } else {
      stop('noise_mechanism must be "add" or "mult"')
    }

    just_cross <- which(!reached & (evidence >= A_current))
    if (length(just_cross) > 0) {
      w <- just_cross[sample.int(length(just_cross), 1)]
      reached[w] <- TRUE
      n_recalled <- n_recalled + 1L
      words[n_recalled] <- w
      rts[n_recalled] <- t
    }
  }

  # Trim to actual length
  if (n_recalled > 0) {
    words <- words[1:n_recalled]
    rts <- rts[1:n_recalled] + ndt[words]
    pos_list <- seq_len(n_recalled)
  } else {
    words <- integer(0)
    rts <- numeric(0)
    pos_list <- integer(0)
  }

  return(list(words = words, rts = rts, pos = pos_list))
}