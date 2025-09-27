W <- matrix(c(
  0, 0,
  0, 0
), nrow = 2)
A <- c(5, 10)
V <- c(2, 1)
ndt <- c(0, 0)

accumulate_evidence_lca(
  W,
  A,
  V,
  ndt,
  100,
  0.01,
  0.5,
  1,
  2,
  function(n, dt) rnorm(n, 0, sqrt(dt))
)
