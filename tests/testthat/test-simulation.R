# test evaluate_with_dt
test_that("evaluate_with_dt works with numeric", {
  test_formula <- list(
    a ~ 5
  )
  result <- multieam:::evaluate_with_dt(test_formula, data = list(), n = 1)
  expect_equal(result$a, 5)
})

test_that("evaluate_with_dt works with numeric vec", {
  test_formula <- list(
    a ~ 5
  )
  result <- multieam:::evaluate_with_dt(test_formula, data = list(), n = 7)
  expect_equal(length(result$a), 7)
  expect_true(all(result$a == 5))
})

test_that("evaluate_with_dt works with chain", {
  test_formula <- list(
    a ~ 1,
    b ~ a + 2
  )
  result <- multieam:::evaluate_with_dt(test_formula, data = list(), n = 1)
  expect_equal(result$a, 1)
  expect_equal(result$b, 3)
})