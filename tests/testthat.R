if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(mlr3survival)

  test_check("mlr3survival")
}
