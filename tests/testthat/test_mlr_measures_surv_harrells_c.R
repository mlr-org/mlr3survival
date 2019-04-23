context("harrells_c")

test_that("harrells_c", {
  m = mlr_measures$get("harrells_c")
  expect_measure(m)
})
