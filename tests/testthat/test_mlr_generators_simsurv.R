context("mlr_generators_simsurv")

test_that("mlr_generators_simsurv", {
  gen = mlr_generators$get("simsurv")
  task = gen$generate(100)
  expect_task_surv(task)
})
