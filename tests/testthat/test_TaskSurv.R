context("TaskSurv")

test_that("Task duplicates rows", {
  task = mlr_tasks$get("lung")
  expect_task(task)

  expect_is(task$truth(), "Surv")

  f = task$formula()
  expect_formula(f)
  expect_set_equal(extract_vars(f)$lhs, task$target_names)
  expect_set_equal(extract_vars(f)$rhs, task$feature_names)

  expect_is(task$survfit(), "survfit")
})