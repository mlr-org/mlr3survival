context("TaskSurv")

test_that("Task duplicates rows", {
  task = mlr_tasks$get("lung")
  expect_task_surv(task)
  expect_set_equal(extract_vars(task$formula())$rhs, ".")
})
