test_that("right censoring task creation", {
  expect_silent({
    task = TaskSurv$new("right", backend = survival::rats, time = "time", event = "status")
  })
  expect_equal(task$censtype, "right")
  expect_numeric(task$times())
  expect_integer(task$status())
  expect_equal(
    task$formula(), as.formula(Surv(time, status, type = "right") ~ .),
    ignore_attr = TRUE
  )
})
