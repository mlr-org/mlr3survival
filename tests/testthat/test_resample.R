context("resampling")

test_that("resampling works", {
  task = mlr_tasks$get("lung")
  learner = mlr_learners$get("surv.rpart")
  rr = resample(task, learner, mlr_resamplings$get("cv"))
  expect_resample_result(rr)
})
