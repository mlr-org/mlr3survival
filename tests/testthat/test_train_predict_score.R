context("resampling")

test_that("train, predict, score", {
  task = mlr_tasks$get("lung")
  learner = mlr_learners$get("surv.rpart")

  perf = learner$train(task)$predict(task)$score()
  expect_number(perf)
})
