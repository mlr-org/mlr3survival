context("resampling")

test_that("train, predict, score", {
  task = mlr_tasks$get("lung")
  learner = mlr_learners$get("surv.rpart")

  e = Experiment$new(task, learner)
  e$train()
  e$predict()
  e$score()

  expect_experiment(e)
  expect_numeric(e$performance)
})
