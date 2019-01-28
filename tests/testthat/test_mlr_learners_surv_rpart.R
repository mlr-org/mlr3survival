context("mlr_learners_surv_rpart")

test_that("autotest", {
  learner = mlr_learners$get("surv.rpart")
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
