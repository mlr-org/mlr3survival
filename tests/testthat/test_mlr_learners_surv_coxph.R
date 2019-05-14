context("surv.coxph")

test_that("autotest", {
  learner = mlr_learners$get("surv.coxph")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)

  if (FALSE) {
    e = result$experiment
    p = e$prediction

    cindex(p$truth, p$risk)
    Hmisc::rcorr.cens(p$risk, p$truth)
  }
})
