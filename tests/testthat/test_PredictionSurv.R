context("PredictionSurv")

test_that("Construction", {
  task = mlr_tasks$get("lung")
  p = PredictionSurv$new(row_ids = task$row_ids, truth = task$truth(), risk = runif(task$nrow))
  expect_prediction_surv(p)
})

test_that("Internally constructed Prediction", {
  task = mlr_tasks$get("lung")
  lrn = mlr_learners$get("surv.rpart")
  e = Experiment$new(task, lrn)$train()$predict()
  p = e$prediction
  expect_prediction_surv(p)
})

test_that("c", {
  task = mlr_tasks$get("lung")
  lrn = mlr_learners$get("surv.rpart")
  rr = resample(task, lrn, "cv3")

  preds = map(rr$experiments(), "prediction")

  pred = do.call(c, map(rr$experiments(), "prediction"))
  expect_prediction_surv(pred)

  dt = as.data.table(pred)
  expect_data_table(dt, nrow = task$nrow, ncol = 4L, any.missing = FALSE)
})
