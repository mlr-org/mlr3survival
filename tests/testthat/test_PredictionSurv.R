context("PredictionSurv")

test_that("Construction", {
  task = mlr_tasks$get("lung")
  p = PredictionSurv$new(row_ids = task$row_ids, truth = task$truth(), risk = runif(task$nrow))
  expect_prediction_surv(p)
})

test_that("Internally constructed Prediction", {
  task = mlr_tasks$get("lung")
  lrn = mlr_learners$get("surv.rpart")
  p = lrn$train(task)$predict(task)
  expect_prediction_surv(p)
})

test_that("c", {
  task = mlr_tasks$get("lung")
  lrn = mlr_learners$get("surv.rpart")
  rr = resample(task, lrn, "cv3")

  preds = rr$predictions

  pred = do.call(c, preds)
  expect_prediction_surv(pred)

  dt = as.data.table(pred)
  expect_data_table(dt, nrow = task$nrow, ncol = 4L, any.missing = FALSE)
})
