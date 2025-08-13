task = tsk("lung")
suppressWarnings({
  p_cox = lrn("surv.coxph")$train(task)$predict(task)
})

test_that("Construction", {
  p = PredictionSurv$new(row_ids = task$row_ids, truth = task$truth(), crank = runif(task$nrow))
  expect_prediction_surv(p)
})

test_that("Construction from predict()", {
  expect_prediction_surv(p_cox)
})

test_that("combining predictions", {
  learner = lrn("surv.kaplan")
  resampling = rsmp("cv", folds = 2L)
  rr = resample(task, learner, resampling)
  expect_resample_result(rr)

  preds = rr$predictions()

  # combining survival matrices
  distr1 = preds[[1L]]$data$distr
  distr2 = preds[[2L]]$data$distr
  times1 = as.numeric(colnames(distr1))
  times2 = as.numeric(colnames(distr2))

  pred = do.call(c, preds)
  expect_prediction_surv(pred)
  surv_mat = pred$data$distr
  expect_matrix(surv_mat)
  # check that time points are properly combined
  times = as.numeric(colnames(surv_mat))
  expect_true(all(times == sort(union(times1, times2))))

  # data.table conversion
  dt = as.data.table(pred)
  expect_data_table(dt, nrows = task$nrow, ncols = 5L, any.missing = FALSE)

  # add extra time point on the 2nd prediction object
  preds2 = rr$predictions()
  preds2[[2L]]$data$distr = cbind(
    distr2,
    matrix(data = rep(0.3, task$nrow / 2), ncol = 1L, dimnames = list(NULL, 108))
  )
  distr1 = preds2[[1L]]$data$distr
  distr2 = preds2[[2L]]$data$distr
  times1 = as.numeric(colnames(distr1))
  times2 = as.numeric(colnames(distr2))

  pred2 = do.call(c, preds2)
  expect_prediction_surv(pred2)
  surv_mat2 = pred2$data$distr
  expect_matrix(surv_mat2)
  # check that time points are properly combined and extra time point has been added
  times = as.numeric(colnames(surv_mat2))
  expect_in(108, times)
  expect_true(all(times == sort(union(times1, times2))))

  # combining predictions with exactly the same time points
  p1 = lrn("surv.kaplan")$train(task)$predict(task)
  p2 = p1$clone()
  expect_length(c(p1, p2, keep_duplicates = TRUE)$row_ids, task$nrow * 2)
  expect_length(c(p1, p2, keep_duplicates = FALSE)$row_ids, task$nrow)
  preds = list(p1, p2)
  p12 = do.call(c, preds)
  expect_matrix(p12$data$distr) # combination is a matrix
  expect_equal(colnames(p12$data$distr), colnames(p1$data$distr)) # same time points
})

test_that("data.table roundtrip", {
  tab = as.data.table(p_cox)
  expect_class(tab$distr, "list")
  expect_equal(length(tab$distr), task$nrow)
  for (i in 1:task$nrow) {
    # 1 numeric vector (survival probabilities) per observation, wrapped in a list
    expect_list(tab$distr[[i]], len = 1)
    expect_numeric(tab$distr[[i]][[1L]], len = ncol(p_cox$data$distr), any.missing = FALSE)
  }

  p = as_prediction_surv(tab)
  expect_prediction_surv(p)
  expect_equal(p_cox$distr$data, p$distr$data)
  expect_equal(as.data.table(p_cox)[, -6L], as.data.table(p)[, -6L])
})

test_that("filtering", {
  p = p_cox$clone()
  ids = sample(task$row_ids, 3)

  # filter to 3 random observations
  p_cox$filter(ids)
  expect_prediction_surv(p_cox)
  expect_setequal(p_cox$data$row_ids, ids)
  assert_surv(p_cox$data$truth, len = 3, any.missing = FALSE)
  expect_numeric(p_cox$data$crank, any.missing = FALSE, len = 3L)
  expect_numeric(p_cox$data$lp, any.missing = FALSE, len = 3L)
  expect_matrix(p_cox$data$distr, nrows = 3L)
  expect_equal(p_cox$data$data_type, "surv")
  expect_equal(p_cox$data$interp_meth, "const_surv")

  # edge case: filter to 1 observation
  p$filter(ids[2])
  expect_prediction_surv(p)
  expect_setequal(p$data$row_ids, ids[2])
  assert_surv(p$data$truth, len = 1, any.missing = FALSE)
  expect_numeric(p$data$crank, any.missing = FALSE, len = 1L)
  expect_numeric(p$data$lp, any.missing = FALSE, len = 1L)
  expect_matrix(p$data$distr, nrows = 1L)
  expect_equal(p$data$data_type, "surv")
  expect_equal(p$data$interp_meth, "const_surv")

  # filter to 0 observations using non-existent id
  p$filter(-42)
  expect_prediction_surv(p)
  expect_length(p$data$row_ids, 0)
  assert_surv(p$data$truth, len = 0)
  expect_numeric(p$data$crank, len = 0)
  expect_numeric(p$data$lp, len = 0)
  expect_matrix(p$data$distr, nrows = 0, ncols = 0)
  expect_equal(p$data$data_type, "surv")
  expect_equal(p$data$interp_meth, "const_surv")
})
