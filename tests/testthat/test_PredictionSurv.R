set.seed(1L)
task = tsk("rats")$filter(sample(300, 20L))

test_that("Construction", {
  p = PredictionSurv$new(row_ids = task$row_ids, truth = task$truth(), crank = runif(task$nrow))
  expect_prediction_surv(p)
})

test_that("Construction from predict()", {
  learner = lrn("surv.kaplan")
  p = learner$train(task)$predict(task)
  expect_prediction_surv(p)
})

test_that("combining predictions", {
  lrn = lrn("surv.kaplan")
  set.seed(1L)
  resampling = rsmp("cv", folds = 2L)
  rr = resample(task, lrn, resampling)
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
    matrix(data = rep(0.3, 10), ncol = 1L, dimnames = list(NULL, 108))
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
  expect_length(c(p1, p2, keep_duplicates = TRUE)$row_ids, 40L)
  expect_length(c(p1, p2, keep_duplicates = FALSE)$row_ids, 20L)
  preds = list(p1, p2)
  p12 = do.call(c, preds)
  expect_matrix(p12$data$distr) # combination is a matrix
  expect_equal(colnames(p12$data$distr), colnames(p1$data$distr)) # same time points
})

skip("implement surv.coxph first")
test_that("data.frame roundtrip", {
  p1 = suppressWarnings(lrn("surv.coxph")$train(task)$predict(task))

  tab = as.data.table(p1)
  expect_class(tab$distr, "list")
  expect_equal(length(tab$distr), 20)
  for (i in 1:20) {
    # 1 vector (survival probabilities) per observation, wrapped in a list
    expect_list(tab$distr[[i]], len = 1)
    expect_vector(tab$distr[[i]][[1L]], size = ncol(p1$data$distr))
  }

  p2 = as_prediction_surv(tab)
  expect_prediction_surv(p2)

  expect_equal(
    suppressWarnings(as.data.table(p1$distr$parameters())$value),
    suppressWarnings(as.data.table(p2$distr$parameters())$value)
  )

  expect_equal(as.data.table(p1)[, -6L], as.data.table(p2)[, -6L])
})

test_that("as_prediction_surv", {
  p = suppressWarnings(lrn("surv.coxph")$train(task))$predict(task)
  expect_prediction_surv(as_prediction_surv(as.data.table(p)))
})

test_that("filtering", {
  p = suppressWarnings(lrn("surv.coxph")$train(task)$predict(task)) # survival matrix
  p2 = reshape_distr_to_3d(p) # survival array
  p3 = p$clone()
  p4 = p2$clone()
  p3$data$distr = p3$distr # Matdist
  p4$data$distr = p4$distr # Arrdist

  p$filter(c(20, 37, 42))
  p2$filter(c(20, 37, 42))
  p3$filter(c(20, 37, 42))
  p4$filter(c(20, 37, 42))
  expect_prediction_surv(p)
  expect_prediction_surv(p2)
  expect_prediction_surv(p3)
  expect_prediction_surv(p4)

  expect_setequal(p$data$row_ids, c(20, 37, 42))
  expect_setequal(p2$data$row_ids, c(20, 37, 42))
  expect_setequal(p3$data$row_ids, c(20, 37, 42))
  expect_setequal(p4$data$row_ids, c(20, 37, 42))
  expect_numeric(p$data$crank, any.missing = FALSE, len = 3L)
  expect_numeric(p2$data$crank, any.missing = FALSE, len = 3L)
  expect_numeric(p3$data$crank, any.missing = FALSE, len = 3L)
  expect_numeric(p4$data$crank, any.missing = FALSE, len = 3L)
  expect_numeric(p$data$lp, any.missing = FALSE, len = 3L)
  expect_numeric(p2$data$lp, any.missing = FALSE, len = 3L)
  expect_numeric(p3$data$lp, any.missing = FALSE, len = 3L)
  expect_numeric(p4$data$lp, any.missing = FALSE, len = 3L)
  expect_matrix(p$data$distr, nrows = 3L)
  expect_array(p2$data$distr, d = 3)
  expect_identical(nrow(p2$data$distr), 3L)
  expect_r6(p3$data$distr, "Matdist")
  expect_r6(p4$data$distr, "Arrdist")

  # edge case: filter to 1 observation
  p$filter(20)
  p2$filter(20)
  p3$filter(20)
  p4$filter(20)
  expect_prediction_surv(p)
  expect_prediction_surv(p2)
  expect_prediction_surv(p3)
  expect_prediction_surv(p4)
  expect_matrix(p$data$distr, nrows = 1L)
  expect_array(p2$data$distr, d = 3)
  expect_identical(nrow(p2$data$distr), 1L)
  expect_r6(p3$data$distr, "WeightedDiscrete") # from Matdist!
  expect_r6(p4$data$distr, "Arrdist") # remains an Arrdist!

  # filter to 0 observations using non-existent (positive) id
  p$filter(42)
  p2$filter(42)
  p3$filter(42)
  p4$filter(42)

  expect_prediction_surv(p)
  expect_prediction_surv(p2)
  expect_prediction_surv(p3)
  expect_prediction_surv(p4)
  expect_null(p$distr)
  expect_null(p2$distr)
  expect_null(p3$distr)
  expect_null(p4$distr)
})
