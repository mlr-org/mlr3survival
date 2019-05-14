context("Experiment")

test_that("Experiment", {
  task = mlr_tasks$get("unemployment")
  learner = mlr_learners$get("surv.rpart")

  e = Experiment$new(task$clone()$filter(1:3000), learner)$train()
  newdata = remove_named(task$clone()$filter(3001:3343)$data(), task$target_names)
  e$predict(newdata = newdata)

  p = as.data.table(e$prediction)
  expect_data_table(p, nrow = 343)
  expect_true(allMissing(p$time))
  expect_true(allMissing(p$status))
  expect_set_equal(p$row_id, 3001:3343)
})
