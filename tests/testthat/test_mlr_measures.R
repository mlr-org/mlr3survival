context("mlr_measures")

test_that("mlr_measures", {
  task = mlr_tasks$get("lung")
  keys = mlr_measures$keys("^surv")

  for (key in keys) {
    m = mlr_measures$get(key)
    expect_measure(m)
    task$measures = list(m)

    e = Experiment$new(task, "surv.rpart")
    e$train()$predict()$score()
    expect_number(e$performance, na.ok = m$na_score)
  }
})
