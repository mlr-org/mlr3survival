lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

expect_task_surv = function(task) {
  expect_is(task, "TaskSurv")
  expect_task(task)
  expect_task_supervised(task)
  expect_is(task$truth(), "Surv")

  f = task$formula()
  expect_formula(f)
  expect_set_equal(extract_vars(f)$lhs, task$target_names)
  expect_is(task$survfit(), "survfit")
}

generate_tasks.LearnerSurv = function(learner, N = 20L) {

  real.time = 1 + rexp(N, rate = 5) * 10
  cens.time = 1 + rexp(N, rate = 1) * 10
  status = ifelse(real.time <= cens.time, 1L, 0L)
  obs.time = ifelse(real.time <= cens.time, real.time, cens.time)

  data = cbind(data.table::data.table(time = obs.time, status = status), generate_data(learner, N))
  task = TaskSurv$new("proto", mlr3::as_data_backend(data), time = "time", status = "status")
  tasks = generate_generic_tasks(learner, task)

  # generate sanity task
  set.seed(100)
  data = data.table::data.table(time = obs.time, status = status, x1 = real.time + rnorm(N, sd = 0.1))
  data$unimportant = runif(nrow(data))
  task = mlr3misc::set_names(list(TaskSurv$new("sanity", mlr3::as_data_backend(data), time = "time", status = "status")), "sanity")
  tasks = c(tasks, task)
}
registerS3method("generate_tasks", "LearnerSurv", generate_tasks.LearnerSurv)

sanity_check.PredictionSurv = function(prediction) {
  prediction$score() >= 0.6
}
registerS3method("sanity_check", "PredictionSurv", sanity_check.PredictionSurv)

expect_prediction_surv = function(p) {
  expect_prediction(p)
  expect_is(p, "PredictionSurv")
}
