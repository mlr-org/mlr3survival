generate_tasks.LearnerSurv = function(learner, N = 20L, ...) { # nolint

  real_time = round(1 + rexp(N, rate = 2) * 20, 1)
  cens_time = round(1 + rexp(N, rate = 3) * 20, 1)
  status = ifelse(real_time <= cens_time, 1L, 0L)
  obs_time = ifelse(real_time <= cens_time, real_time, cens_time)

  data = cbind(data.table::data.table(time = obs_time, event = status), generate_data(learner, N))
  task = mlr3survival::TaskSurv$new("proto", mlr3::as_data_backend(data), time = "time", event = "event")
  tasks = generate_generic_tasks(learner, task)

  # generate sanity task
  if (N %% 2 == 1) N = N + 1
  data = with_seed(100, {
    data.table::data.table(
    x = c(rep(0, N/2), rep(1, N/2)),
    unimportant = runif(N),
    time = c(rep(10, N/2), rep(100, N/2)),
    event = rbinom(N, 1, 0.9)
  )})
  tasks$sanity = mlr3survival::TaskSurv$new("sanity", mlr3::as_data_backend(data), time = "time", event = "event")
  tasks$sanity_reordered = tasks$sanity$clone(deep = TRUE)
  tasks$sanity_reordered$id = "sanity_reordered"

  tasks
}
registerS3method("generate_tasks", "LearnerSurv", generate_tasks.LearnerSurv)

sanity_check.PredictionSurv = function(prediction, ...) { # nolint
  # sanity check discrimination
  prediction$score() >= 0.6
}

registerS3method("sanity_check", "PredictionSurv", sanity_check.PredictionSurv)
