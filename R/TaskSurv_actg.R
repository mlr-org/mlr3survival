#' @title ACTG 320 Survival Task
#'
#' @name mlr_tasks_actg
#'
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id actg
#' @templateVar data actg
#' @template task
#' @template seealso_task
#'
#' @section Pre-processing:
#' - Column `sex` has been renamed to `sexF` and `censor` has been renamed to `status`.
#' - Columns `id`, `time_d`, and `censor_d` have been removed so target is `time`
#' to AIDS diagnosis (in days).
NULL

load_actg = function() {
  data = load_dataset("actg", "mlr3survival")
  data[, c("id", "time_d", "censor_d")] = NULL
  colnames(data)[6L] = "sexF"
  colnames(data)[2L] = "status"

  b = as_data_backend(data)
  task = TaskSurv$new("actg", b, time = "time", event = "status", label = "ACTG 320")
  b$hash = task$man = "mlr3survival::mlr_tasks_actg"

  task
}

#' @include aaa.R
tasks[["actg"]] =  load_actg
