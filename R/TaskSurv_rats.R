#' @title Rats Survival Task
#'
#' @name mlr_tasks_rats
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id rats
#' @templateVar data rats
#' @templateVar data_pkg survival
#' @template task
#' @template seealso_task
#'
#' @section Pre-processing:
#' - Column `sex` has been converted to a `factor`, all others have been
#' converted to `integer`.
NULL

load_rats = function() {
  data = survival::rats
  data = map_at(data, c("rx", "time", "status"), as.integer)
  data$sex = factor(data$sex, levels = c("f", "m"))

  b = as_data_backend(data)
  task = TaskSurv$new("rats", b, time = "time", event = "status", label = "Rats")
  b$hash = task$man = "mlr3survival::mlr_tasks_rats"

  task
}

#' @include aaa.R
tasks[["rats"]] =  load_rats
