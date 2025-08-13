#' @title GRACE 1000 Survival Task
#'
#' @name mlr_tasks_grace
#'
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id grace
#' @templateVar data grace
#' @template task
#' @template seealso_task
#'
#' @section Preprocessing:
#' - Column `id` is removed.
#' - Target columns (`days`, `death`) have been renamed to (`time`, `status`).
NULL

load_grace = function() {
  data = load_dataset("grace", "mlr3survival")
  data[, "id"] = NULL
  colnames(data)[1:2] = c("time", "status")

  b = as_data_backend(data)
  task = TaskSurv$new("grace", b, time = "time", event = "status", label = "GRACE 1000")
  b$hash = task$man = "mlr3survival::mlr_tasks_grace"

  task
}

#' @include aaa.R
tasks[["grace"]] =  load_grace
