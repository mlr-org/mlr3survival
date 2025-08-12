#' @title German Breast Cancer Study Survival Task
#'
#' @name mlr_tasks_gbcs
#'
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id gbcs
#' @templateVar data gbcs
#' @template task
#' @template seealso_task
#'
#' @section Preprocessing:
#' - Column `id` and all date columns have been removed, as well as `rectime`
#' and `censrec`.
#' - Target columns (`survtime`, `censdead`) have been renamed to (`time`, `status`).
NULL

load_gbcs = function() {
  data = load_dataset("gbcs", "mlr3proba")
  data[, c("id", "diagdate", "recdate", "deathdate", "rectime", "censrec")] = NULL
  colnames(data)[9:10] = c("time", "status")

  b = as_data_backend(data)
  task = TaskSurv$new("gbcs", b, time = "time", event = "status", label = "German Breast Cancer")
  b$hash = task$man = "mlr3proba::mlr_tasks_gbcs"

  task
}


#' @include aaa.R
tasks[["gbcs"]] =  load_gbcs
