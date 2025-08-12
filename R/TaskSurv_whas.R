
#' @title Worcester Heart Attack Study (WHAS) Survival Task
#'
#' @name mlr_tasks_whas
#'
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id whas
#' @templateVar data whas
#' @template task
#' @template seealso_task
#'
#' @section Preprocessing:
#' - Columns `id`, `yrgrp`, and `dstat` are removed.
#' - Column `sex` is renamed to `sexF`, `lenfol` to `time`, and `fstat` to `status`.
#' - Target is total follow-up time from hospital admission.
NULL

load_whas = function() {
  data = load_dataset("whas", "mlr3proba")
  data[, c("id", "yrgrp", "dstat")] = NULL
  colnames(data)[2L] = "sexF"
  colnames(data)[10:11] = c("time", "status")

  b = as_data_backend(data)
  task = TaskSurv$new("whas", b, time = "time", event = "status",
                      label = "Worcester Heart Attack")
  b$hash = task$man = "mlr3proba::mlr_tasks_whas"

  task
}

#' @include aaa.R
tasks[["whas"]] =  load_whas
