

#' @title Lung Cancer Survival Task
#'
#' @name mlr_tasks_lung
#'
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id lung
#' @templateVar data lung
#' @templateVar data_pkg survival
#' @template task
#' @template seealso_task
#'
#' @section Pre-processing:
#' - Column `inst` has been removed.
#' - Column `sex` has been converted to a `factor`, all others have been
#' converted to `integer`.
#' - Kept only complete cases (no missing values).
NULL

load_lung = function() {
  data = survival::lung
  data$inst = NULL
  data = map_dtc(data, as.integer)
  data$status = as.integer(data$status == 2L)
  data$sex = factor(ifelse(data$sex == 1L, "m", "f"), levels = c("f", "m"))
  data = stats::na.omit(data)

  b = as_data_backend(data)
  task = TaskSurv$new("lung", b, time = "time", event = "status", label = "Lung Cancer")
  b$hash = task$man = "mlr3survival::mlr_tasks_lung"

  task
}

#' @include aaa.R
tasks[["lung"]] =  load_lung
