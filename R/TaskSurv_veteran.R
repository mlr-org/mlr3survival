#' @title Veteran Survival Task
#'
#' @name mlr_tasks_veteran
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id veteran
#' @templateVar data veteran
#' @templateVar data_pkg survival
#' @template task
#' @template seealso_task
#'
#' @section Pre-processing:
#' - Columns `age`, `time`, `status`, `diagtime` and `karno` have been converted
#' to `integer`.
#' - Columns `trt`, `prior` have been converted to `factor`s. Prior therapy
#' values are `no`/`yes` instead of 0/10.
NULL

load_veteran = function() {
  data = survival::veteran
  data = map_at(data, c("age", "time", "status", "diagtime", "karno"), as.integer)
  data = map_at(data, c("trt", "prior"), as.factor)
  data$trt = factor(data$trt, levels = c("1", "2"))
  data$prior = factor(ifelse(data$prior == 0, "no", "yes"), levels = c("no", "yes"))

  b = as_data_backend(data)
  task = TaskSurv$new("veteran", b, time = "time", event = "status", label = "Veteran")
  b$hash = task$man = "mlr3survival::mlr_tasks_veteran"

  task
}

#' @include aaa.R
tasks[["veteran"]] =  load_veteran
