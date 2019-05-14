#' @title Lung Cancer Survival Task
#'
#' @usage NULL
#' @name mlr_tasks_lung
#' @format [R6::R6Class] inheriting from [TaskSurv].
#'
#' @section Usage:
#' ```
#' mlr_tasks$get("lung")
#' ```
#'
#' @description
#' A survival task for the [survival::lung] data set.
#' Columns have been converted to integer, column "sex" has been converted to a factor.
load_lung = function() {
  data = load_dataset("lung", "survival")
  data = map_dtc(data, as.integer)
  data$status = (data$status == 2L)
  data$sex = factor(ifelse(data$sex == 1L, "m", "f"), levels = c("f", "m"))

  b = as_data_backend(data)
  b$hash = "_mlr3_survival_lung_"
  TaskSurv$new("lung", b, time = "time", status = "status")
}
