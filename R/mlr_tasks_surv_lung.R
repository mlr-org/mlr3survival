#' @title Lung Cancer Survival Task
#'
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
NULL

load_lung = function() {
  b = as_data_backend(load_dataset("lung", "survival"))
  b$hash = "_mlr3_survival_lung_"
  TaskSurv$new("lung", b, time = "time", status = "status")
}
