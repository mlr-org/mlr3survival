#' @title Rats Survival Task
#'
#' @name mlr_tasks_rats
#' @format [R6::R6Class] inheriting from [TaskSurv].
#'
#' @section Usage:
#' ```
#' mlr_tasks$get("rats")
#' ```
#'
#'
#' @description
#' A survival task for the [survival::rats] data set.
load_rats = function() {
  b = as_data_backend(load_dataset("rats", "survival"))
  b$hash = "_mlr3_survival_rats_"
  TaskSurv$new("rats", b, time = "time", status = "status")
}
