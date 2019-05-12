#' @title Rats Survival Task
#'
#' @usage NULL
#' @name mlr_tasks_rats
#' @format [R6::R6Class] inheriting from [TaskSurv].
#'
#' @section Usage:
#' ```
#' mlr_tasks$get("rats")
#' ```
#'
#' @description
#' A survival task for the [survival::rats] data set.
#' Columns have been converted to integer, column "sex" has been converted to a factor.
load_rats = function() {
  data = load_dataset("rats", "survival")
  data = map_at(data, c("rx", "time", "status"), as.integer)
  data$sex = factor(data$sex, levels = c("f", "m"))

  b = as_data_backend(data)
  b$hash = "_mlr3_survival_rats_"
  TaskSurv$new("rats", b, time = "time", status = "status")
}
