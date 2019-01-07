#' @title Survival Measure
#'
#' @name MeasureSurv
#' @format [R6Class] object inheriting from [Measure].
#' @description
#' This task specializes [Measure] for survival analysis problems.
#'
#' @section Usage:
#' See [Measure].
#'
#' @section Details:
#' `$task_type` is `"surv"`.
NULL

#' @export
MeasureSurv = R6Class("MeasureSurv", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize, predict_type = "risk", task_properties = character(0L), packages = character(0L)) {
      super$initialize(id, task_type = "surv", range = range, minimize = minimize, predict_type = predict_type,
        task_properties = task_properties, packages = packages)
    }
  )
)
