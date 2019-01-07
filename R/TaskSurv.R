#' @title Survival Task
#'
#' @name TaskSurv
#' @format [R6Class] object inheriting from [mlr3::Task]/[mlr3::TaskSupervised].
#' @description
#' This task specializes [mlr3::Task] and [mlr3::TaskSupervised] for right-censored survival problems.
#'
#' @section Usage:
#' Inherits from [mlr3::Task]/[mlr3::TaskSupervised].
#' ```
#' # Construction
#' t = TaskSupervised$new(id, backend, time, status)
#'
#' @section Arguments:
#' * `time` (`numeric()`):
#'   Time of event.
#' * `status` (`numeric()`):
#'   Event indicator. 0 means "alive" (no event), 1 means dead (event).
#'
#' @examples
#' library(mlr3)
#' data("lung", package = "survival")
#' b = as_data_backend(lung)
#' task = TaskSurv$new("lung", backend = b, time = "time", status = "status")
#'
#' task$target_names
#' task$feature_names
#' task$formula
#' task$truth()
NULL

#' @export
TaskSurv = R6::R6Class("TaskSurv",
  inherit = TaskSupervised,
  public = list(

    initialize = function(id, backend, time, status) {
      super$initialize(id = id, task_type = "surv", backend = backend, target = c(time, status))
      self$measures = list(mlr_measures$get("harrells_c"))
    },

    truth = function(row_ids = NULL) {
      tn = self$target_names
      d = self$data(row_ids, cols = self$target_names)
      Surv(d[[tn[1L]]], d[[tn[2L]]])
    }
  ),

  active = list(
    formula = function() {
      tn = self$target_names
      f = as.formula(sprintf("Surv(%s, %s) ~ %s", tn[1L], tn[2L], paste0(self$feature_names, collapse = " + ")))
      environment(f) = getNamespace("survival") # ensure that models find Surv()
      f
    }
  )
)
