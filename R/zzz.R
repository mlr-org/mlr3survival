#' @import checkmate
#' @import data.table
#' @import mlr3
#' @import mlr3misc
#' @import paradox
#' @importFrom R6 R6Class
#' @importFrom survival Surv
#' @importFrom utils getFromNamespace
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # logger
  lg = lgr::get_logger("mlr3/mlr3")
  assign("lg", lg, envir = parent.env(environment()))

  # reflections
  x = getFromNamespace("mlr_reflections", ns = "mlr3")

  x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
    ~type,        ~package,       ~task,      ~learner,      ~prediction,       ~prediction_data,     ~measure,
    "surv",       "mlr3survival", "TaskSurv", "LearnerSurv", "PredictionSurv",  "PredictionDataSurv", "MeasureSurv"
  )), "type")

  x$task_col_roles$surv = x$task_col_roles$regr
  x$task_properties$surv = x$task_properties$regr
  x$task_mandatory_properties$surv = c("right-censored", "left-censored", "interval-censored")
  x$default_measures$surv = "surv.cindex"
}



leanify_package()
