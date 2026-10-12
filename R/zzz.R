#' @importFrom Rcpp sourceCpp
#' @useDynLib mlr3survival
NULL

#' @import checkmate
#' @import data.table
#' @import mlr3
#' @import mlr3misc
#' @import paradox
#' @import survdistr
#' @importFrom R6 R6Class
#' @importFrom survival Surv
#' @importFrom utils getFromNamespace
#' @importFrom stats predict
"_PACKAGE"

# add tasks, learners and measures to mlr3 dictionaries
register_mlr3survival = function() {
  x = utils::getFromNamespace("mlr_tasks", ns = "mlr3")
  iwalk(tasks, function(obj, nm) x$add(nm, obj))

  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")
  iwalk(learners, function(obj, nm) x$add(nm, obj))

  x = utils::getFromNamespace("mlr_measures", ns = "mlr3")
  iwalk(measures, function(obj, nm) x$add(nm, obj))
}

.onLoad = function(libname, pkgname) {
  # logger
  lg = lgr::get_logger("mlr3/core")
  assign("lg", lg, envir = parent.env(environment()))

  # reflections
  ## tasks
  x = getFromNamespace("mlr_reflections", ns = "mlr3")
  x$task_types = x$task_types[!"surv"] # to ensure we don't have multiple row entries of 'surv'
  x$task_types = setkeyv(
    rbind(
      x$task_types,
      rowwise_table(
        ~type,
        ~package,
        ~task,
        ~learner,
        ~prediction,
        ~prediction_data,
        ~measure,
        "surv",
        "mlr3survival",
        "TaskSurv",
        "LearnerSurv",
        "PredictionSurv",
        "PredictionDataSurv",
        "MeasureSurv"
      )
    ),
    "type"
  )

  x$task_col_roles$surv = x$task_col_roles$regr
  x$task_properties$surv = x$task_properties$regr
  # x$task_mandatory_properties$surv = "right-censored" # if we need to differentiate

  ## measures
  x$measure_properties$surv = x$measure_properties$regr
  x$default_measures$surv = "surv.cindex"

  ## learners
  x$learner_properties$surv = x$learner_properties$reg
  x$learner_predict_types$surv = list(
    crank = c("crank", "lp", "distr", "response"),
    distr = c("crank", "lp", "distr", "response"),
    lp = c("crank", "lp", "distr", "response"),
    response = c("crank", "lp", "distr", "response")
  )

  # mlr3 is an imported dependency, so its dictionaries are already available
  # Direct registration avoids unloading callbacks that inspect unrelated IDE or package hooks
  register_mlr3survival()
}

.onUnload = function(libpath) {
  # Another cleanup may have already removed shared dictionary entries
  walk(intersect(names(learners), mlr_learners$keys()), function(id) mlr_learners$remove(id))
  walk(intersect(names(measures), mlr_measures$keys()), function(id) mlr_measures$remove(id))
  walk(intersect(names(tasks), mlr_tasks$keys()), function(id) mlr_tasks$remove(id))

  # reflections
  x = getFromNamespace("mlr_reflections", ns = "mlr3")
  type = NULL # silence data.table note
  x$task_types = x$task_types[type != "surv"]
  x$task_col_roles$surv = NULL
  x$task_properties$surv = NULL
  x$measure_properties$surv = NULL
  x$default_measures$surv = NULL
  x$learner_properties$surv = NULL
  x$learner_predict_types$surv = NULL
}

leanify_package()
