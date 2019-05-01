#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3
#' @import mlr3misc
#' @import paradox
#' @importFrom survival Surv survfit
#' @importFrom R6 R6Class
NULL

register_mlr3 = function() {
  # let mlr3 know about survival
  x = utils:getFromNamespace("mlr_reflections", ns = "mlr3")
  x$task_types = union(x$task_types, "surv")
  x$task_col_roles$surv = c("feature", "target", "label", "order", "groups", "weights")
  x$task_properties$surv = c("weights", "groups")
  x$learner_properties$surv = x$learner_properties$regr
  x$learner_predict_types$surv = "risk"

  # tasks
  x = utils:getFromNamespace("mlr_tasks", ns = "mlr3")
  x$add("rats", load_rats)
  x$add("lung", load_lung)
  x$add("unemployment", load_unemployment)

  # learners
  x = utils:getFromNamespace("mlr_learners", ns = "mlr3")
  x$add("surv.coxph", LearnerSurvCoxPH)
  x$add("surv.rpart", LearnerSurvRpart)
  x$add("surv.ranger", LearnerSurvRanger)

  # measures
  x = utils:getFromNamespace("mlr_measures", ns = "mlr3")
  x$add("harrells_c", MeasureSurvHarrellsC)
}

.onLoad = function(libname, pkgname) { # nocov start
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
} # nocov end

.onUnload = function(libpath) { # nocov start
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3survival"], action = "replace")
} # nocov end
