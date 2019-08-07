#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3
#' @import mlr3misc
#' @import paradox
#' @importFrom survival Surv survfit
#' @importFrom R6 R6Class
"_PACKAGE"

register_mlr3 = function() {

  # let mlr3 know about survival
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
    ~type,  ~package,       ~task,      ~learner,      ~prediction,      ~measure,
    "surv", "mlr3survival", "TaskSurv", "LearnerSurv", "PredictionSurv", "MeasureSurv"
  )), "type")
  x$task_col_roles$surv = c("feature", "target", "label", "order", "groups", "weights")
  x$task_properties$surv = c("weights", "groups")
  x$learner_properties$surv = x$learner_properties$regr
  x$learner_predict_types$surv = list(risk = "risk")
  x$default_measures$surv = "surv.harrells_c"

  # tasks
  x = utils::getFromNamespace("mlr_tasks", ns = "mlr3")
  x$add("rats", load_rats)
  x$add("lung", load_lung)
  x$add("unemployment", load_unemployment)

  # generators
  x = utils::getFromNamespace("mlr_generators", ns = "mlr3")
  x$add("simsurv", GeneratorSimsurv)

  # learners
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")
  x$add("surv.coxph", LearnerSurvCoxPH)
  x$add("surv.glmnet", LearnerSurvGlmnet)
  x$add("surv.rpart", LearnerSurvRpart)
  x$add("surv.ranger", LearnerSurvRanger)
  x$add("surv.featureless", LearnerSurvFeatureless)

  # measures
  x = utils::getFromNamespace("mlr_measures", ns = "mlr3")
  x$add("surv.harrells_c", MeasureSurvHarrellsC)
  x$add("surv.unos_c", MeasureSurvUnosC)
}

.onLoad = function(libname, pkgname) {
  # nocov start
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
} # nocov end

.onUnload = function(libpath) {
  # nocov start
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3survival"], action = "replace")
} # nocov end
