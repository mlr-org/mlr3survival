#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3
#' @import mlr3misc
#' @import paradox
#' @importFrom survival Surv survfit
#' @importFrom R6 R6Class
NULL

.onLoad = function(libname, pkgname) {
  # let mlr3 know about survival
  mlr_reflections$task_types = union(mlr_reflections$task_types, "surv")
  mlr_reflections$task_col_roles$surv = c("feature", "target", "label", "order", "groups", "weights")
  mlr_reflections$task_properties$surv = c("weights", "groups")
  mlr_reflections$learner_properties$surv = mlr_reflections$learner_properties$regr
  mlr_reflections$learner_predict_types$surv = "risk"

  # tasks
  mlr_tasks$add("rats", load_rats)
  mlr_tasks$add("lung", load_lung)
  mlr_tasks$add("unemployment", load_unemployment)

  # learners
  mlr_learners$add("surv.coxph", LearnerSurvCoxPH)
  mlr_learners$add("surv.rpart", LearnerSurvRpart)
  mlr_learners$add("surv.ranger", LearnerSurvRanger)

  # measures
  mlr_measures$add("harrells_c", MeasureSurvHarrellsC)
}

if (FALSE) {
}
