#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3
#' @import mlr3misc
#' @import paradox
#' @importFrom survival Surv
#' @importFrom R6 R6Class
NULL

.onLoad = function(libname, pkgname) {
  # let mlr3 know about survival
  mlr_reflections$task_types = union(mlr_reflections$task_types, "surv")
  mlr_reflections$task_col_roles$surv = c("feature", "target", "order", "groups", "weights")
  mlr_reflections$learner_properties$surv = c("missings", "weights", "parallel", "importance")
  mlr_reflections$learner_predict_types$surv = "risk"

  # tasks
  mlr_tasks$add("lung", load_lung)

  # learners
  mlr_learners$add("surv.rpart", LearnerSurvRpart)

  # measures
  mlr_measures$add("harrells_c", MeasureSurvHarrellsC)
}

if (FALSE) {
}
