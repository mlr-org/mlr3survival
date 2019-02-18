#' @title Survival Learner
#'
#' @name LearnerSurv
#' @format [R6Class] object inheriting from [Learner].
#' @description
#' This Learner specializes [Learner] for survival analysis tasks.
#'
#' @section Usage:
#' See [Learner].
#'
#' @family Learner
#' @examples
#' library(mlr3)
#' ids = mlr_learners$keys("^surv")
#' lrns = mlr_learners$mget(ids)
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' lrn = mlr_learners$get("surv.rpart")
#' print(lrn)
NULL

#' @export
LearnerSurv = R6Class("LearnerSurv", inherit = Learner,
  public = list(
    initialize = function(id, feature_types = character(0L), predict_types = "risk", packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L)) {
      super$initialize(id = id, task_type = "surv", feature_types = feature_types, predict_types = predict_types,
        packages = packages, param_set = param_set, param_vals = param_vals, properties = properties)
    }
  )
)
