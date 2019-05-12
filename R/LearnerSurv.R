#' @title Survival Learner
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3::Learner].
#'
#' @description
#' This Learner specializes [mlr3::Learner] for survival problems.
#' The slot `task_type` is set to `"surv"`.
#' Predefined learners can be found in the [mlr3::Dictionary] [mlr3::mlr_learners].
#'
#' @section Construction:
#' ```
#' l = LearnerSurv$new(id, param_set = ParamSet$new(), param_vals = list(), predict_types = character(),
#'      feature_types = character(), properties = character(), packages = character())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the learner.
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Set of hyperparameters.
#'
#' * `param_vals` :: named `list()`\cr
#'   List of hyperparameter settings.
#'
#' * `predict_types` :: `character()`\cr
#'   Supported predict types. Must be a subset of [`mlr_reflections$learner_predict_types`][mlr_reflections].
#'
#' * `feature_types` :: `character()`\cr
#'   Feature types the learner operates on. Must be a subset of `mlr_reflections$task_feature_types`.
#'
#' * `properties` :: `character()`\cr
#'   Set of properties of the learner. Must be a subset of [`mlr_reflections$learner_properties`][mlr_reflections].
#'
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#'
#' @inheritSection mlr3::Learner Fields
#' @inheritSection mlr3::Learner Methods
#'
#' @family Learner
#' @export
#' @examples
#' library(mlr3)
#' ids = mlr_learners$keys("^surv")
#' lrns = mlr_learners$mget(ids)
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' lrn = mlr_learners$get("surv.rpart")
#' print(lrn)
LearnerSurv = R6Class("LearnerSurv", inherit = Learner,
  public = list(
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), predict_types = "risk", feature_types = character(), properties = character(), packages = character()) {
      super$initialize(id = id, task_type = "surv", param_set = param_set, param_vals = param_vals,
        predict_types = predict_types, feature_types = feature_types, properties = properties, packages = packages)
    })
)
