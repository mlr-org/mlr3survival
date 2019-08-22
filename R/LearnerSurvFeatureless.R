#' @title Featureless Survival Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.featureless
#' @format [R6::R6Class] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvFeatureless$new()
#' mlr_learners$get("surv.featureless")
#' lrn("surv.featureless")
#' ```
#'
#' @description
#' A simple [LearnerSurv] which only considers the survival time, ignoring all features.
#' Currently, only risk prediction is supported, where the learner just samples random
#' risk scores.
#'
#' @template seealso_learner
#' @export
LearnerSurvFeatureless = R6Class("LearnerSurvFeatureless", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.featureless",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = "risk",
        properties = c("missings", "importance", "selected_features")
      )
    },

    train_internal = function(task) {
      set_class(list(features = task$feature_names), "surv.feaureless_model")
    },

    predict_internal = function(task) {
      PredictionSurv$new(task = task, risk = runif(task$nrow))
    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      fn = self$model$features
      set_names(double(length(fn)), fn)
    },

    selected_features = function() {
      character(0L)
    })
)
