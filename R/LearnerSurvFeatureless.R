#' @title Featureless Survival Learner
#'
#' @aliases mlr_learners_surv.featureless
#' @format [R6::R6Class] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @description
#' A simple [LearnerSurv] which only considers the survival time, ignoring all features.
#' Currently, only risk prediction is supported, where the learner just samples random
#' risk scores.
#' @export
LearnerSurvFeatureless = R6Class("LearnerSurvFeatureless", inherit = LearnerSurv,
  public = list(
    initialize = function(id = "surv.featureless") {
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = "risk",
        properties = c("missings", "importance", "selected_features")
      )
    },

    train = function(task) {
      self$model = list(features = task$feature_names)
      self
    },

    predict = function(task) {
      as_prediction_data(task, risk = runif(task$nrow))
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
