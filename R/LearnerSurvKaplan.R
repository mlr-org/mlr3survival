#' @title Kaplan-Meier Estimator
#'
#' @include LearnerSurv.R
#' @name mlr_learners_surv.kaplan
#'
#' @description
#' Calls [survival::survfit()] during training to estimate the survival function
#' at the **unique event times** from the training set. During prediction, each
#' test observation is assigned the same survival probabilities at these times.
#' The risk score (`crank`) is calculated as the sum of the cumulative hazard
#' function (also called expected mortality) at the event times.
#'
#' @templateVar id surv.kaplan
#' @template learner
#' @template example
#'
#' @references
#' `r format_bib("kaplan_1958")`
#'
#' @export
LearnerSurvKaplan = R6Class("LearnerSurvKaplan",
  inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.kaplan",
        predict_types = c("crank", "distr"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("missings", "weights", "importance", "selected_features"),
        packages = "survival",
        label = "Kaplan-Meier Estimator",
        man = "mlr3survival::mlr_learners_surv.kaplan"
      )
    },

    #' @description
    #' All features have a score of `0` for this learner.
    #' This method exists solely for compatibility with the `mlr3` ecosystem,
    #' as this learner is used as a fallback for other survival learners that
    #' require an `importance()` method.
    #'
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }

      fn = self$model$features
      named_vector(fn, 0)
    },

    #' @description
    #' Selected features are always the empty set for this learner.
    #' This method is implemented only for compatibility with the `mlr3` API,
    #' as this learner does not perform feature selection.
    #'
    #' @return `character(0)`.
    selected_features = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }

      character()
    }
  ),

  private = list(
    .train = function(task) {
      survfit_obj = invoke(
        survival::survfit,
        formula = task$formula(1),
        data = task$data(cols = task$target_names),
        .args = list(weights = private$.get_weights(task))
      )

      list(
        model = survfit_obj,
        features = task$feature_names, # keep features for importance
        event_times = task$unique_event_times() # add event times for use in prediction
      )
    },

    .predict = function(task) {
      survfit_model = self$native_model
      times = survfit_model$time # unique train times (event + censoring)
      event_times = self$model$event_times # only event times from train set

      if (length(event_times) == 0L) {
        # no events in the training data => S(t) = 1 for all times
        surv = matrix(1, ncol = length(times), nrow = task$nrow,
                      dimnames = list(NULL, times))
      } else {
        id = which(times %in% event_times)
        times = event_times

        # S(t_event)
        surv = matrix(
          rep(survfit_model$surv[id], task$nrow),
          ncol = length(times),
          nrow = task$nrow,
          byrow = TRUE
        )
      }

      surv_return(surv = surv, times = times)
    }
  ),

  active = list(
    #' @field native_model ([survival::survfit])\cr
    #' The fitted model.
    native_model = function() {
      self$model$model
    }
  )
)

#' @include aaa.R
learners[["surv.kaplan"]] = LearnerSurvKaplan
