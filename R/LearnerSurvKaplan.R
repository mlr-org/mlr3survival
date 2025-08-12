#' @title Kaplan-Meier Estimator
#'
#' @include LearnerSurv.R
#' @name mlr_learners_surv.kaplan
#'
#' @description
#' Calls [survival::survfit()].
#'
#' @templateVar id surv.kaplan
#' @template learner
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
        properties = c("missings", "right-censored"),
        packages = c("survival", "distr6"),
        label = "Kaplan-Meier Estimator",
        man = "mlr3proba::mlr_learners_surv.kaplan"
      )
    }
  ),

  private = list(
    .train = function(task) {
      invoke(survival::survfit, formula = task$formula(1), data = task$data())
    },

    .predict = function(task) {
      times = self$model$time
      surv = matrix(rep(self$model$surv, task$nrow), ncol = length(times), nrow = task$nrow, byrow = TRUE)
      surv_return(times = times, surv = surv)
    }
  )
)

#' @include aaa.R
learners[["surv.kaplan"]] = LearnerSurvKaplan

