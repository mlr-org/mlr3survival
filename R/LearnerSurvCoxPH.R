#' @title Cox Proportional Hazards
#'
#' @include LearnerSurv.R
#' @name mlr_learners_surv.coxph
#'
#' @description
#' Calls [survival::coxph()] during training.
#' During prediction, the linear predictor is estimated using [survival::predict.coxph()]
#' while the survival distribution using the Breslow estimator using [survival::survfit.coxph()]
#'
#' @templateVar id surv.coxph
#' @template learner
#'
#' @references
#' `r format_bib("cox_1972")`
#'
#' @export
LearnerSurvCoxPH = R6Class("LearnerSurvCoxPH",
  inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.coxph",
        param_set = ps(
          ties        = p_fct(default = "efron", levels = c("efron", "breslow", "exact"), tags = "train"),
          singular.ok = p_lgl(default = TRUE, tags = "train"),
          type        = p_fct(default = "efron", levels = c("efron", "aalen", "kalbfleisch-prentice"), tags = "predict"),
          stype       = p_int(1L, 2L, default = 2L, tags = "predict")
        ),
        predict_types = c("crank", "distr", "lp"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        properties = "weights",
        packages = "survival",
        label = "Cox Proportional Hazards",
        man = "mlr3survival::mlr_learners_surv.coxph"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      pv$weights = private$.get_weights(task)

      invoke(survival::coxph, formula = task$formula(), data = task$data(),
             model = TRUE, .args = pv)
    },

    .predict = function(task) {
      newdata = ordered_features(task, self)
      pv = self$param_set$get_values(tags = "predict")

      # Get survival predictions via `survfit`
      # TODO: use times argument to predict at the event train time points only!
      fit = invoke(survival::survfit, formula = self$model, newdata = newdata,
                   se.fit = FALSE, .args = pv)

      # Get linear predictors
      lp = as.numeric(invoke(predict, self$model, type = "lp", newdata = newdata))

      surv_mat = t(fit$surv)
      rownames(surv_mat) = NULL # remove row names (observation ids), not needed
      surv_return(times = fit$time, surv = surv_mat, lp = lp)
    }
  )
)

#' @include aaa.R
learners[["surv.coxph"]] = LearnerSurvCoxPH
