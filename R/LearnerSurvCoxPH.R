#' @title Cox Proportional Hazard Learner
#'
#' @aliases mlr_learners_surv.coxph
#' @format [R6::R6Class] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @description
#' A [LearnerSurv] for a Cox PH model implemented in [survival::coxph] in package \CRANpkg{survival}.
#'
#' @export
LearnerSurvCoxPH = R6Class("LearnerSurvCoxPH", inherit = LearnerSurv,
  public = list(
    initialize = function(id = "surv.coxph") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(
          params = list(
            ParamFct$new(id = "ties", default = "efron", levels = c("efron", "breslow", "exact"), tags = "train")
          )
        ),
        predict_types = "risk",
        feature_types = c("logical", "integer", "numeric", "factor"),
        properties = c("weights"),
        packages = "survival"
      )
    },

    train = function(task) {
      pars = self$params("train")
      if ("weights" %in% task$properties) {
        pars = insert_named(pars, list(weights = task$weights$weight))
      }
      self$model = invoke(survival::coxph, formula = task$formula(), data = task$data(), .args = pars)
      self
    },

    predict = function(task) {
      newdata = task$data(cols = task$feature_names)
      risk = unname(predict(self$model, newdata = newdata, type = "lp"))
      PredictionSurv$new(task, risk = risk)
    },

    importance = function() {
      # TODO: renames factors and logicals, so the returned names are not valid
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      p = summary(self$model)$coefficients[, 5L]
      sort(1 - p, decreasing = TRUE)
    },

    selected_features = function() {
      # TODO: renames factors and logicals, so the returned names are not valid
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      beta = coef(self$model)
      names(beta)[!is.na(beta)]
    })
)
