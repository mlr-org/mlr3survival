#' @title Cox Proportional Hazard Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.coxph
#' @format [R6::R6Class] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvCoxPH$new()
#' mlr_learners$get("surv.coxph")
#' lrn("surv.coxph")
#' ```
#'
#' @description
#' A [LearnerSurv] for a Cox PH model implemented in [survival::coxph()] in package \CRANpkg{survival}.
#'
#' @references
#' Cox, David R. (1972).
#' Regression models and life‐tables.
#' Journal of the Royal Statistical Society: Series B (Methodological) 34.2 (1972): 187-202.
#' \doi{10.1111/j.2517-6161.1972.tb00899.x}.
#'
#' @template seealso_learner
#' @export
LearnerSurvCoxPH = R6Class("LearnerSurvCoxPH", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.coxph",
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

    train_internal = function(task) {
      pv = self$param_set$get_values(tags = "train")
      if ("weights" %in% task$properties) {
        pv$weights = task$weights$weight
      }
      invoke(survival::coxph, formula = task$formula(), data = task$data(), .args = pv)
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)
      risk = unname(predict(self$model, newdata = newdata, type = "lp"))
      PredictionSurv$new(task = task, risk = risk)
    }

    # importance = function() {
    #   # TODO: renames factors and logicals, so the returned names are not valid
    #   if (is.null(self$model)) {
    #     stopf("No model stored")
    #   }
    #   p = summary(self$model)$coefficients[, 5L]
    #   sort(1 - p, decreasing = TRUE)
    # },

    # selected_features = function() {
    #   # TODO: renames factors and logicals, so the returned names are not valid
    #   if (is.null(self$model)) {
    #     stopf("No model stored")
    #   }
    #   beta = coef(self$model)
    #   names(beta)[!is.na(beta)]
    # }
  )
)
