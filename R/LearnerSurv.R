#' @title Survival Learner
#'
#' @description
#' This Learner specializes [Learner][mlr3::Learner] for survival problems:
#'
#' * `task_type` is set to `"surv"`
#' * Creates [Prediction][mlr3::Prediction]s of class [PredictionSurv].
#' * Supported `predict_types` are:
#'   - `"distr"`: A survival probability matrix, with rows corresponding to observations
#'                and columns to time points. Internally wrapped as a
#'                [survDistr][survdistr::survDistr] object for interpolation and
#'                transformation.
#'   - `"lp"`: A numeric vector of predicted linear predictors (typically from
#'             proportional hazards models).
#'   - `"crank"`: A numeric vector providing a continuous ranking of risk,
#'                where higher values correspond to higher risk.
#'   - `"response"`: A numeric vector of predicted survival times, passed
#'                   without modification.
#'
#' @template param_id
#' @template param_param_set
#' @template param_predict_types
#' @template param_feature_types
#' @template param_learner_properties
#' @template param_packages
#' @template param_label
#' @template param_man
#'
#' @family Learner
#' @export
#' @examples
#' library(mlr3)
#' # get all survival learners from mlr_learners:
#' lrns = mlr_learners$mget(mlr_learners$keys("^surv"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' mlr_learners$get("surv.coxph")
#' lrn("surv.coxph")
LearnerSurv = R6Class("LearnerSurv",
  inherit = Learner,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ps(), predict_types = "distr",
      feature_types = character(), properties = character(),
      packages = character(), label = NA_character_, man = NA_character_) {
      super$initialize(
        id = id, task_type = "surv", param_set = param_set, predict_types = predict_types,
        feature_types = feature_types, properties = properties,
        packages = c("mlr3survival", packages), label = label, man = man)
    }
  )
)
