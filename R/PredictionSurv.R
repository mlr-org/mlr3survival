#' @title Prediction Object for Survival
#'
#' @name PredictionSurv
#' @format [R6Class] object inheriting from [Prediction].
#' @description
#' This object stores the predictions returned by a learner of class [LearnerSurv].
#'
#' @section Usage:
#' Inherits from [Prediction]
#' ```
#' # Construction
#' p = PredictionSurv$new(task, risk)
#'
#' # Members
#' p$predict_types
#' p$risk
#' p$row_ids
#' p$truth
#'
#' # S3 methods
#' as.data.table(p)
#' ```
#'
#' @section Arguments:
#' * `task` ([Task]):
#'   Task used for prediction. Used to extract `row_ids` and `truth`.
#'   Set to `NULL` to skip all argument checks during initialization.
#'   Slots `p$row_ids` and `p$truth` need to be set manually in this case
#' * `risk` (`numeric()`): Vector of risk scores.
#'
#' @section Details:
#' * `$new()` initializes a new object of class [Prediction].
#' * `$predict_types` ([character]) stores the predict types available: currently only "risk" is supported.
#' * `$risk` stores the predicted risk scores.
#' * `row_ids` stores the row IDs.
#' * `$truth` stores the true survival times as [survival::Surv()] object.
#' * The prediction object can be transformed to a simple [data.table()]
#'   with [data.table::as.data.table].
#'   True survival time and status are separate columns, named "time" and "status".
#' @export
#' @family Prediction
#' @examples
#' library(mlr3)
#' task = mlr_tasks$get("lung")
#' learner = mlr_learners$get("surv.rpart")
#' e = Experiment$new(task, learner)$train()$predict()
#' p = e$prediction
#' head(as.data.table(p))
NULL

PredictionSurv = R6Class("PredictionSurv", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    risk = NULL,
    initialize = function(task = NULL, risk = NULL) {
      n = NULL
      if (!is.null(task)) {
        self$row_ids = task$row_ids
        self$truth = task$truth()
        n = length(self$row_ids)
      }

      self$predict_types = c("risk")[!is.null(risk)]
      self$risk = assert_numeric(risk, len = n, any.missing = FALSE, lower = 0, null.ok = TRUE)
    }
  )
)

#' @export
as.data.table.PredictionSurv = function(x, ...) {
  tab = data.table(row_id = x$row_ids, risk = x$risk)
  truth = x$truth
  if (!is.null(truth))
    tab[, c("time", "status") := list(truth[, 1L], truth[, 2L])]
  tab
}
