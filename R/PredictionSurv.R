#' @title Prediction Object for Survival
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3::Prediction].
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerSurv].
#'
#' The `task_type` is set to `"surv"`.
#'
#' @section Construction:
#' ```
#' p = PredictionSurv$new(task = NULL, risk = NULL)
#' ```
#'
#' * `task` :: [TaskClassif]\cr
#'   Task for which the predictions are made. Used to extract the row ids and the true
#'   labels. Must be subsetted to test set.
#'
#' * `risk` :: `numeric()`\cr
#'   Vector of risk scores. One element for each observation in the test set.
#'   The higher the risk, the more likely is an event.
#'   Used in measures like [mlr_measures_surv.harrells_c].
#'
#' Note that it is allowed to initialize this object without any arguments in order
#' to allow to manually construct [mlr3::Prediction] objects in a piecemeal fashion.
#' Required are "row_ids", "truth", and "predict_type". Depending on the value of
#' "predict_types", also "risk" must be set.
#'
#' @inheritSection mlr3::Prediction Fields
#'
#' @family Prediction
#' @export
#' @examples
#' library(mlr3)
#' task = mlr_tasks$get("lung")
#' learner = mlr_learners$get("surv.rpart")
#' e = Experiment$new(task, learner)$train()$predict()
#' p = e$prediction
#' head(as.data.table(p))
PredictionSurv = R6Class("PredictionSurv", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    risk = NULL,
    initialize = function(task = NULL, risk = NULL) {
      self$task_type = "surv"
      n = NULL
      if (!is.null(task)) {
        self$row_ids = task$row_ids
        self$truth = task$truth()
        n = length(self$row_ids)
      }

      self$predict_types = c("risk")[!is.null(risk)]
      self$risk = assert_numeric(risk, len = n, any.missing = FALSE, null.ok = TRUE)
    })
)

#' @export
as.data.table.PredictionSurv = function(x, ...) {
  tab = data.table(row_id = x$row_ids, risk = x$risk)
  truth = x$truth
  if (!is.null(truth)) {
    tab[, c("time", "status") := list(truth[, 1L], truth[, 2L])]
  }
  tab
}
