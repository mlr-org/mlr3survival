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
#' p = PredictionSurv$new(task = NULL, risk = NULL,
#'   row_ids = task$row_ids, truth = task$truth())
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
#' * `row_ids` :: (`integer()` | `character()`)\cr
#'   Row ids of the task. Per default, these are extracted from the `task`.
#'
#' * `truth` :: `survival::Surv()`\cr
#'   Observed survival times. Per default, these are extracted from the `task`.
#'
#' @section Fields:
#' See [mlr3::Prediction].
#'
#' The field `task_type` is set to `"surv"`.
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
    initialize = function(task = NULL, risk = NULL, row_ids = task$row_ids, truth = task$truth()) {
      self$task_type = "surv"
      self$row_ids = assert_atomic_vector(row_ids)
      n = length(row_ids)
      self$risk = assert_numeric(risk, len = n, any.missing = FALSE, null.ok = TRUE)
      self$truth = assert_surv(truth, len = n, any.missing = FALSE)
      self$predict_types = c("risk")[!is.null(risk)]
    }
  )
)

#' @export
as.data.table.PredictionSurv = function(x, ...) {
  tab = data.table(row_id = x$row_ids, risk = x$risk)
  truth = x$truth
  tab[, c("time", "status") := list(x$truth[, 1L], x$truth[, 2L])]
  setcolorder(tab, c("row_id", "time", "status"))[]
}

#' @export
rbind.PredictionSurv = function(...) {
  dots = list(...)
  assert_list(dots, "PredictionSurv")

  x = map_dtr(dots, function(p) {
    list(row_ids = p$row_ids, risk = p$risk)
  }, .fill = FALSE)

  PredictionSurv$new(row_ids = x$row_ids, truth = do.call(c, map(dots, "truth")), risk = x$risk)
}
