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
#' p = learner$train(task)$predict(task)
#' head(as.data.table(p))
PredictionSurv = R6Class("PredictionSurv", inherit = Prediction,
  public = list(
    initialize = function(row_ids, truth, risk = NULL) {
      self$data$row_ids = assert_atomic_vector(row_ids)
      self$data$truth = assert_surv(truth)
      self$data$risk = assert_numeric(risk, null.ok = TRUE)
      self$task_type = "surv"
    }
  ),

  active = list(
    risk = function() self$data$risk,
    missing = function() {
      if (is.null(self$data$risk)) {
        return(self$data_row_ids[0L])
      }
      self$data$row_ids[is.na(self$data$risk)]
    }
  )
)


#' @export
as.data.table.PredictionSurv = function(x, ...) {
  tab = data.table(row_id = x$data$row_ids, risk = x$data$risk)
  if (!is.null(x$data$truth)) {
    tab[, c("time", "status") := list(x$data$truth[, 1L], x$data$truth[, 2L])]
    setcolorder(tab, c("row_id", "time", "status"))[]
  }
  tab
}

#' @export
c.PredictionSurv = function(..., keep_duplicates = TRUE) {
  dots = list(...)
  assert_list(dots, "PredictionSurv")
  assert_flag(keep_duplicates)

  x = map_dtr(dots, function(p) {
    list(row_ids = p$data$row_ids, risk = p$data$risk)
  }, .fill = FALSE)
  truth = do.call(c, map(dots, "truth"))

  if (!keep_duplicates) {
    keep = !duplicated(x$row_ids, fromLast = TRUE)
    x = x[keep]
    truth = truth[keep]
  }

  PredictionSurv$new(row_ids = x$row_ids, truth = truth, risk = x$risk)
}
