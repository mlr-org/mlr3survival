#' @title Survival Measure
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3::Measure].
#'
#' @description
#' This measure specializes [mlr3::Measure] for classification problems.
#' Predefined measures can be found in the [mlr3::Dictionary] [mlr3::mlr_measures].
#'
#' The `task_type` is set to `"classif"`.
#'
#' @section Construction:
#' ```
#' m = MeasureSurv$new(id, range, minimize, predict_type = "response",
#'      task_properties = character(0L), packages = character(0L))
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the measure.
#'
#' * `range` :: `numeric(2)`\cr
#'   Feasible range for this measure as `c(lower_bound, upper_bound)`.
#'
#' * `minimize` :: `logical(1)`\cr
#'   Set to `TRUE` if good predictions correspond to small values.
#'
#' * `predict_type` :: `character(1)`\cr
#'   Required predict type of the [Learner].
#'
#' * `task_properties` :: `character()`\cr
#'   Required task properties, see [Task].
#'
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
#'
#' @inheritSection mlr3::Measure Fields
#' @inheritSection mlr3::Measure Methods
#'
#' @family Measure
#' @export
MeasureSurv = R6Class("MeasureSurv", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize, predict_type = "risk", task_properties = character(0L), packages = character(0L)) {
      super$initialize(id, task_type = "surv", range = range, minimize = minimize, predict_type = predict_type,
        task_properties = task_properties, packages = packages)
    })
)
