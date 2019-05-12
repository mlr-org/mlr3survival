#' @title Survival Task
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Task]/[TaskSupervised].
#'
#' @description
#' This task specializes [mlr3::Task] and [mlr3::TaskSupervised] for right-censored survival problems.
#' The target column is assumed to be a factor.
#' Predefined tasks are stored in [mlr3::mlr_tasks].
#'
#' The `task_type` is set to `"surv"`.
#'
#' @section Construction:
#' ```
#' t = TaskSurv$new(id, backend, time, status)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Name of the task.
#'
#' * `backend` :: [DataBackend]
#'
#' * `time` :: `numeric()`\cr
#'   Event times.
#'
#' * `status` :: `integer()`\cr
#'   Event indicator. "0" means alive (no event), "1" means dead (event).
#'
#' @section Fields:
#' @inheritSection mlr3::TaskSupervised Fields
#'
#' @section Methods:
#' * `survfit(strata = character())`\cr
#'   `character()` -> [survival::survfit()]\cr
#'   Creates a [survival::survfit()] object for the survival times.
#'   Argument `strata` can be used to stratify into multiple groups.
#' @inheritSection mlr3::TaskSupervised Methods
#'
#'
#' @family Task
#' @export
#' @examples
#' library(mlr3)
#' data("lung", package = "survival")
#' b = as_data_backend(lung)
#' task = TaskSurv$new("lung", backend = b, time = "time", status = "status")
#'
#' task$target_names
#' task$feature_names
#' task$formula()
#' task$truth()
#' task$survfit("age > 50")
TaskSurv = R6::R6Class("TaskSurv",
  inherit = TaskSupervised,
  public = list(
    initialize = function(id, backend, time, status) {
      super$initialize(id = id, task_type = "surv", backend = backend, target = c(time, status))
      self$measures = list(mlr_measures$get("harrells_c"))
    },

    truth = function(row_ids = NULL) {
      tn = self$target_names
      d = self$data(row_ids, cols = self$target_names)
      Surv(d[[tn[1L]]], d[[tn[2L]]])
    },

    formula = function(rhs = NULL) {
      tn = self$target_names
      lhs = sprintf("Surv(%s, %s)", tn[1L], tn[2L])
      formulate(lhs, rhs %??% self$feature_names, env = getNamespace("survival"))
    },

    survfit = function(strata = character()) {
      assert_character(strata, any.missing = FALSE)
      f = self$formula(rhs = strata)
      vars = unique(unlist(extract_vars(f)))
      survfit(f, self$data(cols = vars))
    })
)
