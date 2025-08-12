#' @title Monoclonal Gammopathy Survival Task
#'
#' @name mlr_tasks_mgus
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id mgus
#' @templateVar data mgus
#' @templateVar data_pkg survival
#' @template task
#' @template seealso_task
#'
#' @section Pre-processing:
#' - Removed columns `id`, `pcdx` and `pctime`.
#' - Renamed target columns from (`fultime`, `death`) to (`time`, `status`).
#' - Kept only complete cases (no missing values).
NULL

load_mgus = function() {
  data = survival::mgus
  data[, c("id", "pcdx", "pctime")] = NULL
  colnames(data)[colnames(data) == "futime"] = "time"
  colnames(data)[colnames(data) == "death"] = "status"
  data = stats::na.omit(data)

  b = as_data_backend(data)
  task = TaskSurv$new("mgus", b, time = "time", event = "status", label = "MGUS")
  b$hash = task$man = "mlr3proba::mlr_tasks_mgus"

  task
}

#' @include aaa.R
tasks[["mgus"]] =  load_mgus
