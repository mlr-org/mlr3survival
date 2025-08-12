#' @title German Breast Cancer Study Survival Task
#'
#' @name mlr_tasks_gbsg
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id gbsg
#' @templateVar data gbsg
#' @templateVar data_pkg survival
#' @template task
#' @template seealso_task
#'
#' @section Pre-processing:
#' - Removed column `pid`.
#' - Column `meno` has been converted to `factor` and 0/1 values have been
#' replaced with `premenopausal` and `postmenopausal` respectively.
#' - Column `hormon` has been converted to `factor` and 0/1 values have been
#' replaced with `no` and `yes` respectively.
#' - Column `grade` has been converted to `factor`.
#' - Renamed target column `rfstime` to `time`.
NULL

load_gbsg = function() {
  data = survival::gbsg
  data$pid = NULL
  data$meno = factor(ifelse(data$meno == 0, "premenopausal", "postmenopausal"),
                     levels = c("premenopausal", "postmenopausal"))
  data$hormon = factor(ifelse(data$hormon == 0, "no", "yes"),
                       levels = c("no", "yes"))
  data$grade = factor(data$grade)
  colnames(data)[colnames(data) == "rfstime"] = "time"

  b = as_data_backend(data)
  task = TaskSurv$new("gbsg", b, time = "time", event = "status",
                      label = "German Breast Cancer")
  b$hash = task$man = "mlr3proba::mlr_tasks_gbsc"

  task
}


#' @include aaa.R
tasks[["gbsg"]] = load_gbsg
