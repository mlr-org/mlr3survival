#' @title Uno's C-Index
#'
#' @aliases mlr_measures_surv.unos_c
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @description
#' Calls [survAUC::UnoC()].
#'
#' @export
MeasureSurvUnosC = R6Class("MeasureSurvHarrellsC",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.unos_c",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        na_score = TRUE
      )
    },

    calculate = function(experiment, prediction = experiment$prediction) {
      surv_train = experiment$task$truth(experiment$train_set)
      perf = survAUC::UnoC(surv_train, prediction$truth, prediction$risk)
      if (is.nan(perf))
        perf = NA_real_
      perf
    }
  )
)
