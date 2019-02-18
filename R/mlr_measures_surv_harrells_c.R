#' @title Harrell's C-Index
#'
#' @name mlr_measures_surv.harrells_c
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @description
#' Calls [Hmisc::rcorr.cens()].
#'
#' @export
MeasureSurvHarrellsC = R6Class("MeasureSurvHarrellsC",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.harrells_c",
        range = 0:1,
        minimize = FALSE,
        packages = "Hmisc",
      )
    },

    calculate = function(e) {
      p = e$prediction
      Hmisc::rcorr.cens(-1 * p$risk, p$truth)[["C Index"]]
    }
  )
)
