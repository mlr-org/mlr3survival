MeasureSurvivalHarrellsC = R6Class("MeasureSurvivalHarrellsC",
  inherit = MeasureSurvival,
  public = list(
    initialize = function(id = "harrells_c") {
      super$initialize(
        id = id,
        range = 0:1,
        minimize = FALSE,
        packages = "Hmisc",
      )
    },

    calculate = function(e) {
      p = e$prediction
      positive = e$data$task$positive
  #   s = getPredictionTruth(pred)
  #   Hmisc::rcorr.cens(-1 * y, s)[["C Index"]]
    }
  )
)

  # fun = function(task, model, pred, feats, extra.args) {
  #   requirePackages("_Hmisc")
  #   y = getPredictionResponse(pred)
  #   if (anyMissing(y))
  #     return(NA_real_)
  #   s = getPredictionTruth(pred)
  #   Hmisc::rcorr.cens(-1 * y, s)[["C Index"]]
  # }
