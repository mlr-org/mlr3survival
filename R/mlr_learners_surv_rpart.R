#' @title Survival Tree Learner
#'
#' @name mlr_learners_survival.rpart
#' @format [R6::R6Class] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @description
#' A learner for a regression tree implemented in [rpart::rpart].
#' @export
LearnerSurvRpart = R6Class("LearnerSurvRpart", inherit = LearnerSurv,
  public = list(
    initialize = function(id = "surv.rpart") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "minsplit", default = 20L, lower = 1L, tags = "train"),
            ParamDbl$new(id = "cp", default = 0.01, lower = 0, upper = 1, tags = "train"),
            ParamInt$new(id = "maxcompete", default = 4L, lower = 0L, tags = "train"),
            ParamInt$new(id = "maxsurrogate", default = 5L, lower = 0L, tags = "train"),
            ParamInt$new(id = "maxdepth", default = 30L, lower = 1L, upper = 30L, tags = "train"),
            ParamInt$new(id = "xval", default = 10L, lower = 0L, tags = "train")
          )
        ),
        predict_types = "risk",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = "missings",
        packages = "rpart"
      )
    },

    train = function(task) {
      pars = self$params("train")
      self$model = invoke(rpart::rpart, formula = task$formula, data = task$data(),
        method = "exp", .args = pars)
      self
    },

    predict = function(task) {
      newdata = task$data()
      risk = unname(predict(self$model, newdata = newdata, type = "vector"))
      PredictionSurv$new(task, risk = risk)
    },

    importance = function() {
      if (is.null(self$model))
        stopf("No model stored")
      sort(self$model$variable.importance, decreasing = TRUE)
    },

    selected_features = function() {
      if (is.null(self$model))
        stopf("No model stored")
      unique(setdiff(self$model$frame$var, "<leaf>"))
    }
  )
)
