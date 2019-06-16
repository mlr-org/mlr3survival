#' @title Survival Tree Learner
#'
#' @aliases mlr_learners_surv.rpart
#' @format [R6::R6Class] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @description
#' A [LearnerSurv] for a regression tree implemented in [rpart::rpart] in package \CRANpkg{rpart}.
#' Parameter `xval` is set to 0 in order to save some computation time.
#'
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
        param_vals = list(xval = 0L),
        predict_types = "risk",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("weights", "missings", "importance", "selected_features"),
        packages = "rpart"
      )
    },

    train = function(task) {
      pars = self$params("train")
      if ("weights" %in% task$properties) {
        pars = insert_named(pars, list(weights = task$weights$weight))
      }
      self$model = invoke(rpart::rpart, formula = task$formula(), data = task$data(), method = "exp", .args = pars)
      self
    },

    predict = function(task) {
      newdata = task$data(cols = task$feature_names)
      list(risk = unname(predict(self$model, newdata = newdata, type = "vector")))
    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      # importance is only present if there is at least on split
      sort(self$model$variable.importance %??% set_names(numeric()), decreasing = TRUE)
    },

    selected_features = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      unique(setdiff(self$model$frame$var, "<leaf>"))
    }
  )
)
