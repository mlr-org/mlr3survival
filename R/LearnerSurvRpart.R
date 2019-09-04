#' @title Survival Tree Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.rpart
#' @format [R6::R6Class] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvRpart$new()
#' mlr_learners$get("surv.rpart")
#' lrn("surv.rpart")
#' ```
#'
#' @description
#' A [LearnerSurv] for a regression tree implemented in [rpart::rpart()] in package \CRANpkg{rpart}.
#' Parameter `xval` is set to 0 in order to save some computation time.
#'
#' @references
#' Breiman, L. (1984).
#' Classification and Regression Trees.
#' New York: Routledge.
#' \doi{10.1201/9781315139470}.
#'
#' @template seealso_learner
#' @export
LearnerSurvRpart = R6Class("LearnerSurvRpart", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("minsplit", default = 20L, lower = 1L, tags = "train"),
        ParamDbl$new("cp", default = 0.01, lower = 0, upper = 1, tags = "train"),
        ParamInt$new("maxcompete", default = 4L, lower = 0L, tags = "train"),
        ParamInt$new("maxsurrogate", default = 5L, lower = 0L, tags = "train"),
        ParamInt$new("maxdepth", default = 30L, lower = 1L, upper = 30L, tags = "train"),
        ParamInt$new("xval", default = 10L, lower = 0L, tags = "train")
      ))
      ps$values = list(xval = 0L)

      super$initialize(
        id = "surv.rpart",
        param_set = ps,
        predict_types = "risk",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("weights", "missings", "importance", "selected_features"),
        packages = "rpart"
      )
    },

    train_internal = function(task) {
      pv = self$param_set$get_values(tags = "train")
      if ("weights" %in% task$properties) {
        pv = insert_named(pv, list(weights = task$weights$weight))
      }
      invoke(rpart::rpart, formula = task$formula(), data = task$data(), method = "exp", .args = pv)
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)
      risk = unname(predict(self$model, newdata = newdata, type = "vector"))
      PredictionSurv$new(task = task, risk = risk)
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
