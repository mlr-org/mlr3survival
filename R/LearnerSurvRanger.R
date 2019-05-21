#' @title Survival Ranger Learner
#'
#' @aliases mlr_learners_surv.ranger
#' @format [R6::R6Class()] inheriting from [LearnerSurv].
#'
#' @description
#' A [LearnerSurv] for a survival random forest implemented in [ranger::ranger()] in package \CRANpkg{ranger}.
#'
#' @export
LearnerSurvRanger = R6Class("LearnerSurvRanger", inherit = LearnerSurv,
  public = list(
    initialize = function(id = "surv.ranger") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "num.trees", default = 500L, lower = 1L, tags = c("train", "predict")),
            ParamInt$new(id = "mtry", lower = 1L, tags = "train"),
            ParamFct$new(id = "importance", levels = c("none", "impurity", "impurity_corrected", "permutation"), tags = "train"),
            ParamLgl$new(id = "write.forest", default = TRUE, tags = "train"),
            ParamInt$new(id = "min.node.size", default = 5L, lower = 1L, tags = "train"), # for probability == TRUE, def = 10
            ParamLgl$new(id = "replace", default = TRUE, tags = "train"),
            ParamDbl$new(id = "sample.fraction", lower = 0L, upper = 1L, tags = "train"), # for replace == FALSE, def = 0.632
            # ParamDbl$new(id = "class.weights", defaul = NULL, tags = "train"), #
            ParamFct$new(id = "splitrule", levels = c("variance", "extratrees", "maxstat"), default = "variance", tags = "train"),
            ParamInt$new(id = "num.random.splits", lower = 1L, default = 1L, tags = "train"), # requires = quote(splitrule == "extratrees")
            ParamDbl$new(id = "split.select.weights", lower = 0, upper = 1, tags = "train"),
            ParamUty$new(id = "always.split.variables", tags = "train"),
            ParamFct$new(id = "respect.unordered.factors", levels = c("ignore", "order", "partition"), default = "ignore", tags = "train"), # for splitrule == "extratrees", def = partition
            ParamLgl$new(id = "scale.permutation.importance", default = FALSE, tags = "train"), # requires = quote(importance == "permutation")
            ParamLgl$new(id = "keep.inbag", default = FALSE, tags = "train"),
            ParamLgl$new(id = "holdout", default = FALSE, tags = "train"), # FIXME: do we need this?
            ParamInt$new(id = "num.threads", lower = 1L, tags = c("train", "predict")),
            ParamLgl$new(id = "save.memory", default = FALSE, tags = "train"),
            ParamLgl$new(id = "verbose", default = TRUE, tags = c("train", "predict")),
            ParamLgl$new(id = "oob.error", default = TRUE, tags = "train")
          )
        ),
        predict_types = "risk",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("weights", "importance", "oob_error"),
        packages = "ranger"
      )
    },

    train = function(task) {
      pars = self$params("train")
      targets = task$target_names

      invoke(ranger::ranger,
        formula = NULL,
        dependent.variable.name = targets[1L],
        status.variable.name = targets[2L],
        data = task$data(),
        case.weights = task$weights$weight,
        .args = pars
      )
    },

    predict = function(task) {
      pars = self$params("predict")
      newdata = task$data(cols = task$feature_names)
      p = predict(object = self$model, data = newdata)

      list(risk = rowMeans(p$chf))
    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      if (self$model$importance.mode == "none") {
        stopf("No importance stored")
      }

      sort(self$model$variable.importance, decreasing = TRUE)
    },

    oob_error = function() {
      mod$prediction.error
    }
  )
)
