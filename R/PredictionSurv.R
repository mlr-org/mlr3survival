#' @title Prediction Object for Survival
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerSurv].
#'
#' The `task_type` is set to `"surv"`.
#'
#' For accessing survival and hazard functions, as well as other complex methods
#' from a [PredictionSurv] object, see public methods of [survDistr][survdistr::survDistr]
#' and example below.
#'
#' @family Prediction
#' @examples
#' library(mlr3)
#' task = tsk("lung")
#' learner = lrn("surv.kaplan")
#' part = partition(task)
#' p = learner$train(task, part$train)$predict(task, part$test)
#' head(as.data.table(p))
#'
#' p$distr # survDistr
#'
#' # survival probabilities of the 4 test observations at two time points
#' p$distr$survival(times = c(80, 100))[1:4, ]
#' @export
PredictionSurv = R6Class("PredictionSurv",
  inherit = Prediction,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([TaskSurv])\cr
    #'   Task, used to extract defaults for `row_ids` and `truth`.
    #' @param row_ids (`integer()`)\cr
    #'   Row ids of the predicted observations, i.e. the row ids of the test set.
    #' @param truth (`survival::Surv()`)\cr
    #'   True (observed) response.
    #' @param crank (`numeric()`)\cr
    #'   Numeric vector of predicted continuous rankings (or relative risks). One element for each
    #'   observation in the test set. For a pair of continuous ranks, a higher rank indicates that
    #'   the observation is more likely to experience the event.
    #' @param distr (`matrix()`)\cr
    #'   A matrix of predicted survival probabilities, where rows = observations
    #'   and columns correspond to time points.
    #' @param lp (`numeric()`)\cr
    #'   Numeric vector of linear predictor scores.
    #'   One element for each observation in the test set.
    #'   \eqn{lp = X\beta} where \eqn{X} is a matrix of covariates and \eqn{\beta} is a vector of estimated coefficients.
    #' @param response (`numeric()`)\cr
    #'   Numeric vector of predicted survival times.
    #'   One element for each observation in the test set.
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs argument checks and predict type conversions.
    #' @param data_type (`character(1)`)\cr
    #'   Type of data (e.g. survival or hazard) stored in `distr`.
    #' @param interp_meth (`character(1)`)\cr
    #'   Type of interpolation used in subsequent methods of the `survDistr` object.
    initialize = function(
      task = NULL,
      row_ids = task$row_ids,
      truth = task$truth(),
      crank = NULL,
      distr = NULL,
      lp = NULL,
      response = NULL,
      check = TRUE,
      data_type = "surv",
      interp_meth = "const_surv"
      ) {
      pdata = list(
        row_ids = row_ids,
        truth = truth,
        crank = crank,
        distr = distr,
        lp = lp,
        response = response
      )
      setattr(pdata, "data_type", data_type)
      setattr(pdata, "interp_meth", interp_meth)
      class(pdata) = c("PredictionDataSurv", "PredictionData")

      # TODO (maybe): simplify `distr` if it is a survDistr? so that we only
      # keep the survival matrix on the $data$distr slot?

      if (check) {
        pdata = check_prediction_data(pdata)
      }

      self$task_type = "surv"
      self$man = "mlr3survival::PredictionSurv"
      self$data = pdata
      self$predict_types = intersect(c("crank", "distr", "lp", "response"), names(pdata))
    }
  ),

  active = list(
    #' @field truth (`Surv`)\cr
    #' True (observed) outcome.
    truth = function() {
      self$data$truth
    },

    #' @field crank (`numeric()`)\cr
    #' Access the stored predicted continuous ranking.
    crank = function() {
      self$data$crank %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field distr ([survdistr::survDistr])\cr
    #' Returns the stored survival predictions as a
    #' [survDistr][survdistr::survDistr] distribution object.
    #' The `survDistr` API provides methods to transform survival data
    #' into other quantities such as cumulative hazard, hazard,
    #' probability density, and cumulative distribution functions.
    distr = function() {
      distr_data = self$data$distr

      # Return NULL if missing or empty matrix
      if (is.null(distr_data) || !is.matrix(distr_data) || any(dim(distr_data) == 0)) {
        return(NULL)
      }

      # Attempt to construct survDistr safely
      tryCatch(
        survDistr$new(
          x = distr_data,
          data_type = attr(self$data, "data_type"),
          interp_meth = attr(self$data, "interp_meth")
        ),
        error = function(e) {
          warning("Failed to construct survDistr: ", conditionMessage(e))
          NULL
        }
      )
    },

    #' @field lp (`numeric()`)\cr
    #' Access the stored predicted linear predictor.
    lp = function() {
      self$data$lp
    },

    #' @field response (`numeric()`)\cr
    #' Access the stored predicted survival time.
    response = function() {
      self$data$response
    }
  )
)

#' @export
as.data.table.PredictionSurv = function(x, ...) {
  tab = as.data.table(x$data[c("row_ids", "crank", "lp", "response")])
  tab$time = x$data$truth[, 1L]
  tab$status = as.logical(x$data$truth[, 2L])

  n_obs = length(x$row_ids)
  if ("distr" %in% x$predict_types && n_obs > 0) {
    distr = x$data$distr # (survival) matrix

    # split survival matrix to 1 vector (curve) per observation
    # wrapped in a list for nice printing
    tab$distr = lapply(1:n_obs, function(i) {
      list(distr[i, , drop = TRUE])
    })

    # strange edge issue with 1 row `data.table` => extra list is removed!
    # so we put it back in:
    if (nrow(tab) == 1) {
      tab$distr = list(list(tab$distr))
    }
  }

  setcolorder(tab, c("row_ids", "time", "status"))[]
}
