#' @title Convert to a Survival Prediction
#'
#' @description
#' Convert object to a [PredictionSurv].
#'
#' @inheritParams mlr3::as_prediction
#'
#' @return [PredictionSurv].
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("rats")
#' learner = lrn("surv.coxph")
#' learner$train(task)
#' p = learner$predict(task)
#'
#' # convert to a data.table
#' tab = as.data.table(p)
#'
#' # convert back to a Prediction
#' as_prediction_surv(tab)
as_prediction_surv = function(x, ...) {
  UseMethod("as_prediction_surv")
}

#' @rdname as_prediction_surv
#' @export
as_prediction_surv.PredictionSurv = function(x, ...) {
  x
}

#' @rdname as_prediction_surv
#' @export
as_prediction_surv.data.frame = function(x, ...) {
  mandatory = c("row_ids", "time", "status")
  optional = c("crank", "lp", "distr", "response")
  assert_names(names(x), must.include = mandatory, subset.of = c(mandatory, optional))

  distr = if ("distr" %in% names(x)) {
    times = names(x$distr[[1L]][[1L]])

    # Reconstruct the survival matrix from the list of vectors
    surv_mat = do.call(rbind, lapply(x$distr, function(l) { matrix(l[[1L]], nrow = 1) }))
    set_col_names(surv_mat, times)
  } else NULL

  if ("crank" %nin% names(x)) {
    if ("lp" %in% names(x)) {
      x$crank = x$lp
    } else if ("response" %in% names(x)) {
      x$crank = -x$response
    } else {
      x$crank = get_mortality(x = distr, check = FALSE)
    }
  }

  setDT(x) # if just a `data.frame`, with = FALSE below will not work!
  x_subset = x[, setdiff(names(x), c("time", "status", "distr")), with = FALSE]

  invoke(PredictionSurv$new,
    truth = Surv(x$time, x$status),
    distr = distr,
    data_type = attr(x, "data_type") %||% "surv",
    interp_meth = attr(x, "interp_meth") %||% "const_surv",
    .args = x_subset
  )
}
