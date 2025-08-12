#' @title Convert to a Survival Prediction
#'
#' @description
#' Convert object to a [PredictionSurv].
#'
#' @inheritParams mlr3::as_prediction
#'
#' @return [PredictionSurv].
#' @export
as_prediction_surv = function(x, ...) {
  UseMethod("as_prediction_surv")
}


#' @rdname as_prediction_surv
#' @export
as_prediction_surv.PredictionSurv = function(x, ...) { # nolint
  x
}


#' @rdname as_prediction_surv
#' @export
as_prediction_surv.data.frame = function(x, ...) { # nolint
  mandatory = c("row_ids", "time", "status")
  optional = c("crank", "lp", "distr", "response")
  assert_names(names(x), must.include = mandatory)
  assert_names(names(x), subset.of = c(mandatory, optional))

  if ("distr" %in% names(x)) {
    # FIXME: this is not working
    #distr = x$distr[[1L]][[1L]]
  } else {
    distr = NULL
  }

  if ("crank" %nin% names(x)) {
    if ("lp" %in% names(x)) {
      x$crank = x$lp
    } else if ("response" %in% names(x)) {
      x$crank = -x$response
    } else {
      x$crank = -apply(1 - distr, 1L, function(.x) sum(c(.x[1L], diff(.x)) * x$time))
    }
  }

  invoke(PredictionSurv$new,
    truth = Surv(x$time, x$status),
    distr = distr,
    data_type = attr(x, "data_type"),
    interpolation_type = attr(x, "interpolation_type"),
    .args = x[, -intersect(c("time", "status", "distr"), names(x)), with = FALSE],
  )
}
