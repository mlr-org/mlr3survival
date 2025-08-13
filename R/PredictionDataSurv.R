# Here we define some mlr3-mandatory S3 methods of the `PredictionDataSurv` object

#' @export
as_prediction.PredictionDataSurv = function(x, check = TRUE, ...) {
  # due to restrictions in mlr3 we pass further arguments as attributes
  # in crank that is by construction (see `surv_return.R`) not NULL
  invoke(PredictionSurv$new, check = check, data_type = attr(x$crank, "data_type"),
         interp_meth = attr(x$crank, "interp_meth"), .args = x)
}

#' @export
check_prediction_data.PredictionDataSurv = function(pdata, ...) {
  n = length(assert_row_ids(pdata$row_ids))
  assert_surv(pdata$truth, "Surv", len = n, any.missing = TRUE, null.ok = TRUE)
  assert_numeric(pdata$crank, len = n, any.missing = FALSE, null.ok = FALSE)
  assert_numeric(pdata$response, len = n, any.missing = FALSE, null.ok = TRUE)
  assert_numeric(pdata$lp, len = n, any.missing = FALSE, null.ok = TRUE)
  if (inherits(pdata$distr, "matrix")) {
    assert_matrix(pdata$distr, nrows = n, any.missing = FALSE, null.ok = TRUE)
  } # TODO: check if survDistr is given? or enforce matrix for now?

  assert_character(attr(pdata$crank, "data_type"), len = 1L, any.missing = FALSE, null.ok = TRUE)
  assert_character(attr(pdata$crank, "interp_meth"), len = 1L, any.missing = FALSE, null.ok = TRUE)

  pdata
}

#' @export
is_missing_prediction_data.PredictionDataSurv = function(pdata, ...) {
  miss = logical(length(pdata$row_ids))

  if (!is.null(pdata$crank)) {
    miss = is.na(pdata$crank)
  }

  if (!is.null(pdata$lp)) {
    miss = miss | is.na(pdata$lp)
  }

  if (!is.null(pdata$response)) {
    miss = miss | is.na(pdata$response)
  }

  pdata$row_ids[miss]
}

#' @export
c.PredictionDataSurv = function(..., keep_duplicates = TRUE) {
  dots = list(...)
  assert_list(dots, "PredictionDataSurv")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  # predict types must be the same type across predictions to combine
  predict_types = names(mlr_reflections$learner_predict_types$surv)
  predict_types = map(dots, function(x) intersect(names(x), predict_types))
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot combine predictions: Different prediction types")
  }
  predict_types = predict_types[[1L]]

  row_ids = do.call(c, map(dots, "row_ids"))
  ii = if (keep_duplicates) seq_along(row_ids) else which(!duplicated(row_ids, fromLast = TRUE))

  elems = c("truth", intersect(c("crank", "lp", "response"), predict_types))
  result = named_list(elems)
  result$row_ids = row_ids[ii]
  for (elem in elems) {
    result[[elem]] = do.call(c, map(dots, elem))[ii]
  }

  # Merge distr (survival matrices) if present
  if ("distr" %in% predict_types) {
    distr_list = map(dots, "distr") # list of data matrices

    # check that all matrices are of the same type
    data_types = map_chr(dots, function(x) x$data_type)
    if (!every(data_types[-1L], identical, y = data_types[1L])) {
      stopf("Cannot combine predictions: Different 'data_type'")
    }
    data_type = data_types[[1L]]
    # TODO: maybe check/enforce the 'surv' matrix data_type here?
    # e.g. what if it is a hazard matrix (or something else)?

    # check that interpolation methods are of the same type
    interp_meths = map_chr(dots, function(x) x$interp_meth)
    if (!every(interp_meths[-1L], identical, y = interp_meths[1L])) {
      stopf("Cannot combine predictions: Different 'interp_meth'")
    }
    interp_meth = interp_meths[[1L]]

    # Get all common time points across all matrices
    times_list = map(distr_list, function(d) as.numeric(colnames(d)))
    common_times = sort(unique(unlist(times_list)))

    # Interpolate each matrix to the common time grid
    interp_mats = mapply(function(mat, times) {
      survdistr:::rcpp_mat_interp(
        x         = mat,
        times     = times,
        new_times = common_times,
        constant  = ifelse(interp_meth == "const_surv", TRUE, FALSE)
      )
    }, distr_list, times_list, SIMPLIFY = FALSE)

    # Combine row-wise and set colnames
    merged_mat = do.call(rbind, interp_mats)
    colnames(merged_mat) = common_times

    result$distr = merged_mat
    setattr(result$crank, "data_type", data_type)
    setattr(result$crank, "interp_meth", interp_meth)
    # TODO: Can't do the following? refine how we pass these arguments?
    # result$data_type = data_type
    # result$interp_meth = interp_meth
  }

  set_class(result, "PredictionDataSurv")
}

#' @export
filter_prediction_data.PredictionDataSurv = function(pdata, row_ids, ...) {
  keep = pdata$row_ids %in% row_ids

  pdata$row_ids = pdata$row_ids[keep]
  pdata$truth = pdata$truth[keep]

  if (!is.null(pdata$crank)) {
    pdata$crank = pdata$crank[keep]
  }

  if (!is.null(pdata$lp)) {
    pdata$lp = pdata$lp[keep]
  }

  if (!is.null(pdata$response)) {
    pdata$response = pdata$response[keep]
  }

  if (!is.null(pdata$distr)) {
    # TODO: is it a 2d matrix always here?
    filtered = pdata$distr[keep, , drop = FALSE]
    pdata$distr = if (nrow(filtered) > 0) {
      filtered
    } else {
      # edge case: keep an empty matrix
      matrix(numeric(0), nrow = 0, ncol = 0)
    }
  }

  pdata
}
