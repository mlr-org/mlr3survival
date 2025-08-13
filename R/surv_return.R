#' @title Get Survival Predict Types
#'
#' @description
#' Internal helper function to easily return the correct survival predict types.
#' This should be used by all survival mlr3 learners, to ensure that the `crank`
#' prediction type is always included.
#'
#' @param times (`numeric()`) \cr Vector of survival times.
#' @param surv (`matrix()`)\cr Matrix of predicted survival probabilities, rows (1st dimension) are observations, columns (2nd dimension) are time points.
#' Number of columns should be equal to length of `times`.
#' In case a `numeric()` vector is provided, it is converted to a single row (one observation) matrix.
#' @param crank (`numeric()`)\cr Relative risk/continuous ranking.
#' Higher value is associated with higher risk.
#' If `NULL` then either set as `-response` if available or `lp` if available (this assumes that the `lp` prediction comes from a PH type model - in case of an AFT model the user should provide `-lp`).
#' In case neither `response` or `lp` are provided, then `crank` is calculated as the sum of the cumulative hazard function (**expected mortality**) derived from the predicted survival function (`surv`), see [get_mortality].
#' @param lp (`numeric()`)\cr Predicted linear predictor, used to impute `crank` if the latter is `NULL`.
#' @param response (`numeric()`)\cr Predicted survival time, passed through function without
#' modification.
#' @param data_type (`character(1)`)\cr Data type for `surv`. Either `"surv"` for survival or `"haz"` for hazard matrices.
#' @param interp_meth (`character(1)`)\cr
#' Interpolation type; one of `"const_surv"`, `"linear_surv"`, or `"const_haz"`.
#'
#' @references
#' `r format_bib("sonabend_2022")`
#'
#' @examples
#' n = 10 # number of observations
#' k = 50 # time points
#'
#' # Create the matrix with random values between 0 and 1
#' mat = matrix(runif(n * k, min = 0, max = 1), nrow = n, ncol = k)
#'
#' # transform it to a survival matrix
#' surv_mat = t(apply(mat, 1L, function(row) sort(row, decreasing = TRUE)))
#'
#' # crank is expected mortality, distr is the survival matrix
#' surv_return(times = 1:k, surv = surv_mat)
#'
#' # if crank is set, it's not overwritten
#' surv_return(times = 1:k, surv = surv_mat, crank = rnorm(n))
#'
#' # lp = crank
#' surv_return(lp = rnorm(n))
#'
#' # if response is set and no crank, crank = -response
#' surv_return(response = sample(1:100, n))
#'
#' # if both are set, they are not overwritten
#' surv_return(crank = rnorm(n), response = sample(1:100, n))
#'
#' @export
surv_return = function(
  times = NULL,
  surv = NULL,
  crank = NULL,
  lp = NULL,
  response = NULL,
  data_type = "surv",
  interp_meth = "const_surv"
  ) {
  assert_choice(data_type, c("surv", "haz")) # TODO: support haz???
  assert_choice(interp_meth, c("const_surv", "linear_surv", "const_haz"))

  if (!is.null(surv)) {
    # FIXME: allow surv to be a more generic data (surv or haz) denoted by the data_type
    # would it make sense for it to be only surv matrix or survDistr (so that
    # get_mortality => crank calculation can work)

    # given data_type
    if (class(surv)[1L] == "numeric") {
      # in case of a vector (one observation) convert to matrix
      surv = matrix(surv, nrow = 1L, dimnames = list(NULL, names(surv)))
    }

    if (!inherits(surv, "matrix")) {
      stop("'surv' must be a 2D matrix: rows = observations, columns = time points")
    }

    times = times %??% colnames(surv)
    if (length(times) != ncol(surv)) {
      stop("'times' must equal the number of columns of 'surv'")
    }
    colnames(surv) = times
  }

  # set crank (mandatory)
  if (is.null(crank)) {
    if (!is.null(response)) {
      # low survival time = high risk
      # high crank = high risk
      crank = -response
    } else if (!is.null(lp)) {
      # assumes PH-type lp where high value = high risk
      crank = lp
    } else if (!is.null(surv)) {
      crank = get_mortality(surv)
    }
  }

  pred = list(
    distr = surv, # matrix
    crank = crank,
    lp = lp,
    response = response
  )

  # due to restrictions in mlr3 we pass further arguments as attributes
  # in crank that is the only predict type that will not be NULL
  setattr(pred$crank, "data_type", data_type)
  setattr(pred$crank, "interp_meth", interp_meth)

  pred
}

#' @title Calculate the expected mortality risks from a survival matrix
#'
#' @description Many methods can be used to reduce a discrete survival
#' distribution prediction (i.e. matrix) to a relative risk / ranking
#' prediction, see Sonabend et al. (2022).
#'
#' This function calculates a relative risk score as the sum of the
#' predicted cumulative hazard function, also called **ensemble/expected mortality**.
#' This risk score can be loosely interpreted as the expected number of deaths for
#' patients with similar characteristics, see Ishwaran et al. (2008) and has no
#' model or survival distribution assumptions.
#'
#' @param x (`matrix()`) \cr A survival matrix where rows are the
#' (predicted) observations and columns the time-points.
#' For more details, see [survdistr::assert_surv_matrix].
#'
#' @return a `numeric` vector of the mortality risk scores, one per row of the
#' input survival matrix.
#'
#' @references
#' `r format_bib("sonabend_2022", "ishwaran_2008")`
#'
#' @examples
#' n = 10 # number of observations
#' k = 50 # time points
#'
#' # Create the matrix with random values between 0 and 1
#' mat = matrix(runif(n * k, min = 0, max = 1), nrow = n, ncol = k)
#'
#' # transform it to a survival matrix
#' surv_mat = t(apply(mat, 1L, function(row) sort(row, decreasing = TRUE)))
#' colnames(surv_mat) = 1:k # time points
#'
#' # get mortality scores (larger values correspond to higher risk scores)
#' mort = get_mortality(surv_mat)
#' mort
#'
#' @export
get_mortality = function(x) {
  assert_surv_matrix(x)

  # H(t) = -log(S(t))
  cumhaz = -log(x)

  # Ignore S(t) = 0 => -log(S(t)) = Inf
  cumhaz[is.infinite(cumhaz)] = 0

  rowSums(cumhaz)
}
