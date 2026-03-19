#' @title Get Survival Predict Types
#'
#' @description
#' Helper function to more easily return the correct survival predict types.
#' This should be used by all survival mlr3 learners, to ensure that the `crank`
#' prediction type is always included.
#'
#' @param surv (`matrix()`)\cr
#'  Matrix of predicted survival probabilities (rows = observations, columns = time points).
#'  Number of columns should be equal to length of `times`, if provided.
#'  Several checks are performed with [assert_prob][survdistr::assert_prob()].
#' @param times (`numeric()`|`NULL`) \cr
#'  Vector of survival times.
#'  If `NULL`, extracted from `colnames(surv)`.
#' @param crank (`numeric()`)\cr
#'  Relative risk/continuous ranking.
#'  Higher value is associated with higher risk.
#'  If `NULL` then either set as `-response` if available or `lp` if available
#'  (this assumes that the `lp` prediction comes from a PH type model - in case
#'  of an AFT model the user should provide `-lp`).
#'  In case neither `response` or `lp` are provided, then `crank` is calculated
#'  as the sum of the cumulative hazard function (**expected mortality**)
#'  derived from the predicted survival function (`surv`), see [get_mortality].
#' @param lp (`numeric()`)\cr
#'  Predicted linear predictor, used to impute `crank` if the latter is `NULL`.
#' @param response (`numeric()`)\cr
#'  Predicted survival time, passed through function without modification.
#' @template param_interp_method
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
#' surv_return(surv = surv_mat, times = 1:k)
#'
#' # if crank is set, it's not overwritten
#' surv_return(surv = surv_mat, times = 1:k, crank = rnorm(n))
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
  surv = NULL,
  times = NULL,
  crank = NULL,
  lp = NULL,
  response = NULL,
  interp_method = "const_surv"
  ) {
  assert_choice(interp_method, c("const_surv", "linear_surv", "const_haz"))

  if (!is.null(surv)) {
    assert_matrix(surv, mode = "numeric", any.missing = FALSE, min.rows = 1, min.cols = 1)

    # perform checks on the survival matrix (better to do always here, and avoid troubles later)
    # we also get validated times invisibly
    times = survdistr::assert_prob(x = surv, times = times, type = "surv")
    # TODO: pass `times` also separately in mlr3's Prediction object and don't "stick" it as colnames!
    colnames(surv) = times
  }

  # set crank (mandatory)
  if (is.null(crank)) {
    # TODO: set crank = lp (priority over response)
    if (!is.null(response)) {
      # low survival time = high risk
      # high crank = high risk
      crank = -response
    } else if (!is.null(lp)) {
      # assumes PH-type lp where high value = high risk
      crank = lp
    } else if (!is.null(surv)) {
      # `surv` already validated above, no need to check again
      crank = get_mortality(x = surv, check = FALSE)
    }
  }

  pred = list(
    distr = surv, # matrix
    crank = crank,
    lp = lp,
    response = response
  )

  # due to restrictions in mlr3 we pass further arguments as attributes
  setattr(pred, "interp_method", interp_method)

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
#' @param x (`matrix()`) \cr
#'  A survival matrix.
#'  Rows correspond to observations and columns correspond to time points.
#' @param times (`numeric()`|`NULL`)\cr
#'  Optional numeric vector of time points corresponding to the columns of `x`.
#' @param eps (`numeric(1)`)\cr
#'  Small positive constant used to bound survival probabilities away from zero
#'  and prevent infinite cumulative hazards, as \code{H(t) = -log(S(t))}.
#' @param check (`logical(1)`)\cr
#'  If `TRUE` (default), uses [assert_prob][survdistr::assert_prob()]
#'  to perform several checks on the survival matrix.
#'  Set to `FALSE` to skip checks (NOT recommended for external use).
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
#' x = t(apply(mat, 1L, function(row) sort(row, decreasing = TRUE)))
#' colnames(x) = 1:k # time points
#'
#' # get mortality scores (larger values correspond to higher risk scores)
#' mort = get_mortality(x)
#' mort
#'
#' @export
get_mortality = function(x, times = NULL, eps = 1e-12, check = TRUE) {
  assert_matrix(x, mode = "numeric", any.missing = FALSE, min.rows = 1, min.cols = 1)
  assert_flag(check)
  if (check) assert_prob(x, times, type = "surv")

  # H(t) = -log(S(t)), floored at eps to avoid -log(0) = Inf
  cumhaz = -log(pmax(x, eps))

  rowSums(cumhaz)
}
