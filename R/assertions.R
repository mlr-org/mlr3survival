#' @title Assert survival object
#'
#' @description
#' Asserts `x` is a [survival::Surv] object with added checks.
#'
#' @param x (`Surv`)\cr
#' Object to check.
#' @param len (`integer(1)`)\cr
#' If non-NULL checks object is length `len`.
#' @param any.missing (`logical(1)`)\cr
#' If `FALSE` then errors if there are any NAs in `x`.
#' @param null.ok (`logical(1)`)\cr
#' If `FALSE` then errors if `x` is NULL, otherwise passes.
#' @param .var.name (`character(1)`)\cr
#' Optional variable name to return if assertion fails.
#'
#' @export
assert_surv = function(x, len = NULL, any.missing = TRUE, null.ok = FALSE, .var.name = vname(x)) { # nolint
  assert_class(x, "Surv", null.ok = null.ok, .var.name = .var.name)
  assert_matrix(x, any.missing = any.missing, nrows = len, null.ok = null.ok, .var.name = .var.name)
}

