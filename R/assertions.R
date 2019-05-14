assert_surv = function(x, len = NULL, any.missing = TRUE, .var.name = vname(x)) {
  assert_class(x, "Surv", .var.name = .var.name)
  assert_matrix(x, any.missing = any.missing, nrows = len, .var.name = .var.name)
}
