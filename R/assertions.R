assert_surv = function(x, len = NULL, any.missing = FALSE, .var.name = vname(x)) {
  assert_class(x, "Surv", .var.name = .var.name)
  assert_matrix(x, any.missing = any.missing, nrow = len, .var.name = .var.name)
}
