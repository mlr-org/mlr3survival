#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3
#' @importFrom survival Surv
#' @importFrom R6 R6Class
NULL

.onLoad = function(libname, pkgname) {
  mlr_reflections$task_types = union(mlr_reflections$task_types, "survival")
  mlr_reflections$task_col_roles$survival = c("feature", "target", "order", "groups", "weights")
}
