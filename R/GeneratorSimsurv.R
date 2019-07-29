#' @title Survival Task Generator for Package 'simsurv'
#'
#' @aliases mlr_generators_simsurv
#' @format [R6::R6Class] inheriting from [mlr3::Generator].
#'
#' @description
#' A [mlr3::Generator] calling [simsurv::simsurv()] from package \CRANpkg{simsurv}.
#'
#' This simulator only exposes a small subset of the flexibility of \CRANpkg{simsurv}, and just creates a small data set with the following numerical covariates:
#'
#' * `treatment`: Bernoulli distributed with log hazard ratio `-0.5`.
#' * `height`: Normally distributed with log hazard ratio `1`.
#' * `weight`: normally distributed with log hazard ratio `0`.
#'
#' See [simsurv::simsurv()] for an explanation of the hyperparameters.
#'
#' @export
#' @examples
#' generator = mlr3::mlr_generators$get("simsurv")
#' task = generator$generate(200)
#' task$head()
#' plot(task$survfit())
GeneratorSimsurv = R6Class("GeneratorSmiley",
  inherit = Generator,
  public = list(
    initialize = function(id = "smiley") {
      param_set = ParamSet$new(list(
        ParamFct$new("dist", levels = c("weibull", "exponential", "gompertz"), default = "weibull"),
        ParamDbl$new("lambdas", lower = 0, default = 0.1, tags = "required"),
        ParamDbl$new("gammas", lower = 0, default = 1.5, tags = "required"),
        ParamDbl$new("maxt", lower = 0, default = 5, tags = "required")
      ))
      super$initialize(id = id, "classif", "mlbench", param_set, param_vals = list(lambdas = 0.1, gammas = 1.5, maxt = 5))
    }
  ),

  private = list(
    .generate = function(n) {
      require_namespaces("simsurv")

      pv = self$param_set$values
      covs = data.table(
        treatment = rbinom(n, 1L, 0.5),
        height = rnorm(n, 180, 15),
        weight = rnorm(n, 80, 10)
      )
      betas = c(treatment = -0.5, height = 1, weight = 0)

      data = setDT(invoke(simsurv::simsurv, x = covs, betas = , .args = pv))
      data = rcbind(data, covs)
      TaskSurv$new("simsurv", remove_named(data, "id"), time = "eventtime", status = "status")
    }
  )
)
