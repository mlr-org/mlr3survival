% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TaskSurv_lung.R
\name{mlr_tasks_lung}
\alias{mlr_tasks_lung}
\title{Lung Cancer Survival Task}
\format{\link[R6:R6Class]{R6::R6Class} inheriting from \link{TaskSurv}.}
\description{
A survival task for the \link[survival:lung]{survival::lung} data set.
Columns have been converted to integer, column "sex" has been converted to a factor.
}
\section{Construction}{
\preformatted{mlr3::mlr_tasks$get("lung")
mlr3::tsk("lung")
}
}

\seealso{
\link[mlr3misc:Dictionary]{Dictionary} of \link[mlr3:Task]{Tasks}: \link[mlr3:mlr_tasks]{mlr3::mlr_tasks}
}
