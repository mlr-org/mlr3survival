% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LearnerSurvRanger.R
\docType{data}
\name{LearnerSurvRanger}
\alias{LearnerSurvRanger}
\alias{mlr_learners_surv.ranger}
\title{Survival Ranger Learner}
\format{\code{\link[R6:R6Class]{R6::R6Class()}} inheriting from \link{LearnerSurv}.}
\description{
A \link{LearnerSurv} for a survival random forest implemented in \code{\link[ranger:ranger]{ranger::ranger()}} in package \CRANpkg{ranger}.
}
\section{Construction}{
\preformatted{LearnerSurvRanger$new()
mlr_learners$get("surv.ranger")
lrn("surv.ranger")
}
}

\references{
Marvin N. Wright and Andreas Ziegler (2017).
ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R.
Journal of Statistical Software, 77(1), 1-17.
\doi{10.18637/jss.v077.i01}.

Breiman, L. (2001).
Random Forests.
Machine Learning 45(1).
\doi{10.1023/A:1010933404324}.
}
\seealso{
\link[mlr3misc:Dictionary]{Dictionary} of \link[mlr3:Learner]{Learners}: \link[mlr3:mlr_learners]{mlr3::mlr_learners}
}
\keyword{datasets}
