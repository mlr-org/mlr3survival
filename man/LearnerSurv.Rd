% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LearnerSurv.R
\name{LearnerSurv}
\alias{LearnerSurv}
\title{Survival Learner}
\format{\link{R6Class} object inheriting from \link{Learner}.}
\description{
This Learner specializes \link{Learner} for survival analysis tasks.
}
\section{Usage}{

See \link{Learner}.
}

\examples{
library(mlr3)
ids = mlr_learners$ids("^surv")
lrns = mlr_learners$mget(ids)
names(lrns)

# get a specific learner from mlr_learners:
lrn = mlr_learners$get("surv.rpart")
print(lrn)
}
\concept{Learner}