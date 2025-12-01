# Prediction Object for Survival

This object stores the predictions returned by a learner of class
[LearnerSurv](https://mlr3survival.mlr-org.com/reference/LearnerSurv.md).

The `task_type` is set to `"surv"`.

For accessing survival and hazard functions, as well as other complex
methods from a PredictionSurv object, see public methods of
[survDistr](https://survdistr.mlr-org.com/reference/survDistr.html) and
example below.

## Super class

[`mlr3::Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)
-\> `PredictionSurv`

## Active bindings

- `truth`:

  (`Surv`)  
  True (observed) outcome.

- `crank`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Access the stored predicted continuous ranking.

- `distr`:

  ([survdistr::survDistr](https://survdistr.mlr-org.com/reference/survDistr.html))  
  Returns the stored survival predictions as a
  [survDistr](https://survdistr.mlr-org.com/reference/survDistr.html)
  distribution object. The `survDistr` API provides methods to transform
  survival data into other quantities such as cumulative hazard, hazard,
  probability density, and cumulative distribution functions.

- `lp`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Access the stored predicted linear predictor.

- `response`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Access the stored predicted survival time.

## Methods

### Public methods

- [`PredictionSurv$new()`](#method-PredictionSurv-new)

- [`PredictionSurv$clone()`](#method-PredictionSurv-clone)

Inherited methods

- [`mlr3::Prediction$filter()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-filter)
- [`mlr3::Prediction$format()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-format)
- [`mlr3::Prediction$help()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-help)
- [`mlr3::Prediction$obs_loss()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-obs_loss)
- [`mlr3::Prediction$print()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-print)
- [`mlr3::Prediction$score()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    PredictionSurv$new(
      task = NULL,
      row_ids = task$row_ids,
      truth = task$truth(),
      crank = NULL,
      distr = NULL,
      lp = NULL,
      response = NULL,
      check = TRUE,
      data_type = "surv",
      interp_meth = "const_surv"
    )

#### Arguments

- `task`:

  ([TaskSurv](https://mlr3survival.mlr-org.com/reference/TaskSurv.md))  
  Task, used to extract defaults for `row_ids` and `truth`.

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row ids of the predicted observations, i.e. the row ids of the test
  set.

- `truth`:

  ([`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html))  
  True (observed) response.

- `crank`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Numeric vector of predicted continuous rankings (or relative risks).
  One element for each observation in the test set. For a pair of
  continuous ranks, a higher rank indicates that the observation is more
  likely to experience the event.

- `distr`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  A matrix of predicted survival probabilities, where rows =
  observations and columns correspond to time points.

- `lp`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Numeric vector of linear predictor scores. One element for each
  observation in the test set. \\lp = X\beta\\ where \\X\\ is a matrix
  of covariates and \\\beta\\ is a vector of estimated coefficients.

- `response`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Numeric vector of predicted survival times. One element for each
  observation in the test set.

- `check`:

  (`logical(1)`)  
  If `TRUE`, performs argument checks and predict type conversions.

- `data_type`:

  (`character(1)`)  
  Type of data (e.g. survival or hazard) stored in `distr`.

- `interp_meth`:

  (`character(1)`)  
  Type of interpolation used in subsequent methods of the `survDistr`
  object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PredictionSurv$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tsk("lung")
learner = lrn("surv.kaplan")
part = partition(task)
p = learner$train(task, part$train)$predict(task, part$test)
head(as.data.table(p))
#>    row_ids  time status    crank     distr
#>      <int> <num> <lgcl>    <num>    <list>
#> 1:      15   371   TRUE 81.78041 <list[1]>
#> 2:      16   520   TRUE 81.78041 <list[1]>
#> 3:      18   118   TRUE 81.78041 <list[1]>
#> 4:      20    12   TRUE 81.78041 <list[1]>
#> 5:      29   460   TRUE 81.78041 <list[1]>
#> 6:      30   153   TRUE 81.78041 <list[1]>

p$distr # survDistr
#> A [55 x 107] survival matrix
#> Number of observations: 55
#> Number of time points: 107
#> Interpolation method: Piece-wise Constant Survival 

# survival probabilities of the 4 test observations at two time points
p$distr$survival(times = c(80, 100))[1:4, ]
#>             80       100
#> [1,] 0.9026549 0.8584071
#> [2,] 0.9026549 0.8584071
#> [3,] 0.9026549 0.8584071
#> [4,] 0.9026549 0.8584071
```
