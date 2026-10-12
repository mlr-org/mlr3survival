# Kaplan-Meier Estimator

Calls
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
during training to estimate the survival function at the **unique event
times** from the training set. During prediction, each test observation
is assigned the same survival probabilities at these times. The risk
score (`crank`) is calculated as the sum of the cumulative hazard
function (also called expected mortality) at the event times.

## Dictionary

This [Learner](https://mlr3.mlr-org.com/reference/Learner.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html) or
with the associated sugar function
[lrn()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    mlr_learners$get("surv.kaplan")
    lrn("surv.kaplan")

## Meta Information

- Task type: “surv”

- Predict Types: “crank”, “distr”

- Feature Types: “logical”, “integer”, “numeric”, “character”, “factor”,
  “ordered”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3),
  [mlr3survival](https://CRAN.R-project.org/package=mlr3survival),
  [survival](https://CRAN.R-project.org/package=survival)

## Parameters

Empty ParamSet

## References

Kaplan EL, Meier P (1958). “Nonparametric Estimation from Incomplete
Observations.” *Journal of the American Statistical Association*,
**53**(282), 457–481.
[doi:10.1080/01621459.1958.10501452](https://doi.org/10.1080/01621459.1958.10501452)
.

## See also

Other survival learners:
[`mlr_learners_surv.coxph`](https://mlr3survival.mlr-org.com/reference/mlr_learners_surv.coxph.md)

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3survival::LearnerSurv`](https://mlr3survival.mlr-org.com/reference/LearnerSurv.md)
-\> `LearnerSurvKaplan`

## Active bindings

- `native_model`:

  ([survival::survfit](https://rdrr.io/pkg/survival/man/survfit.html))  
  The fitted model.

## Methods

### Public methods

- [`LearnerSurvKaplan$new()`](#method-LearnerSurvKaplan-new)

- [`LearnerSurvKaplan$importance()`](#method-LearnerSurvKaplan-importance)

- [`LearnerSurvKaplan$selected_features()`](#method-LearnerSurvKaplan-selected_features)

- [`LearnerSurvKaplan$clone()`](#method-LearnerSurvKaplan-clone)

Inherited methods

- [`mlr3::Learner$base_learner()`](https://mlr3.mlr-org.com/reference/Learner.html#method-base_learner)
- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/reference/Learner.html#method-configure)
- [`mlr3::Learner$encapsulate()`](https://mlr3.mlr-org.com/reference/Learner.html#method-encapsulate)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$print()`](https://mlr3.mlr-org.com/reference/Learner.html#method-print)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/reference/Learner.html#method-reset)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerSurvKaplan$new()

------------------------------------------------------------------------

### Method `importance()`

All features have a score of `0` for this learner. This method exists
solely for compatibility with the `mlr3` ecosystem, as this learner is
used as a fallback for other survival learners that require an
`importance()` method.

#### Usage

    LearnerSurvKaplan$importance()

#### Returns

Named [`numeric()`](https://rdrr.io/r/base/numeric.html).

------------------------------------------------------------------------

### Method `selected_features()`

Selected features are always the empty set for this learner. This method
is implemented only for compatibility with the `mlr3` API, as this
learner does not perform feature selection.

#### Usage

    LearnerSurvKaplan$selected_features()

#### Returns

`character(0)`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerSurvKaplan$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

# Define the Learner
learner = lrn("surv.kaplan")
learner
#> 
#> ── <LearnerSurvKaplan> (surv.kaplan): Kaplan-Meier Estimator ───────────────────
#> • Model: -
#> • Parameters: list()
#> • Packages: mlr3, mlr3survival, and survival
#> • Predict Types: [crank] and distr
#> • Feature Types: logical, integer, numeric, character, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: importance, missings, selected_features, and weights
#> • Other settings: use_weights = 'use', predict_raw = 'FALSE'

# Define a Task
task = tsk("lung")

# Stratification based on event
task$set_col_roles(cols = "status", add_to = "stratum")

# Create train and test set
part = partition(task)

# Train the learner on the train set
learner$train(task, row_ids = part$train)
learner$native_model
#> Call: survfit(formula = task$formula(1), data = task$data(cols = task$target_names), 
#>     weights = NULL)
#> 
#>        n events median 0.95LCL 0.95UCL
#> [1,] 112     81    310     267     390

# Make predictions for the test set
predictions = learner$predict(task, row_ids = part$test)
predictions
#> 
#> ── <PredictionSurv> for 56 observations: ───────────────────────────────────────
#>  row_ids time status    crank     distr
#>        9  567   TRUE 52.63894 <list[1]>
#>       13  301   TRUE 52.63894 <list[1]>
#>       21  473   TRUE 52.63894 <list[1]>
#>      ---  ---    ---      ---       ---
#>      158  185  FALSE 52.63894 <list[1]>
#>      159  222  FALSE 52.63894 <list[1]>
#>      168  177  FALSE 52.63894 <list[1]>

# Score the predictions
predictions$score()
#> surv.cindex 
#>         0.5 
```
