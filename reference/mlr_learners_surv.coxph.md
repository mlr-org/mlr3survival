# Cox Proportional Hazards

Calls [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)
during training. During prediction, the linear predictor is estimated
using
[`survival::predict.coxph()`](https://rdrr.io/pkg/survival/man/predict.coxph.html)
while the survival distribution using the Breslow estimator using
[`survival::survfit.coxph()`](https://rdrr.io/pkg/survival/man/survfit.coxph.html)

## Dictionary

This [Learner](https://mlr3.mlr-org.com/reference/Learner.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html) or
with the associated sugar function
[lrn()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    mlr_learners$get("surv.coxph")
    lrn("surv.coxph")

## Meta Information

- Task type: “surv”

- Predict Types: “crank”, “distr”, “lp”

- Feature Types: “logical”, “integer”, “numeric”, “factor”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3),
  [mlr3survival](https://CRAN.R-project.org/package=mlr3survival),
  [survival](https://CRAN.R-project.org/package=survival)

## Parameters

|  |  |  |  |  |
|----|----|----|----|----|
| Id | Type | Default | Levels | Range |
| ties | character | efron | efron, breslow, exact | \- |
| singular.ok | logical | TRUE | TRUE, FALSE | \- |
| type | character | efron | efron, aalen, kalbfleisch-prentice | \- |
| stype | integer | 2 |  | \\\[1, 2\]\\ |

## References

Cox DR (1972). “Regression Models and Life-Tables.” *Journal of the
Royal Statistical Society: Series B (Methodological)*, **34**(2),
187–202.
[doi:10.1111/j.2517-6161.1972.tb00899.x](https://doi.org/10.1111/j.2517-6161.1972.tb00899.x)
.

## See also

Other survival learners:
[`mlr_learners_surv.kaplan`](https://mlr3survival.mlr-org.com/reference/mlr_learners_surv.kaplan.md)

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3survival::LearnerSurv`](https://mlr3survival.mlr-org.com/reference/LearnerSurv.md)
-\> `LearnerSurvCoxPH`

## Methods

### Public methods

- [`LearnerSurvCoxPH$new()`](#method-LearnerSurvCoxPH-new)

- [`LearnerSurvCoxPH$clone()`](#method-LearnerSurvCoxPH-clone)

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
- [`mlr3::Learner$selected_features()`](https://mlr3.mlr-org.com/reference/Learner.html#method-selected_features)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerSurvCoxPH$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerSurvCoxPH$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

# Define the Learner
learner = lrn("surv.coxph")
learner
#> 
#> ── <LearnerSurvCoxPH> (surv.coxph): Cox Proportional Hazards ───────────────────
#> • Model: -
#> • Parameters: list()
#> • Packages: mlr3, mlr3survival, and survival
#> • Predict Types: [crank], distr, and lp
#> • Feature Types: logical, integer, numeric, and factor
#> • Encapsulation: none (fallback: -)
#> • Properties: weights
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
#> Call:
#> survival::coxph(formula = task$formula(), data = task$data(), 
#>     model = TRUE)
#> 
#>                 coef  exp(coef)   se(coef)      z       p
#> age        0.0112226  1.0112858  0.0142049  0.790 0.42950
#> meal.cal  -0.0001446  0.9998554  0.0004184 -0.346 0.72970
#> pat.karno -0.0167315  0.9834077  0.0098372 -1.701 0.08897
#> ph.ecog    0.7279428  2.0708161  0.2806164  2.594 0.00948
#> ph.karno   0.0274259  1.0278055  0.0136580  2.008 0.04464
#> sexm       0.3791073  1.4609798  0.2420168  1.566 0.11724
#> wt.loss   -0.0264181  0.9739278  0.0114005 -2.317 0.02049
#> 
#> Likelihood ratio test=17.94  on 7 df, p=0.01225
#> n= 112, number of events= 81 

# Make predictions for the test set
predictions = learner$predict(task, row_ids = part$test)
predictions
#> 
#> ── <PredictionSurv> for 56 observations: ───────────────────────────────────────
#>  row_ids time status      crank         lp     distr
#>        4  310   TRUE  0.8737011  0.8737011 <list[1]>
#>        6  218   TRUE -0.2003632 -0.2003632 <list[1]>
#>        7  166   TRUE  0.3892358  0.3892358 <list[1]>
#>      ---  ---    ---        ---        ---       ---
#>      163  197  FALSE  0.3359723  0.3359723 <list[1]>
#>      164  203  FALSE  0.1232581  0.1232581 <list[1]>
#>      167  174  FALSE  0.5195508  0.5195508 <list[1]>

# Score the predictions
predictions$score()
#> surv.cindex 
#>   0.6748682 
```
