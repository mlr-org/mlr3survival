# Convert to a Survival Task

Convert object to a survival task
([TaskSurv](https://mlr3survival.mlr-org.com/reference/TaskSurv.md)).

## Usage

``` r
as_task_surv(x, ...)

# S3 method for class 'TaskSurv'
as_task_surv(x, clone = FALSE, ...)

# S3 method for class 'data.frame'
as_task_surv(
  x,
  time = "time",
  event = "event",
  time2 = "time2",
  type = "right",
  id = deparse(substitute(x)),
  ...
)

# S3 method for class 'DataBackend'
as_task_surv(
  x,
  time = "time",
  event = "event",
  time2,
  type = "right",
  id = deparse(substitute(x)),
  ...
)
```

## Arguments

- x:

  (`any`)  
  Object to convert, e.g. a
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

- ...:

  (`any`)  
  Additional arguments.

- clone:

  (`logical(1)`)  
  If `TRUE`, ensures that the returned object is not the same as the
  input `x`.

- time:

  (`character(1)`)  
  Name of the column for event time if data is right censored, otherwise
  starting time if interval censored.

- event:

  (`character(1)`)  
  Name of the column giving the event indicator. If data is right
  censored then `"0"` means alive (no event), `"1"` means dead (event).
  If `type` is `"interval"` then "0" means right censored, "1" means
  dead (event), "2" means left censored, and "3" means interval
  censored.

- time2:

  (`character(1)`)  
  Name of the column for ending time of the interval for interval
  censored or counting process data, otherwise ignored.

- type:

  (`character(1)`)  
  Name of the column giving the type of censoring. Available options are
  `"right"` (default), `"left"` and `"interval"`.

- id:

  (`character(1)`)  
  Id for the new task. Defaults to the (deparsed and substituted) name
  of `x`.
