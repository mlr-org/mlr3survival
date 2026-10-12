# Changelog

## mlr3survival (development version)

- Initial porting of `mlr3proba` code.
- Fix compatibility with `mlr3` (1.3.0)
- Use `survdistr` 0.0.3 CRAN version
- Learner `surv.kaplan` improvements: predictions only at event time
  points be default
- Add active field `native_model` for Kaplan-Meier (mlr3 \>= 1.4.0)
