test_that("unloading cleans up registrations and preserves unrelated hooks", {
  ns = asNamespace("mlr3survival")
  on_load = ns$.onLoad
  # Real package loading assigns the logger before the namespace is locked.
  environment(on_load) = new.env(parent = ns)
  load_event = packageEvent("mlr3", "onLoad")
  unload_event = packageEvent("mlr3survival", "onUnload")
  load_hooks = getHook(load_event)
  unload_hooks = getHook(unload_event)

  dictionaries = list(mlr_tasks, mlr_learners, mlr_measures)
  keys = list(names(ns$tasks), names(ns$learners), names(ns$measures))
  other_keys = Map(function(dictionary, ids) setdiff(dictionary$keys(), ids), dictionaries, keys)
  fields = c(
    "task_col_roles",
    "task_properties",
    "measure_properties",
    "default_measures",
    "learner_properties",
    "learner_predict_types"
  )
  other_reflections = map(fields, function(field) {
    mlr_reflections[[field]][
      setdiff(names(mlr_reflections[[field]]), "surv")
    ]
  })
  other_task_types = mlr_reflections$task_types[!"surv"]

  tryCatch(
    {
      # IDEs and other packages can install hooks without the metadata mlr3misc expects.
      unrelated_hook = local(function(...) NULL)
      setHook(load_event, unrelated_hook, action = "append")
      setHook(unload_event, unrelated_hook, action = "append")
      expected_load_hooks = getHook(load_event)
      expected_unload_hooks = getHook(unload_event)

      # Repeated cycles catch duplicate registrations and hooks left behind by test() reloads.
      for (i in seq_len(2L)) {
        on_load(NULL, "mlr3survival")
        expect_identical(getHook(load_event), expected_load_hooks)
        expect_identical(getHook(unload_event), expected_unload_hooks)
        for (j in seq_along(dictionaries)) {
          expect_true(all(dictionaries[[j]]$has(keys[[j]])))
        }
        expect_equal(sum(mlr_reflections$task_types$type == "surv"), 1L)
        for (field in fields) {
          expect_false(is.null(mlr_reflections[[field]]$surv))
        }

        # Shared entries may already be missing when another package has cleaned up first.
        if (i == 2L) {
          for (j in seq_along(dictionaries)) {
            dictionaries[[j]]$remove(head(keys[[j]], 1L))
          }
        }

        # Run user hooks before .onUnload, as namespace unloading does, while keeping
        # the test runner's namespace loaded. This tests cleanup, not DLL or S3 unloading.
        expect_silent({
          for (hook in rev(getHook(unload_event))) {
            hook("mlr3survival", NULL)
          }
          ns$.onUnload(NULL)
        })
        expect_silent(ns$.onUnload(NULL))

        expect_identical(getHook(load_event), expected_load_hooks)
        expect_identical(getHook(unload_event), expected_unload_hooks)
        for (j in seq_along(dictionaries)) {
          expect_setequal(dictionaries[[j]]$keys(), other_keys[[j]])
        }
        expect_identical(mlr_reflections$task_types, other_task_types)
        for (j in seq_along(fields)) {
          expect_null(mlr_reflections[[fields[j]]]$surv)
          expect_identical(mlr_reflections[[fields[j]]], other_reflections[[j]])
        }
      }
    },
    finally = {
      # Restore registrations and hooks even after a failure so other tests remain independent.
      on_load(NULL, "mlr3survival")
      setHook(load_event, load_hooks, action = "replace")
      setHook(unload_event, unload_hooks, action = "replace")
    }
  )
})
