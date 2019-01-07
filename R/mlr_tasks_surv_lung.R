load_lung = function() {
  b = as_data_backend(load_dataset("lung", "survival"))
  TaskSurv$new("lung", b, time = "time", status = "status")
}
