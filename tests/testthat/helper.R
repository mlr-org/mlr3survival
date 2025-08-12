library(checkmate)
library(mlr3)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3survival"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)
