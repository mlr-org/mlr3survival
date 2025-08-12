#' @param event (`character(1)`)\cr
#' Name of the column giving the event indicator.
#' If data is right censored then `"0"` means alive (no event), `"1"` means dead (event).
#' If `type` is `"interval"` then "0" means right censored, "1" means dead (event),
#' "2" means left censored, and "3" means interval censored.
