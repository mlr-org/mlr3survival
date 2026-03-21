#' @examples
#'
#' # Define the Learner
#' learner = lrn("<%= id %>")
#' learner
#'
#' # Define a Task
#' task = tsk("lung")
#'
#' # Stratification based on event
#' task$set_col_roles(cols = "status", add_to = "stratum")
#'
#' # Create train and test set
#' part = partition(task)
#'
#' # Train the learner on the train set
#' learner$train(task, row_ids = part$train)
#' learner$native_model
#'
#' # Make predictions for the test set
#' predictions = learner$predict(task, row_ids = part$test)
#' predictions
#'
#' # Score the predictions
#' predictions$score()
#'
