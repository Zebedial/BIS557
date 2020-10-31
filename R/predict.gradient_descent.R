#' Formulate prediction function for gradient descent
#'
#' @param object ridge_regression object, see ridge_regression
#' @param ... input of dataframe
#' @return Predicted value of variable of interest given X's.
#' @export
#'
#'
predict.gradient_descent <- function(object, ...) {
  #Make the data input a list
  dots <- list(...)
  data <- dots[[1]]

  # check for bad arg
  if (!inherits(data, "data.frame")) {
    stop("The second argument should be a data frame.")
  }

  # create new model matrix and predict
  X <- model.matrix(attributes(object)$formula, data)
  X %*% object$coefficients
}
