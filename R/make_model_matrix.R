#' @title Produce a list of outcome as well as model matrix
#' @description This function helps to generate the outcome vector and the model matrix based on the model and data given
#' @param form A formula that denotes the model of interest
#' @param data A dataset that contains the variables of interest
#' @export
#' @examples
#'   data("iris")
#'   form <- Sepal.Length ~.
#' make_model_matrices(form, iris)

make_model_matrix <- function(form, data) {
  d_no_na <- model.frame(form,data) 
  X <- model.matrix(form, d_no_na)
  y_name <- as.character(form)[2]
  Y <- matrix(d_no_na[,y_name], ncol = 1)
  list(X=X, Y=Y)
}