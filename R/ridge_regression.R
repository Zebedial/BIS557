#' Ridge Regression
#'
#' @description Implement a ridge regression function taking into account colinear (or nearly colinear) regression variables.
#'
#' @param formula a formula that accounts for the model of interest
#' @param data a dataframe that contains the variables of interest
#' @param lambda ridge penalty with default value 0
#' @param contrasts an list of contrast
#' @param tol a tolerance to detect collinearity
#' @examples
#' data(iris)
#' ridge_regression(Sepal.Length ~ ., iris, 0.01)
#' @return A list of beta estimates and the formula.
#' @export
#'


ridge_regression <- function(formula, data, lambda = 0, contrasts = NULL, tol = 1e-8) {
  #Extract variable names from the model
  var.list<-all.vars(formula)
  y.name<-var.list[1]

  #Subset the original data frame, in order to get compatible y and X
  data<-model.frame(formula,data)

  #Extract the vector of predicted variable
  Y<-matrix(data[,y.name], ncol = 1)

  #Extract the matrix of predictors
  X<-model.matrix(formula, data, contrasts.arg = contrasts)

  #Singular value decomposition
  svd_x <- svd(X)

  #Truncate the components of decomposition, get rid of the variables induce collinearity problem
  cond.num <- svd_x$d / svd_x$d[1]
  truncate <- max(which(tol < cond.num))
  svd_x$d <- svd_x$d[seq_len(truncate)]
  svd_x$u <- svd_x$u[, seq_len(truncate)]
  svd_x$v <- svd_x$v[, seq_len(truncate)]

  #Compute the estimate, specify the size in case of 1x1 matrices
  Sigma <- diag(svd_x$d, ncol = length(svd_x$d), nrow = length(svd_x$d))
  lambda_I <-  diag(rep(lambda, length(svd_x$d)), ncol = length(svd_x$d), nrow = length(svd_x$d))
  beta <- svd_x$v %*% solve(Sigma^2 + lambda_I) %*% Sigma %*% t(svd_x$u) %*% Y
  beta <- as.vector(beta)
  #Name the coefficients, which makes it consistent to the output of lm()
  names(beta)<-colnames(X)
  ret <- list(coefficients = beta, formula = formula)
  attributes(ret)$formula <- formula
  class(ret) <- "my_lm_ridge"
  return(ret)
}

