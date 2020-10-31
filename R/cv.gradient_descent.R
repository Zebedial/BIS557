#' @title Do k-fold cross-validation for gradient descent method
#' @description This function helps to conduct a k-fold cross-validation for the gradient descent method given model and data. The user need to
#' specified input for the function gradient_descent. More details see gradient_descent.
#'
#' @param formula a symbolic description of the model to be fitted. This should be a formula class argument.
#' @param data Specification of a dataframe that contains the variables in the model.
#' @param ... other inputs that need to be passed to the function ridge_regression.
#' @param folds Number of folds used in the k-fold cross-validation
#' @return a list containing the raw vector of residuals and the computed mean squared error (out of sample accuracy)
#' @examples
#' data(iris)
#' cv.gradient_descent(Sepal.Length ~ ., iris)
#' @export


cv.gradient_descent <- function(formula, data, folds = 10, ...){

  if (!require("rsample")) install.packages("rsample")
  library(rsample)
  library(foreach)

  #Extract variable names from the model
  var.list<-all.vars(formula)
  y.name<-var.list[1]

  folds <- vfold_cv(data, v = folds)
  #Raw vector of residuals
  os.resids <- foreach(fold = folds$splits, .combine = c) %do% {
    fit <- gradient_descent(formula, data = analysis(fold), ...)
    as.vector(assessment(fold)[,y.name] -
                as.vector(predict.gradient_descent(fit, assessment(fold))))
  }
  #Return a list containing the vector of residual and the MSE
  return(list(os.resids = os.resids, MSE = mean(os.resids^2)))
}

data(iris)
cv.gradient_descent(Sepal.Length ~ ., iris)

formula <- Sepal.Length ~.
folds <- vfold_cv(iris)
#Raw vector of residuals
os.resids <- foreach(fold = folds$splits, .combine = c) %do% {
  fit <- lm(formula, data = analysis(fold))
  as.vector(assessment(fold)$Sepal.Length -
              as.vector(predict(fit, assessment(fold))))
}
mean(os.resids^2)
