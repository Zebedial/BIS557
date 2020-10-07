#' @title Optimize Lambda for a given ridge regression function
#' @description This function help to choose optimal lambda parameter from a given set by conducting cross-validation to the models with different
#' lambdas and return the lambda that gives the smallest mean squared error. This function was based on the function ridge_regression.
#'
#' @param formula a symbolic description of the model to be fitted. This should be a formula class argument.
#' @param data Specification of a dataframe that contains the variables in the model.
#' @param ... other inputs that need to be passed to the function ridge_regression.
#' @param folds Number of folds used in the k-fold cross-validation
#' @param lambda.set a set of lambdas to be checked. This should be a vector of values or a sequence of numbers.
#' @return a optimal value from the specified set of lambda which returns the smallest MSE in the cross-validation check, the vector of MSE over all lambdas,
#' and the minimum of the MSE.
#' @examples
#' data(iris)
#' optimize_lambda(Sepal.Length ~ ., iris, seq(0,2,0.01))
#' @export



optimize_lambda <- function(formula, data, lambda.set, folds = 10, ...) {

  if (!require("rsample")) install.packages("rsample")
  if (!require("doParallel")) install.packages("doParallel")
  library(rsample)
  library(foreach)
  library(doParallel)

  #Extract variable names from the model
  var.list<-all.vars(formula)
  y.name<-var.list[1]

  folds <- vfold_cv(data, v = folds)

  resids <- foreach(i = seq_along(lambda.set)) %dopar% {
    foreach(fold = folds$splits, .combine = c) %do% {
      fit <- ridge_regression(formula, analysis(fold), lambda.set[i], ...)
      as.vector(assessment(fold)[,y.name] -
                  as.vector(predict.ridge_regression(fit, assessment(fold))))
    }
  }
  MSE<-NULL
  for (i in 1:length(lambda.set)) {
    MSE<-c(MSE,mean((resids[[i]])^2))
  }
  return(list(opt.lambda = lambda.set[which.min(MSE)], MSE = MSE, min.MSE = min(MSE)))
}

