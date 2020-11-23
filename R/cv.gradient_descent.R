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


gradient_descent <- function(formula, data, contrasts = NULL, gamma = 0.0001, maxiter = 1e6, tolt = 1e-12){

  #Extract variable names from the model
  var.list<-all.vars(formula)
  y.name<-var.list[1]

  #Subset the original data frame, in order to get compatible y and X
  data<-model.frame(formula,data)

  #Extract the vector of predicted variable
  y<-matrix(data[,y.name], ncol = 1)

  #Extract the matrix of predictors
  X<-model.matrix(formula, data, contrasts.arg = contrasts)

  #Gradient_descent can only handle X matrix with full rank. For X matrix with problem of collinearity, if perfect collinearity presents, the OLS estimate
  #computed by gradient descent contains redundant estimate corresponding to variables should be omitted; for other strong collinearity, the method may not
  #be convergent (i.e. the lm_patho data).
  if (qr(X)$rank==dim(X)[2]) {
    #Initialize a vector of beta, a counter and a starter of difference
    beta<-matrix(1, nrow = ncol(X))
    count<-0
    diff<-1

    #Sum of squared residuals (error) computed before the update of beta
    ss.a<-t(y)%*%y - 2*t(y)%*%X%*%beta + t(beta)%*%t(X)%*%X%*%beta

    while (count<maxiter & diff>tolt) {

      #Update beta by gamma times the gradient
      beta<-beta - gamma*(2*t(X)%*%X%*%beta - 2*t(X)%*%y)
      #Sum of square (error) computed after the update of beta
      ss.b<-t(y)%*%y - 2*t(y)%*%X%*%beta + t(beta)%*%t(X)%*%X%*%beta
      #Update difference, counter and SSR
      diff<-abs(ss.b-ss.a)
      count<-count+1
      ss.a<-ss.b
    }

    beta <- as.vector(beta)
    #Name the coefficients, which makes it consistent to the output of lm()
    names(beta)<-colnames(X)
    ret <- list(coefficients = beta, formula = formula)
    attributes(ret)$formula <- formula
    class(ret) <- "my_lm_gradient_descent"

    if (diff>tolt) {
      print("Looping over the maximum iteration time. Difference is still larger than the tolerance!")
    } else {
      return(ret)
    }
    #When matrix X is not of full rank, give a message and pass the data to linear_model
  } else {
    warning("Data is not compatible with the gradient descent method. OLS estimates are solved by linear_model")
    return(linear_model(formula,data))
  }
}

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
