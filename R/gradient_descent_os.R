#' @title An alternative version of gradeint descent method with loss as the out of sample accuracy
#' @description Implement gradient descent for ordinary least square. The loss used this time is the out of sample accuracy calculated by the k
#'  folds cross-validation. The function may take longer time to run due to the more complex structure Gradient descent can only handle design
#'  matrix with full rank.
#'  For design matrix with problem of collinearity, if perfect collinearity presents, the OLS estimate computed by gradient descent
#'  contains redundant estimate corresponding to variables should be omitted; For other cases with strong collinearity, the method may not be
#'  convergent (i.e. the lm_patho data). This function will pass the data to the the function "linear_model" when it cannot be able to handle
#'  the problem
#' @param formula A symbolic description of the model to be fitted. This should be a formula class argument.
#' @param data Specification of a dataframe that contains the variables in the model.
#' @param contrasts A list of contrasts.
#' @param gamma Specification of a learning rate that adjust the OLS estimates along gradient.
#' @param fold.num number of folds specified to conduct the cross-validation
#' @param maxiter Maximum number of iterations for the updating process of OLS estimates.
#' @param tolt A tolerance that bounds the difference between the current SSR and the updated SSR.
#' @return A list of component that imitates the output of lm() function. Including estimated coefficients for predictors specified in the formula.
#' also may return a warning if the iterations exceed the maximum iteration number.
#' @examples
#' data(iris)
#' gradient_descent_os(Sepal.Length ~ ., iris)
#' @export

gradient_descent_os <- function(formula, data, contrasts = NULL, gamma = 0.0001, fold.num = 10, maxiter = 1e6, tolt = 1e-8){

  if (!require("rsample")) install.packages("rsample")
  library(rsample)
  library(foreach)

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
    beta<-matrix(1, nrow = ncol(X))
    count<-0
    diff<-1
    #define the folds
    folds <- vfold_cv(data, v = fold.num)
    #prepare a vector to contain the residuals
    os.resids<-NULL
    #each observation can be used both as assessment and analysis object
    for (i in 1:fold.num) {
      os.resids <- c(os.resids, as.vector(assessment(folds$splits[[i]])[,y.name] - (model.matrix(formula,assessment(folds$splits[[i]])) %*% beta)))
    }
    #compute the initial value of out of sample accuracy
    os.mse.a <- mean(os.resids^2)

    while (count<maxiter & diff>tolt) {

      #Update beta by gamma times the gradient, the changing rate was computed by data in training group, which indicates the whole data set
      beta<-beta - gamma*(2*t(X)%*%X%*%beta - 2*t(X)%*%y)

      os.resids<-NULL
      for (j in 1:fold.num) {
        os.resids <- c(os.resids,as.vector(assessment(folds$splits[[j]])[,y.name] - (model.matrix(formula,assessment(folds$splits[[j]])) %*% beta)))
      }

      os.mse.b <- mean(os.resids^2)
      #Update difference, counter and SSR
      diff<-abs(os.mse.b-os.mse.a)
      count<-count+1
      os.mse.a<-os.mse.b
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


