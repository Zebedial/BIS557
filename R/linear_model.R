#' @title Linear Model
#' @description Implement a linear regression based on given model and data.
#'
#' @param formula A symbolic description of the model to be fitted. This should be a formula class argument.
#' @param data Specification of a dataframe that contains the variables in the model.
#' @param contrasts A list of contrasts.
#' @return A list of component that imitates the output of lm() function. Including estimated coefficients for predictors specified in the formula.
#' @examples
#' data(iris)
#' linear_model(Sepal.Length ~ ., iris, contrasts)
#' @export

linear_model<-function(formula, data, contrasts=NULL) {

  #Extract variable names from the model
  var.list<-all.vars(formula)
  y.name<-var.list[1]

  #Subset the original data frame, in order to get compatible y and X
  data<-model.frame(formula,data)

  #Extract the vector of predicted variable
  y<-matrix(data[,y.name], ncol = 1)

  #Extract the matrix of predictors
  X<-model.matrix(formula, data, contrasts.arg = contrasts)

  #Computing OLS estimates using QR decomposition. When X matrix is not full ranked, which means problems of collinearity presents,
  #qr.solve function helps automatically adjust X matrix (omit the redundant variable) and return the OLS estimates.
  betahat<-qr.solve(X,y)

  #Since computed OLS return zeros for coefficient estimates of omitted variable (Rather than NA in lm()), substitute zeros with NA
  betahat[betahat==0]<-NA

  betahat <- as.vector(betahat)
  #Name the coefficients, which makes it consistent to the output of lm()
  names(betahat)<-colnames(X)
  #return a list of output, including coefficients
  return(list(coefficients=betahat))
}
