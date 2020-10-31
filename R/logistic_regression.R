#' @title Multiclass logistic regression
#' @description This function helps to do the preparation works of implememnt multiclass classification by using logistic regression. It returns a matrix of
#' estimated coefficients with different rows corresponding to different comparison. The comparison there means being a specific class vs. not being that class.
#' @param X A specifeid design matrix
#' @param Y The multilevel outcome of interest
#' @param maxit Maximum number of iterations
#' @return A matrix of coefficient estimates corresponding to different comparison.
#' @export
#'
multiclass_logistic <- function(X, Y, maxit=100){
  #number of class
  type <- length(unique(Y))
  #initialize beta
  beta <- matrix(0, nrow = type, ncol = ncol(X))
  beta_old <-  matrix(0, nrow = type, ncol = ncol(X))

  for(i in 1:maxit){
    for (j in 1:type){
      #Updates
      beta_old[j,] <- beta[j,]
      p <- 1 / (1 + exp(- X %*% beta[j,]))
      D <- diag(as.vector(p*(1-p)))
      XtDX <- crossprod(X, D%*%X)
      grad <- t(X) %*% (1*(Y==unique(Y)[j]) - matrix(p, ncol = 1))
      beta[j,] <- beta_old[j,] + solve(XtDX, grad)
    }
  }
  list(coefficients=beta)
}

#' @title Multiclass logistic regression prediction
#' @description This function use the outcome of multilevel logistic regression function to conduct prediction given data.
#' @param X A specifeid design matrix
#' @param Y The multilevel outcome of interest. This is the reference value to evaluate the accuracy of prediction.
#' @param object Output from the multiclass_logistic regression function. (beta matrix)
#' @return A series outcome of prediction including a matrix of conditional probabilities and a crude prediction of Y.
#' @export
#'
predict_logistic <- function(X, Y, object){
  #number of class
  type <- length(unique(Y))
  #coefficients
  beta <- object$coefficients
  p <- matrix(0,nrow = nrow(X),ncol = type)
  sum <- 0
  #Denominator used later
  for (i in 1:type) {
    sum <- sum + exp(X%*%beta[i,])

  }
  for (j in 1:type){
    p[,j] <- exp(X%*%beta[j,])/sum
  }

  #A crude classification that pick higher probability
  pick <- apply(p, 1, which.max)
  fitted.Y <- unique(Y)[pick]

  return(list(fitted.Y=fitted.Y, p=p))
}



