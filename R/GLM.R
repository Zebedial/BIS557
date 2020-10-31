#' @title First-Order GLM (Using Gradient Descent)
#' @description This function provides two ways to imitate the "glm" function, solving the estimated coefficients in a first-order gradient
#' descient feature. The first method implements the straight forward constant learning rate one. The second method implements the adaptive
#' or called the Momentum algorithm of optimzation. An object contains the estimates as well as the tracking vector of difference of estimates
#' along the iterations would be the output.
#' @param form A prespecified formula of interest.
#' @param data A dataset contains the elements of interests.
#' @param mu_fun The inverse link function specified to decode the linear outcome eta to the mean.
#' @param tol Tolerance for the convergence.
#' @param Gamma A learning rate.
#' @param method Indicator of method used. If method=1, do the constant step size iteration. If method=2, do the adaptive (Momentum) one.
#' @param maxit Max times of iteration
#' @param tol A tolerance that helps to contol the cease of the iterations
#' @return An object contains the fitted coefficients and the difference that measures convergence.
#' @examples
#' \dontrun{my_glm(form, data, mu_fun = function(eta) 1/(1+exp(-eta)), method=1)}
#' @export
#'
my_glm <- function(form, data, mu_fun, method, gamma=0.0001, maxit = 1000, tol = 1e-6){

  #Extract the independent variables
  X <- model.matrix(form, data)
  #Create a dataset dropping NAs in regressors, preparing for extracting a Y vector with proper length
  data_no_na <- model.frame(form, data)
  #Identify the name of dependent variable
  y_name <- as.character(form)[2]
  #Extract a vector of response variable
  Y <- as.matrix(data_no_na[,y_name], ncol = 1)

  beta <- matrix(0, ncol(X), ncol = 1)
  beta_diff <- c()
  count <- 0
  #Constant
  if (method==1) {
    for (i in 1:maxit){
      beta_old <- beta
      eta <- X %*% beta_old
      mu <- mu_fun(eta)

      grad <- t(X) %*% (Y-mu)
      beta <- beta_old + gamma*grad
      beta_diff <- c(beta_diff, sum((beta - beta_old)^2))
      #print(tail(beta_diff, 1))
      count <- count+1

      if (tail(beta_diff, 1) <= tol){
        break
      }
    }
  }
  #Adaptive
  if (method==2) {
    #eta.0 <- X %*% beta
    #mu.0 <- mu_fun(eta.0)
    #grad.0 <- t(X) %*% (Y-mu.0)
    #v_old <- grad.0
    v_old <- 0

    frac <- 0.9

    for (i in 1:maxit){
      beta_old <- beta
      eta <- X %*% beta_old
      mu <- mu_fun(eta)

      grad <- t(X) %*% (Y-mu)

      v_new <- frac*v_old + gamma*grad
      beta <- beta_old + v_new
      beta_diff <- c(beta_diff, sum((beta - beta_old)^2))
      v_old <- v_new
      count <- count+1

      #print(tail(beta_diff, 1))

      if (tail(beta_diff, 1) <= tol){
        break
      }
    }
  }
  list(coefficients=beta, beta_diff=beta_diff, count=count)
}


