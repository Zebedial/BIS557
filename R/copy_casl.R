#' Soft threshold function.
#'
#' @param a Numeric vector of values to threshold.
#' @param b The soft thresholded value.
#'
#' @return Numeric vector of the soft-thresholded values of a.
#'
#' @author Taylor Arnold, Michael Kane, Bryan Lewis.
#'
#' @references
#'
#' Taylor Arnold, Michael Kane, and Bryan Lewis.
#' \emph{A Computational Approach to Statistical Learning}.
#' Chapman & Hall/CRC Texts in Statistical Science, 2019.
#'
#' @export
casl_util_soft_thresh <-
  function(a, b)
  {
    a[abs(a) <= b] <- 0
    a[a > 0] <- a[a > 0] - b
    a[a < 0] <- a[a < 0] + b
    a
  }

#' Update beta vector using coordinate descent.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param lambda The penalty term.
#' @param alpha Value from 0 and 1; balance between l1/l2 penalty.
#' @param b A vector of warm start coefficients for the algorithm.
#' @param W A vector of sample weights.
#'
#' @return A matrix of regression vectors with ncol(X) columns
#' and length(lambda_vals) rows.
#'
#' @author Taylor Arnold, Michael Kane, Bryan Lewis.
#'
#' @references
#'
#' Taylor Arnold, Michael Kane, and Bryan Lewis.
#' \emph{A Computational Approach to Statistical Learning}.
#' Chapman & Hall/CRC Texts in Statistical Science, 2019.
#'
#' @export
casl_lenet_update_beta <-
  function(X, y, lambda, alpha, b, W)
  {
    WX <- W * X
    WX2 <- W * X^2
    Xb <- X %*% b

    for (i in seq_along(b))
    {
      Xb <- Xb - X[, i] * b[i]
      b[i] <- casl_util_soft_thresh(sum(WX[,i, drop=FALSE] *
                                          (y - Xb)),
                                    lambda*alpha)
      b[i] <- b[i] / (sum(WX2[, i]) + lambda * (1 - alpha))
      Xb <- Xb + X[, i] * b[i]
    }
    b
  }


#' Compute linear elastic net using coordinate descent.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param lambda The penalty term.
#' @param alpha Value from 0 and 1; balance between l1/l2 penalty.
#' @param b Current value of the regression vector.
#' @param tol Numeric tolerance parameter.
#' @param maxit Integer maximum number of iterations.
#' @param W Vector of sample weights.
#'
#' @return Regression vector beta of length ncol(X).
#'
#' @author Taylor Arnold, Michael Kane, Bryan Lewis.
#'
#' @references
#'
#' Taylor Arnold, Michael Kane, and Bryan Lewis.
#' \emph{A Computational Approach to Statistical Learning}.
#' Chapman & Hall/CRC Texts in Statistical Science, 2019.
#'
#' @export
casl_lenet <-
  function(X, y, lambda, alpha = 1, b=matrix(0, nrow=ncol(X), ncol=1),
           tol = 1e-5, maxit=50L, W=rep(1, length(y))/length(y))
  {
    for (j in seq_along(lambda))
    {
      if (j > 1)
      {
        b[,j] <- b[, j-1, drop = FALSE]
      }

      # Update the slope coefficients until they converge.
      for (i in seq(1, maxit))
      {
        b_old <- b[, j]
        b[, j] <- casl_lenet_update_beta(X, y, lambda[j], alpha,
                                         b[, j], W)
        if (all(abs(b[, j] - b_old) < tol)) {
          break
        }
      }
      if (i == maxit)
      {
        warning("Function lenet did not converge.")
      }
    }
    b
  }
