% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logistic_regression.R
\name{multiclass_logistic}
\alias{multiclass_logistic}
\title{Multiclass logistic regression}
\usage{
multiclass_logistic(X, Y, maxit = 100)
}
\arguments{
\item{X}{A specifeid design matrix}

\item{Y}{The multilevel outcome of interest}

\item{maxit}{Maximum number of iterations}
}
\value{
A matrix of coefficient estimates corresponding to different comparison.
}
\description{
This function helps to do the preparation works of implememnt multiclass classification by using logistic regression. It returns a matrix of
estimated coefficients with different rows corresponding to different comparison. The comparison there means being a specific class vs. not being that class.
}
