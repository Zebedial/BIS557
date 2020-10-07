library(testthat)
library(MASS)

context("Test the output of homework 2 - ridge regression function solving collinearity")

test_that("Your ridge_regression() function works in an easy case.", {
  
  data(iris)
  
  fit_lm_ridge <- lm.ridge(Sepal.Length ~ ., iris, lambda = 0.01)
  
  fit_ridge_regression <- ridge_regression(Sepal.Length  ~ ., iris, lambda = 0.01)
  
  expect_equivalent(coef(fit_lm_ridge), fit_ridge_regression$coefficients,
                    tolerance = 0.05)
})

test_that("Your ridge_regression() function works with contrasts.", {
  
  data(iris)
  
  fit_lm_ridge <- lm.ridge(Sepal.Length ~ ., iris,
                           contrasts = list(Species = "contr.sum"), lambda = 0.01)
  
  fit_ridge_regression <- ridge_regression(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"), lambda = 0.01)
  
  expect_equivalent(coef(fit_lm_ridge), fit_ridge_regression$coefficients,
                    tolerance = 0.05)
})
