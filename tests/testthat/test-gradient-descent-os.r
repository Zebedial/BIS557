library(testthat)

context("Test the output of homework 2 - gradient descent function with loss as the out-of-sample accuracy")

test_that("You gradient_descent_os() function works in an easy case.", {
  
  data(iris)
  
  fit_gd <- gradient_descent_os(Sepal.Length ~ ., iris)
  
  fit_lm <- lm(Sepal.Length  ~ ., iris)
  
  expect_equivalent(fit_lm$coefficients, fit_gd$coefficients,
                    tolerance = 0.2)
})

test_that("You gradient_descent_os() function works with contrasts.", {
  
  data(iris)
  
  fit_gd <- gradient_descent_os(Sepal.Length ~ ., iris,
                                  contrasts = list(Species = "contr.sum"))
  
  fit_lm <- lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))
  
  expect_equivalent(fit_lm$coefficients, fit_gd$coefficients,
                    tolerance = 0.2)
})

test_that("Your gradient_descent_os() function works in a tougher case.", {
  
  data(lm_patho)
  
  fit_gd <- gradient_descent_new(y ~., lm_patho)
  
  fit_lm <- lm(y ~., lm_patho)
  
  expect_equivalent(fit_lm$coefficients, fit_gd$coefficients,
                    tolerance = 1e-3)
})