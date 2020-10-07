library(testthat)
library(glmnet)

context("Test the output of homework 2 - the optimized lambda in a ridge regression")

test_that("Your optimize_lambda function gives proper choice of lambda", {
  
  data(iris)
  
  lambdas.set <- seq(0,1,0.01)
  
  fit_opt <- optimize_lambda(Sepal.Length ~ ., iris, lambda.set = lambdas.set)
  
  fit_glmnet <- cv.glmnet(model.matrix(Sepal.Length ~ ., iris), as.matrix(iris[,1]), lambda = lambdas.set, alpha = 0)
  
  expect_equivalent(fit_opt$opt.lambda, fit_glmnet$lambda.min,
                    tolerance = 0.05)
})


