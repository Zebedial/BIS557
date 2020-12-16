

# Create list of weights to describe a dense neural network.
##
#Args:
# sizes: A vector giving the size of each layer, including
# the input and output layers.
##
#Returns:
# A list containing initialized weights and biases.
casl_nn_make_weights <- function(sizes) {
  L <- length(sizes) - 1L
  weights <- vector("list", L)
  for (j in seq_len(L)) {
    w <- matrix(rnorm(sizes[j] * sizes[j + 1L]),
                ncol = sizes[j],
                nrow = sizes[j + 1L])
    weights[[j]] <- list(w=w,
                        b=rnorm(sizes[j + 1L]))
  }
  weights
}

# Apply a rectified linear unit (ReLU) to a vector/matrix.
##  Args:

# v: A numeric vector or matrix.
##Returns:
# The original input with negative values truncated to zero.
casl_util_ReLU <- function(v) {
  v[v < 0] <- 0
  v
}

# Apply derivative of the rectified linear unit (ReLU).
##Args:
  # v: A numeric vector or matrix.
  ##Returns:
# Sets positive values to 1 and negative values to zero.
casl_util_ReLU_p <- function(v){
    p <- v * 0
    p[v > 0] <- 1
    p
}

# Derivative of the mean squared error (MSE) function.
##Args:
  # y: A numeric vector of responses.
  # a: A numeric vector of predicted responses.
  ##Returns:
  # Returned current derivative the MSE function.
casl_util_mse_p <- function(y, a) {
  (a - y)
}

# Apply forward propagation to a set of NN weights and biases.
##Args:
  # x: A numeric vector representing one row of the input.
  # weights: A list created by casl_nn_make_weights.
  # sigma: The activation function.
  ##Returns:
  # A list containing the new weighted responses (z) and
  # activations (a).
casl_nn_forward_prop <- function(x, weights, sigma) {
  L <- length(weights)
  z <- vector("list", L)
  a <- vector("list", L)
  for (j in seq_len(L)){
    a_j1 <- if(j == 1) x else a[[j - 1L]]
    z[[j]] <- weights[[j]]$w %*% a_j1 + weights[[j]]$b
    a[[j]] <- if (j != L) sigma(z[[j]]) else z[[j]]
  }
  list(z=z, a=a)
}

# Apply backward propagation algorithm.
##Args:
  # x: A numeric vector representing one row of the input.
  # y: A numeric vector representing one row of the response.
  # weights: A list created by casl_nn_make_weights.
  # f_obj: Output of the function casl_nn_forward_prop.
  # sigma_p: Derivative of the activation function.
  # f_p: Derivative of the loss function.
  ##Returns:
  # A list containing the new weighted responses (z) and
  # activations (a).
casl_nn_backward_prop <- function(x, y, weights, f_obj, sigma_p, f_p) {
  z <- f_obj$z; a <- f_obj$a
  L <- length(weights)
  grad_z <- vector("list", L)
  grad_w <- vector("list", L)
  for (j in rev(seq_len(L)))
  {
    if (j == L)
    {
      grad_z[[j]] <- f_p(y, a[[j]])
    } else {
      grad_z[[j]] <- (t(weights[[j + 1]]$w) %*%
                        grad_z[[j + 1]]) * sigma_p(z[[j]])
    }
    a_j1 <- if(j == 1) x else a[[j - 1L]]
    grad_w[[j]] <- grad_z[[j]] %*% t(a_j1)
  }
  list(grad_z=grad_z, grad_w=grad_w)
}

# Apply stochastic gradient descent (SGD) to estimate NN.
##Args:
# X: A numeric data matrix.
  # y: A numeric vector of responses.
  # sizes: A numeric vector giving the sizes of layers in
  # the neural network.
  # epochs: Integer number of epochs to computer.
  # eta: Positive numeric learning rate.
  # weights: Optional list of starting weights.
  ##Returns:
  # A list containing the trained weights for the network.
casl_nn_sgd <- function(X, y, sizes, epochs, eta, weights=NULL){
  if (is.null(weights))
  {
    weights <- casl_nn_make_weights(sizes)
  }
  for (epoch in seq_len(epochs))
  {
    for (i in seq_len(nrow(X)))
    {
      f_obj <- casl_nn_forward_prop(X[i,], weights,
                                    casl_util_ReLU)
      b_obj <- casl_nn_backward_prop(X[i,], y[i,], weights,
                                     f_obj, casl_util_ReLU_p,
                                     casl_util_mse_p)
      for (j in seq_along(b_obj))
      {
        weights[[j]]$b <- weights[[j]]$b -
          eta * b_obj$grad_z[[j]]
        weights[[j]]$w <- weights[[j]]$w -
          eta * b_obj$grad_w[[j]]
      }
    }
  }
  weights
}

# Predict values from a training neural network.
##Args:
  # weights: List of weights describing the neural network.
  # X_test: A numeric data matrix for the predictions.
  ##Returns:
  # A matrix of predicted values.
casl_nn_predict <- function(weights, X_test) {
  p <- length(weights[[length(weights)]]$b)
  y_hat <- matrix(0, ncol = p, nrow = nrow(X_test))
  for (i in seq_len(nrow(X_test)))
  {
    a <- casl_nn_forward_prop(X_test[i,], weights,
                              casl_util_ReLU)$a
    y_hat[i, ] <- a[[length(a)]]
  }
  y_hat
}

X <- matrix(runif(1000, min=-1, max=1), ncol=1)
y <- X[,1,drop = FALSE]^2 + rnorm(1000, sd = 0.1)
weights <- casl_nn_sgd(X, y, sizes=c(1, 25, 1),epochs=25, eta=0.01)
y_pred <- casl_nn_predict(weights, X)

#plot(X,y)
#plot(X,y_pred)



# Create list of weights and momentum to describe a NN.
#Args: # sizes: A vector giving the size of each layer, including
  # the input and output layers.
  ##Returns:
  # A list containing initialized weights, biases, and
  # momentum.
casl_nn_make_weights_mu <- function(sizes){
  L <- length(sizes) - 1L
  weights <- vector("list", L)
  for (j in seq_len(L))
  {
    w <- matrix(rnorm(sizes[j] * sizes[j + 1L],
                     sd = 1/sqrt(sizes[j])),
                ncol = sizes[j],
                nrow = sizes[j + 1L])
    v <- matrix(0,
                ncol = sizes[j],
                nrow = sizes[j + 1L])
    weights[[j]] <- list(w=w,
                         v=v,
                         b=rnorm(sizes[j + 1L]))
  }
  weights
}

# Apply stochastic gradient descent (SGD) to estimate NN.
##Args:
  # X: A numeric data matrix.
  # y: A numeric vector of responses.
  # sizes: A numeric vector giving the sizes of layers in
  # the neural network.
  # epochs: Integer number of epochs to computer.
  # eta: Positive numeric learning rate.
  # mu: Non-negative momentum term.
  # l2: Non-negative penalty term for l2-norm.
  # weights: Optional list of starting weights.
  ##Returns:
  # A list containing the trained weights for the network.
casl_nn_sgd_mu <- function(X, y, sizes, epochs, eta, mu=0, l2=0, weights=NULL) {
  if (is.null(weights))
  {
    weights <- casl_nn_make_weights_mu(sizes)
  }
  for (epoch in seq_len(epochs))
  {
    for (i in seq_len(nrow(X)))
    {
      f_obj <- casl_nn_forward_prop(X[i, ], weights,
                                    casl_util_ReLU)
      b_obj <- casl_nn_backward_prop(X[i, ], y[i, ], weights,
                                     f_obj, casl_util_ReLU_p,
                                     casl_util_mse_p)
      for (j in seq_along(b_obj))
      {
        weights[[j]]$b <- weights[[j]]$b -
          eta * b_obj$grad_z[[j]]
        weights[[j]]$v <- mu * weights[[j]]$v -
          eta * b_obj$grad_w[[j]]
        weights[[j]]$w <- (1 - eta * l2) *
          weights[[j]]$w +
          weights[[j]]$v
      }
    }
  }
  weights
}

l2_norm <- rep(NA_real_, 3)
l2_vals <- c(0, 0.01, 0.04, 0.05)
weights_start <- casl_nn_make_weights(sizes=c(1, 10, 1))
for (i in seq_along(l2_vals))
{
  weights <- casl_nn_sgd_mu(X, y, weights=weights_start,
                            epochs=10, eta=0.1,
                            l2=l2_vals[i])
  l2_norm[i] <- sum((weights[[1]]$w)^2)
}
l2_norm

# Apply the softmax function to a vector.
##Args:
  # z: A numeric vector of inputs.
  ##Returns:
  # Output after applying the softmax function.
casl_util_softmax <- function(z) {
   exp(z) / sum(exp(z))
}


# Apply forward propagation to for a multinomial NN.
##Args:# x: A numeric vector representing one row of the input.
  # weights: A list created by casl_nn_make_weights.
  # sigma: The activation function.
##Returns:
  # A list containing the new weighted responses (z) and
  # activations (a).
casl_nnmulti_forward_prop <- function(x, weights, sigma) {
  L <- length(weights)
  z <- vector("list", L)
  a <- vector("list", L)
  for (j in seq_len(L))
  {
    a_j1 <- if(j == 1) x else a[[j - 1L]]
    z[[j]] <- weights[[j]]$w %*% a_j1 + weights[[j]]$b
    if (j != L) {
      a[[j]] <- sigma(z[[j]])
    } else {
       a[[j]] <- casl_util_softmax(z[[j]])
    }
  }
  list(z=z, a=a)
}

# Apply backward propagation algorithm for a multinomial NN.
##Args:# x: A numeric vector representing one row of the input.
  # y: A numeric vector representing one row of the response.
  # weights: A list created by casl_nn_make_weights.
  # f_obj: Output of the function casl_nn_forward_prop.
  # sigma_p: Derivative of the activation function.
  ##Returns:
  # A list containing the new weighted responses (z) and
  # activations (a).
casl_nnmulti_backward_prop <- function(x, y, weights, f_obj, sigma_p) {
  z <- f_obj$z; a <- f_obj$a
  L <- length(weights)
  grad_z <- vector("list", L)
  grad_w <- vector("list", L)
  for (j in rev(seq_len(L)))
  {
    if (j == L)
    {
      grad_z[[j]] <- a[[j]] - wt*y
    } else {
      grad_z[[j]] <- (t(weights[[j + 1L]]$w) %*%
                        grad_z[[j + 1L]]) * sigma_p(z[[j]])
    }
    a_j1 <- if(j == 1) x else a[[j - 1L]]
    grad_w[[j]] <- grad_z[[j]] %*% t(a_j1)
  }
  list(grad_z=grad_z, grad_w=grad_w)
}

# Apply stochastic gradient descent (SGD) for multinomial NN.
##Args:
  # X: A numeric data matrix.
  # y: A numeric vector of responses.
  # sizes: A numeric vector giving the sizes of layers in
  # the neural network.
  # epochs: Integer number of epochs to computer.
  # eta: Positive numeric learning rate.
  # mu: Non-negative momentum term.
  # l2: Non-negative penalty term for l2-norm.
  # weights: Optional list of starting weights.
  ## Returns:
  # A list containing the trained weights for the network.
casl_nnmulti_sgd <- function(X, y, sizes, epochs, eta, mu=0, l2=0, weights=NULL) {
  if (is.null(weights))
  {
    weights <- casl_nn_make_weights_mu(sizes)
  }
  for (epoch in seq_len(epochs))
  {
    for (i in seq_len(nrow(X)))
    {
      f_obj <- casl_nnmulti_forward_prop(X[i, ], weights,
                                        casl_util_ReLU)
      b_obj <- casl_nnmulti_backward_prop(X[i, ], y[i, ],
                                        weights, f_obj,
                                        casl_util_ReLU_p)
      for (j in seq_along(b_obj))
      {
        weights[[j]]$b <- weights[[j]]$b -
          eta * b_obj$grad_z[[j]]
        weights[[j]]$v <- mu * weights[[j]]$v -
          eta * b_obj$grad_w[[j]]
        weights[[j]]$w <- (1 - eta * l2) *
          weights[[j]]$w +
          weights[[j]]$v
      }
    }
  }
  weights
}

# Predict values from training a multinomial neural network.
##Args:# weights: List of weights describing the neural network.
  # X_test: A numeric data matrix for the predictions.
  ##Returns:
  # A matrix of predicted values.
casl_nnmulti_predict <- function(weights, X_test) {
  p <- length(weights[[length(weights)]]$b)
  y_hat <- matrix(0, ncol=p, nrow=nrow(X_test))
  for (i in seq_len(nrow(X_test))) {
    a <- casl_nnmulti_forward_prop(X_test[i, ], weights,
                                     casl_util_ReLU)$a
    y_hat[i,] <- a[[length(a)]]
  }
  y_hat
}

X <- matrix(runif(1000, min=-1, max=1), ncol=1)
y <- X[, 1, drop=FALSE]^2 + rnorm(1000, sd=0.1)
y <- cbind(as.numeric(y > 0.3), as.numeric(y <= 0.3))
weights <- casl_nnmulti_sgd(X, y, sizes=c(1, 25, 2),
                            epochs=25L, eta=0.01)
y_pred <- casl_nnmulti_predict(weights, X)




















setwd("C:\\Users\\ASUS\\Desktop\\557 project")
fulldata <- read.csv("HH_Provider_Oct2020.csv", header=T)

data1 <- fulldata[,-c(17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,79,87,89)]

data1 <- data1[complete.cases(data1),]



#table(data1$Quality.of.patient.care.star.rating)
data1$rate <- data1$Quality.of.patient.care.star.rating
data1$rate[data1$rate==1 | data1$rate==1.5 | data1$rate==2] <- 2



data2 <- data1[,-c(1:7,15,16)]

colnames(data2) <- c("Ownership", "B1", "B2", "B3", "B4", "B5", "B6","Q1", "Q2","Q3",
                     "Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17",
                     "DTC1","DTC2","DTC3","DTC4","PPR1","PPR2","PPR3","PPR4","C1","C2","rate")

data2 <- data2[,-c(2,3,8,9,10)]


data2.quant <- data2[,-c(1:5,23,27)]
data2.qual <- data2[,c(1:5,23,27)]

mean <- apply(data2.quant, 2, mean)
std <- apply(data2.quant, 2, sd)

data2.scaled <- scale(data2.quant, mean, std)

X <- cbind(model.matrix(~., data=data2.qual)[,-1], data2.scaled)[,-37]

y <- cbind(as.numeric(data2$rate==2),
           as.numeric(data2$rate==2.5),
           as.numeric(data2$rate==3),
           as.numeric(data2$rate==3.5),
           as.numeric(data2$rate==4),
           as.numeric(data2$rate==4.5),
           as.numeric(data2$rate==5))

set.seed(8888)
train.id <- sample(1:length(data2$rate),2010)
test.id <- NULL
for (i in 1:4019) {
  if (i %in% train.id){
    test.id <- test.id
  } else {
    test.id <- c(test.id, i)
  }
}

X_train <- X[train.id,]
y_train <- y[train.id,]

X_test <- X[test.id,]
y_test <- y[test.id,]



wt <- c(1,1,1,1,1,1,1)
#wt <- c(1,1,1,1,1)


weights <- casl_nnmulti_sgd(X_train, y_train, sizes=c(36,rep(50,15),7), epochs=25L, eta=0.01)


y_pred <- casl_nnmulti_predict(weights, X_train)


max.ind <- apply(y_train, 1, which.max)
max.ind.pred <- apply(y_pred, 1, which.max)
#Accuracy
mean(max.ind==max.ind.pred)
table(max.ind.pred)


##############################################################################################################################



setwd("C:\\Users\\ASUS\\Desktop\\557 project")
fulldata <- read.csv("HH_Provider_Oct2020.csv", header=T)

data1 <- fulldata[,-c(17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,79,87,89)]

data1 <- data1[complete.cases(data1),]

table(data1$Quality.of.patient.care.star.rating)
data1$rate <- "others"
data1$rate[data1$Quality.of.patient.care.star.rating==3 | data1$Quality.of.patient.care.star.rating==3.5 | 
             data1$Quality.of.patient.care.star.rating==4 | data1$Quality.of.patient.care.star.rating==4.5] <- "3-4.5"

data2 <- data1[,-c(1:7,15,16)]

colnames(data2) <- c("Ownership", "B1", "B2", "B3", "B4", "B5", "B6","Q1", "Q2","Q3",
                     "Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17",
                     "DTC1","DTC2","DTC3","DTC4","PPR1","PPR2","PPR3","PPR4","C1","C2","rate")

data2 <- data2[,-c(2,3,8,9,10)]
set.seed(8888)
train.id <- sample(1:length(data2$rate),2010)
test.id <- NULL
for (i in 1:4019) {
  if (i %in% train.id){
    test.id <- test.id
  } else {
    test.id <- c(test.id, i)
  }
}

train.data <- data2[train.id,]

test.data <- data2[test.id,]

X_train <- model.matrix(rate~., data=train.data)[,-1]

X_train1 <- X_train

y_train <- cbind(as.numeric(train.data$rate=="others"), 
                 as.numeric(train.data$rate=="3-4.5"))

weights <- casl_nnmulti_sgd(X_train1, y_train, sizes=c(36,rep(50,5),2), epochs=25L, eta=0.01)


y_pred <- casl_nnmulti_predict(weights, X_train1)


max.ind <- apply(y_train, 1, which.max)
max.ind.pred <- apply(y_pred, 1, which.max)
#Accuracy
mean(max.ind==max.ind.pred)