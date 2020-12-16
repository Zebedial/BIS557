# Helper packages
library(dplyr)         # for basic data wrangling

# Modeling packages
library(keras)         # for fitting DNNs
library(tfruns)        # for additional grid search & model training functions

# Modeling helper package - not necessary for reproducibility
library(tfestimators)  # provides grid search & model training interface

model <- keras_model_sequential() %>%
  
  # Network architecture
  layer_dense(units = 36, activation = "relu", input_shape = ncol(X)) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 7, name="my_layer", activation = "softmax") %>%
  
  # Backpropagation
  compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )

fit1 <- model %>%
  fit(
    x = X,
    y = y,
    epochs = 25,
    batch_size = 16,
    validation_split = 0.5,
    verbose = FALSE
  )

fit1

plot(fit1)

layer_name <- 'my_layer'
int_layer_model <- keras_model(inputs=model$input, 
                               outputs=get_layer(model, layer_name)$output)
int_output <- predict(int_layer_model, X)

max.ind <- apply(y, 1, which.max)
max.ind.pred <- apply(int_output, 1, which.max)
#Accuracy
mean(max.ind==max.ind.pred)
table(max.ind.pred)
table(max.ind)



model2 <- keras_model_sequential() %>%
  
  # Network architecture
  layer_dense(units = 36, activation = "relu", input_shape = ncol(X), kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 128, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 128, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 7, name="my_layer", activation = "softmax") %>%
  
  # Backpropagation
  compile(
    loss = WCE,
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )

fit2 <- model2 %>%
  fit(
    x = X,
    y = y,
    epochs = 25,
    batch_size = 16,
    validation_split = 0.5,
    verbose = FALSE
  )

fit2

plot(fit2)

layer_name <- 'my_layer'
int_layer_model <- keras_model(inputs=model2$input, 
                               outputs=get_layer(model2, layer_name)$output)
int_output <- predict(int_layer_model, X)

max.ind <- apply(y, 1, which.max)
max.ind.pred <- apply(int_output, 1, which.max)
#Accuracy
mean(max.ind==max.ind.pred)
table(max.ind.pred)
table(max.ind)

weights1 <- get_weights(model)


WCE <- function(y_true, y_pred) {
  -k_sum(y_true * k_log(y_pred))
}


