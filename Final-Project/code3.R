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


# Helper packages
library(dplyr)         # for basic data wrangling

# Modeling packages
library(keras)         # for fitting DNNs
library(tfruns)        # for additional grid search & model training functions

# Modeling helper package - not necessary for reproducibility
library(tfestimators)  # provides grid search & model training interface





model <- keras_model_sequential() %>%
  
  # Network architecture
  layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 7, name="my_layer", activation = "softmax") %>%
  
  # Backpropagation
  compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )

set.seed(8888)
val_indices <- sample(1:2010, 600)
X_val <- X_train[val_indices,]
partial_X_train <- X_train[-val_indices,]
y_val <- y_train[val_indices,]
partial_y_train <- y_train[-val_indices,]

fit1 <- model %>%
  fit(
    x = partial_X_train,
    y = partial_y_train,
    epochs = 25,
    batch_size = 32,
    verbose = FALSE,
    validation_data = list(X_val, y_val)
  )


fit1

plot(fit1)

results1 <- model %>% evaluate(X_test, y_test) 

#################################################################################

fit0 <- model %>%
  fit(
    x = X_train,
    y = y_train,
    epochs = 10,
    batch_size = 32,
    verbose = FALSE
  )


fit0

plot(fit0)

results0 <- model %>% evaluate(X_test, y_test) 


layer_name <- 'my_layer'
int_layer_model <- keras_model(inputs=model$input, 
                               outputs=get_layer(model, layer_name)$output)
int_output <- predict(int_layer_model, X_test)

max.ind <- apply(y_test, 1, which.max)
max.ind.pred <- apply(int_output, 1, which.max)
#Accuracy
mean(max.ind==max.ind.pred)
table(max.ind.pred)
table(max.ind)

for (i in 1:2019) {
  if (i > 1500) {
    print(i/2019)
  }
}



col1 <- c(sum(max.ind==1 & max.ind.pred==1),sum(max.ind==1 & max.ind.pred==2),
          sum(max.ind==1 & max.ind.pred==3),sum(max.ind==1 & max.ind.pred==4),
          sum(max.ind==1 & max.ind.pred==5),sum(max.ind==1 & max.ind.pred==6),
          sum(max.ind==1 & max.ind.pred==7))
col2 <- c(sum(max.ind==2 & max.ind.pred==1),sum(max.ind==2 & max.ind.pred==2),
          sum(max.ind==2 & max.ind.pred==3),sum(max.ind==2 & max.ind.pred==4),
          sum(max.ind==2 & max.ind.pred==5),sum(max.ind==2 & max.ind.pred==6),
          sum(max.ind==2 & max.ind.pred==7))
col3 <- c(sum(max.ind==3 & max.ind.pred==1),sum(max.ind==3 & max.ind.pred==2),
          sum(max.ind==3 & max.ind.pred==3),sum(max.ind==3 & max.ind.pred==4),
          sum(max.ind==3 & max.ind.pred==5),sum(max.ind==3 & max.ind.pred==6),
          sum(max.ind==3 & max.ind.pred==7))
col4 <- c(sum(max.ind==4 & max.ind.pred==1),sum(max.ind==4 & max.ind.pred==2),
          sum(max.ind==4 & max.ind.pred==3),sum(max.ind==4 & max.ind.pred==4),
          sum(max.ind==4 & max.ind.pred==5),sum(max.ind==4 & max.ind.pred==6),
          sum(max.ind==4 & max.ind.pred==7))
col5 <- c(sum(max.ind==5 & max.ind.pred==1),sum(max.ind==5 & max.ind.pred==2),
          sum(max.ind==5 & max.ind.pred==3),sum(max.ind==5 & max.ind.pred==4),
          sum(max.ind==5 & max.ind.pred==5),sum(max.ind==5 & max.ind.pred==6),
          sum(max.ind==5 & max.ind.pred==7))
col6 <- c(sum(max.ind==6 & max.ind.pred==1),sum(max.ind==6 & max.ind.pred==2),
          sum(max.ind==6 & max.ind.pred==3),sum(max.ind==6 & max.ind.pred==4),
          sum(max.ind==6 & max.ind.pred==5),sum(max.ind==6 & max.ind.pred==6),
          sum(max.ind==6 & max.ind.pred==7))
col7 <- c(sum(max.ind==7 & max.ind.pred==1),sum(max.ind==7 & max.ind.pred==2),
          sum(max.ind==7 & max.ind.pred==3),sum(max.ind==7 & max.ind.pred==4),
          sum(max.ind==7 & max.ind.pred==5),sum(max.ind==7 & max.ind.pred==6),
          sum(max.ind==7 & max.ind.pred==7))
sumtable0 <- cbind(col1,col2,col3,col4,col5,col6,col7)




##Ridge
lambda1 <- seq(1,25)*0.004
acc.set1 <- numeric(25)

for (i1 in 1:25) {

  lambda <- lambda1[i1]
  k <- 5
  set.seed(8888)
  indices <- sample(1:2010)
  folds <- cut(indices, breaks = k, labels = FALSE)
  num_epochs <- 10
  all_acc <- c()
  for (i in 1:k) {
    cat("processing fold #", i, "\n")
    val_indices <- which(folds == i, arr.ind = TRUE)
    val_data <- X_train[val_indices,]
    val_targets <- y_train[val_indices,]
    partial_train_data <- X_train[-val_indices,]
    partial_train_targets <- y_train[-val_indices,]
    set.seed(8888)
    model <- keras_model_sequential() %>%
      
      # Network architecture
      layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train), kernel_regularizer = regularizer_l2(lambda)) %>%
      layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(lambda)) %>%
      layer_dense(units = 7, name="my_layer", activation = "softmax") %>%
      
      # Backpropagation
      compile(
        loss = 'categorical_crossentropy',
        optimizer = optimizer_rmsprop(),
        metrics = c('accuracy')
      )
    
    model %>% fit(partial_train_data, partial_train_targets,
                  epochs = num_epochs, batch_size = 32, verbose = 0)
    results <- model %>% evaluate(val_data, val_targets, verbose = 0)
    all_acc <- c(all_acc, results[2])
  }
  
  acc.set1[i1] <- mean(all_acc)
}


plot(lambda1, acc.set1, type="l", xlab="Lambda", ylab="Accuracy")

ac.ridge <- numeric(25)
for (i2 in 1:25) {
  lambda <- lambda1[i1]
  set.seed(8888)
  model1 <- keras_model_sequential() %>%
    
    # Network architecture
    layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train), kernel_regularizer = regularizer_l2(lambda)) %>%
    layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(lambda)) %>%
    layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(lambda)) %>%
    layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(lambda)) %>%
    layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(lambda)) %>%
    layer_dense(units = 7, name="my_layer", activation = "softmax") %>%
    
    # Backpropagation
    compile(
      loss = 'categorical_crossentropy',
      optimizer = optimizer_rmsprop(),
      metrics = c('accuracy')
    )
  
  
  fit1 <- model1 %>%
    fit(
      x = X_train,
      y = y_train,
      epochs = 6,
      batch_size = 32,
      verbose = FALSE,
    )
  results1 <- model1 %>% evaluate(X_test, y_test)
  ac.ridge[i2] <- results1[2]
}
plot(lambda1, ac.ridge, type="l")

lambda <- 0.04
set.seed(8888)
model1 <- keras_model_sequential() %>%
  
  # Network architecture
  layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train), kernel_regularizer = regularizer_l2(lambda)) %>%
  layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l2(lambda)) %>%
  layer_dense(units = 7, name="my_layer", activation = "softmax") %>%
  
  # Backpropagation
  compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )


fit1 <- model1 %>%
  fit(
    x = X_train,
    y = y_train,
    epochs = 6,
    batch_size = 32,
    verbose = FALSE,
  )

fit1

results1 <- model1 %>% evaluate(X_test, y_test) 


layer_name <- 'my_layer'
int_layer_model <- keras_model(inputs=model1$input, 
                               outputs=get_layer(model1, layer_name)$output)
int_output <- predict(int_layer_model, X_test)

max.ind <- apply(y_test, 1, which.max)
max.ind.pred <- apply(int_output, 1, which.max)
#Accuracy
mean(max.ind==max.ind.pred)
table(max.ind.pred)
table(max.ind)



##Lasso
acc.set2 <- numeric(25)

for (i1 in 1:25) {
  
  lambda <- lambda1[i1]
  k <- 5
  indices <- sample(1:2010)
  folds <- cut(indices, breaks = k, labels = FALSE)
  num_epochs <- 6
  all_acc <- c()
  for (i in 1:k) {
    cat("processing fold #", i, "\n")
    val_indices <- which(folds == i, arr.ind = TRUE)
    val_data <- X_train[val_indices,]
    val_targets <- y_train[val_indices,]
    partial_train_data <- X_train[-val_indices,]
    partial_train_targets <- y_train[-val_indices,]
    set.seed(8888)
    model <- keras_model_sequential() %>%
      
      # Network architecture
      layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train), kernel_regularizer = regularizer_l1(lambda)) %>%
      layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l1(lambda)) %>%
      layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l1(lambda)) %>%
      layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l1(lambda)) %>%
      layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l1(lambda)) %>%
      layer_dense(units = 7, name="my_layer", activation = "softmax") %>%
      
      # Backpropagation
      compile(
        loss = 'categorical_crossentropy',
        optimizer = optimizer_rmsprop(),
        metrics = c('accuracy')
      )
    
    model %>% fit(partial_train_data, partial_train_targets,
                  epochs = num_epochs, batch_size = 16, verbose = 0)
    results <- model %>% evaluate(val_data, val_targets, verbose = 0)
    all_acc <- c(all_acc, results[2])
  }
  
  acc.set2[i1] <- mean(all_acc)
}

##Dropout

prop <- (1:20)*0.02
acc.set3 <- numeric(20)

for (i1 in 1:20) {

  p <- prop[i1]
  k <- 5
  indices <- sample(1:2010)
  folds <- cut(indices, breaks = k, labels = FALSE)
  num_epochs <- 10
  all_acc <- c()
  for (i in 1:k) {
    cat("processing fold #", i, "\n")
    val_indices <- which(folds == i, arr.ind = TRUE)
    val_data <- X_train[val_indices,]
    val_targets <- y_train[val_indices,]
    partial_train_data <- X_train[-val_indices,]
    partial_train_targets <- y_train[-val_indices,]
    set.seed(8888)
    model <- keras_model_sequential() %>%
      
      # Network architecture
      layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train)) %>%
      layer_dropout(rate=p)%>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dropout(rate=p)%>%
      layer_dense(units = 7, name="my_layer", activation = "softmax") %>%
      
      # Backpropagation
      compile(
        loss = 'categorical_crossentropy',
        optimizer = optimizer_rmsprop(),
        metrics = c('accuracy')
      )
    
    model %>% fit(partial_train_data, partial_train_targets,
                  epochs = num_epochs, batch_size = 32, verbose = 0)
    results <- model %>% evaluate(val_data, val_targets, verbose = 0)
    all_acc <- c(all_acc, results[2])
  }
  
  acc.set3[i1] <- mean(all_acc)
}

plot(prop,acc.set3,type="l", xlab="Probability", ylab="Accuracy")

p <- 0.02
set.seed(8888)
model2 <- keras_model_sequential() %>%
  
  # Network architecture
  layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train)) %>%
  layer_dropout(rate=p)%>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate=p)%>%
  layer_dense(units = 7, name="my_layer", activation = "softmax") %>%
  
  # Backpropagation
  compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )


fit2 <- model2 %>%
  fit(
    x = X_train,
    y = y_train,
    epochs = 6,
    batch_size = 32,
    verbose = FALSE,
  )

fit2

results2 <- model2 %>% evaluate(X_test, y_test) 


layer_name <- 'my_layer'
int_layer_model <- keras_model(inputs=model2$input, 
                               outputs=get_layer(model2, layer_name)$output)
int_output <- predict(int_layer_model, X_test)

max.ind <- apply(y_test, 1, which.max)
max.ind.pred <- apply(int_output, 1, which.max)
#Accuracy
mean(max.ind==max.ind.pred)
table(max.ind.pred)
table(max.ind)

col1 <- c(sum(max.ind==1 & max.ind.pred==1),sum(max.ind==1 & max.ind.pred==2),
          sum(max.ind==1 & max.ind.pred==3),sum(max.ind==1 & max.ind.pred==4),
          sum(max.ind==1 & max.ind.pred==5),sum(max.ind==1 & max.ind.pred==6),
          sum(max.ind==1 & max.ind.pred==7))
col2 <- c(sum(max.ind==2 & max.ind.pred==1),sum(max.ind==2 & max.ind.pred==2),
          sum(max.ind==2 & max.ind.pred==3),sum(max.ind==2 & max.ind.pred==4),
          sum(max.ind==2 & max.ind.pred==5),sum(max.ind==2 & max.ind.pred==6),
          sum(max.ind==2 & max.ind.pred==7))
col3 <- c(sum(max.ind==3 & max.ind.pred==1),sum(max.ind==3 & max.ind.pred==2),
          sum(max.ind==3 & max.ind.pred==3),sum(max.ind==3 & max.ind.pred==4),
          sum(max.ind==3 & max.ind.pred==5),sum(max.ind==3 & max.ind.pred==6),
          sum(max.ind==3 & max.ind.pred==7))
col4 <- c(sum(max.ind==4 & max.ind.pred==1),sum(max.ind==4 & max.ind.pred==2),
          sum(max.ind==4 & max.ind.pred==3),sum(max.ind==4 & max.ind.pred==4),
          sum(max.ind==4 & max.ind.pred==5),sum(max.ind==4 & max.ind.pred==6),
          sum(max.ind==4 & max.ind.pred==7))
col5 <- c(sum(max.ind==5 & max.ind.pred==1),sum(max.ind==5 & max.ind.pred==2),
          sum(max.ind==5 & max.ind.pred==3),sum(max.ind==5 & max.ind.pred==4),
          sum(max.ind==5 & max.ind.pred==5),sum(max.ind==5 & max.ind.pred==6),
          sum(max.ind==5 & max.ind.pred==7))
col6 <- c(sum(max.ind==6 & max.ind.pred==1),sum(max.ind==6 & max.ind.pred==2),
          sum(max.ind==6 & max.ind.pred==3),sum(max.ind==6 & max.ind.pred==4),
          sum(max.ind==6 & max.ind.pred==5),sum(max.ind==6 & max.ind.pred==6),
          sum(max.ind==6 & max.ind.pred==7))
col7 <- c(sum(max.ind==7 & max.ind.pred==1),sum(max.ind==7 & max.ind.pred==2),
          sum(max.ind==7 & max.ind.pred==3),sum(max.ind==7 & max.ind.pred==4),
          sum(max.ind==7 & max.ind.pred==5),sum(max.ind==7 & max.ind.pred==6),
          sum(max.ind==7 & max.ind.pred==7))
sumtable1 <- cbind(col1,col2,col3,col4,col5,col6,col7)

