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
Y <- apply(y, 1, which.max)

fulldata <- data.frame(cbind(Y,X))
library(nlme)
library(mgcv)
library(nnet)
reg.model <- multinom(Y~., data=fulldata)

y.pred <- fitted(reg.model)

max.ind.pred <- apply(y.pred, 1, which.max)
#Accuracy
mean(Y==max.ind.pred)
table(max.ind.pred)
table(Y)

max.ind<-Y

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
sumtable.reg <- cbind(col1,col2,col3,col4,col5,col6,col7)
