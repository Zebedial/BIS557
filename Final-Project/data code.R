setwd("C:\\Users\\ASUS\\Desktop\\557 project")
fulldata <- read.csv("HH_Provider_Oct2020.csv", header=T)

data1 <- fulldata[,-c(17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,79,87,89)]

data1 <- data1[complete.cases(data1),]

table(data1$Quality.of.patient.care.star.rating)
data1$rate <- "1-1.5"
data1$rate[data1$Quality.of.patient.care.star.rating==2 | data1$Quality.of.patient.care.star.rating==2.5] <- "2-2.5"
data1$rate[data1$Quality.of.patient.care.star.rating==3 | data1$Quality.of.patient.care.star.rating==3.5] <- "3-3.5"
data1$rate[data1$Quality.of.patient.care.star.rating==4 | data1$Quality.of.patient.care.star.rating==4.5] <- "4-4.5"
data1$rate[data1$Quality.of.patient.care.star.rating==5] <- "5"

data2 <- data1[,-c(1:7,15,16)]

colnames(data2) <- c("Ownership", "B1", "B2", "B3", "B4", "B5", "B6","Q1", "Q2","Q3",
                     "Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17",
                     "DTC1","DTC2","DTC3","DTC4","PPR1","PPR2","PPR3","PPR4","C1","C2","rate")

data2 <- data2[,-c(2,3,8,9,10)]

X <- model.matrix(rate~., data=data2)[,-1]

y <- cbind(as.numeric(data2$rate=="1-1.5"), 
           as.numeric(data2$rate=="2-2.5"), 
           as.numeric(data2$rate=="3-3.5"), 
           as.numeric(data2$rate=="4-4.5"),
           as.numeric(data2$rate=="5"))



################################################################################################################################

setwd("C:\\Users\\ASUS\\Desktop\\557 project")
fulldata <- read.csv("HH_Provider_Oct2020.csv", header=T)

data1 <- fulldata[,-c(17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,79,87,89)]

data1 <- data1[complete.cases(data1),]

#table(data1$Quality.of.patient.care.star.rating)
data1$rate <- as.factor(data1$Quality.of.patient.care.star.rating)


data2 <- data1[,-c(1:7,15,16)]

colnames(data2) <- c("Ownership", "B1", "B2", "B3", "B4", "B5", "B6","Q1", "Q2","Q3",
                     "Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17",
                     "DTC1","DTC2","DTC3","DTC4","PPR1","PPR2","PPR3","PPR4","C1","C2","rate")

data2 <- data2[,-c(2,3,8,9,10)]

X <- model.matrix(rate~., data=data2)[,-1]

y <- cbind(as.numeric(data2$rate=="1" | data2$rate=="1.5" | as.numeric(data2$rate=="2")), 
           as.numeric(data2$rate=="2.5"),
           as.numeric(data2$rate=="3"), 
           as.numeric(data2$rate=="3.5"), 
           as.numeric(data2$rate=="4"), 
           as.numeric(data2$rate=="4.5"),
           as.numeric(data2$rate=="5"))