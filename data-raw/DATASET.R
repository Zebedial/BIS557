## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

setwd("C:\\Users\\ASUS\\Desktop\\BIS557\\homework-1") #local directory where the data locates

lm_patho<-read.csv("lm_patho.csv",header=T)

use_data(lm_patho)

## Penguinsi

library(palmerpenguins)
library(missForest)
penguinsi <- penguins
penguinsi <- data.frame(missForest(as.data.frame(penguinsi))$ximp)
setwd("C:\\Users\\ASUS\\Desktop\\BIS557\\homework-1\\bis557")
use_data(penguinsi, overwrite = T) #add dataset into the package
