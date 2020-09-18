## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

setwd("C:\\Users\\ASUS\\Desktop\\BIS557\\homework-1")

lm_patho<-read.csv("lm_patho.csv",header=T)

use_data(lm_patho)
