## quick means of vpd
source("functions/basic_functions.R")

vpd <- read.csv("raw_data/vpdl.csv")

library(doBy)

mean(vpd$VpdL)
se(vpd$VpdL)
sd(vpd$VpdL)
max(vpd$VpdL)
min(vpd$VpdL)
hist(vpd$VpdL)
