# Example 6: Motivation and Productivity

# set work directory
setwd("c:/users/wchan/google drive/stat6108/data")

# load library Hmisc
library(Hmisc)

# import data
mydata <- read.table("example1_6.dat", header=TRUE)

sink("example1_6.out", split=TRUE)
list(mydata)
cat("\n compute Pearson correlation and its test \n")
rcorr(as.matrix(mydata), type="pearson")
cat("\n compute Spearman correlation and its test \n")
rcorr(as.matrix(mydata), type="spearman")
sink()
