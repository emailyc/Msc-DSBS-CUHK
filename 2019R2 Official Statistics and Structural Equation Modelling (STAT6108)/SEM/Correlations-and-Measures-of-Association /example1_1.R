# Example 1: Job Commitment and Performance

# set work directory
setwd("c:/users/wchan/google drive/stat6108/data")

# load library Hmisc
library(Hmisc)

# import data
mydata <- read.table("example1_1.dat", header=TRUE)

sink("example1_1.out", split=TRUE)
list(mydata)

# Scatter plot
attach(mydata)
plot(commitment, performance, main="Scatter plot of performance against commitment", xlab="job commitment", ylab="job performance")

# Person correlation coefficient
cat("\n compute Pearson correlation and its test \n")
rcorr(as.matrix(mydata), type="pearson")
sink()
