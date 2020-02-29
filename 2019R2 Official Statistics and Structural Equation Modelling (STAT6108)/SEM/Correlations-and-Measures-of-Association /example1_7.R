# Example 7: Productivity and Company Image

# set work directory
setwd("c:/users/wchan/google drive/stat6108/data")

# load library vcdExtra
library(vcdExtra)

# import data
mydata <- read.table("example1_7.dat", header=TRUE)

# How to create cross classification table?

# Method 1: From Raw Data set
table1 <- table(mydata$productivity, mydata$image)

# Method 2: From Data Set Weighted by Frequency
table2 <- xtabs(count ~ productivity+image, data=mydata)

# Method 3: Create a table directly 
table3 <- matrix(c(10, 5, 2, 8, 9, 7, 2, 6, 8), nrow=3, ncol=3, byrow=TRUE)
colnames(table3) <- c("Low","Moderate","High")
rownames(table3) <- c("Low","Moderate","High")
table3 <- as.table(table3)

sink("example1_7.out", split=TRUE)
list(table1,table2,table3)
cat("\n Goodman and Kruskal gamma coefficient \n")
GKgamma(table2, level = 0.95)
sink()
