# Example 9: Salary and Transport

# set work directory
setwd("c:/users/wchan/google drive/stat6108/data")

# load library DescTools
library(DescTools)

# import data
mydata <- read.table("example1_9.dat", header=TRUE)

# Create a table 
mytable <- xtabs(count ~ transportation+salary, data=mydata)
rownames(mytable) <- c("walking", "bus", "minibus", "taxi")
colnames(mytable) <- c("decrease","no change","increase")

sink("example1_9.out", split=TRUE)
writeLines("\n Print Data Set \n")
mydata
writeLines("\n Print table \n")
mytable
writeLines("\n Lambda coefficient: Row variable (transportation) as outcome \n")
Lambda(mytable, direction="row", conf.level=.95)
writeLines("\n Lambda coefficient: Column variable (salary) as outcome \n")
Lambda(mytable, direction="column", conf.level=.95)
writeLines("\n Symmetric Lambda coefficient \n")
Lambda(mytable, direction="symmetric", conf.level=.95)
sink()
