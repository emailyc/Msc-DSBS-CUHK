# Example 8: Soda Preference

# set work directory
setwd("c:/users/wchan/google drive/stat6108/data")

# load library vcdExtra
library(vcdExtra)

# Create a table directly 
table <- matrix(c(60, 20, 30, 10, 10, 70), nrow=2, byrow=TRUE)
rownames(table) <- c("Male", "Female")
colnames(table) <- c("Coke","Pepsi","Coke Light")
table <- as.table(table)

sink("example1_8.out", split=TRUE)
writeLines("\n Print table \n")
table
writeLines("\n Row Totals \n")
margin.table(table,1) 
writeLines("\n Column Totals \n")
margin.table(table,2) 
writeLines("\n Cramer's V coefficient \n")
assocstats(table)
sink()
