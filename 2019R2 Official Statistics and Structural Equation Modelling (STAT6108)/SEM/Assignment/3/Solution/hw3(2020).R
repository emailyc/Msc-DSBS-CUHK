# install and upload package "psych"
if (!require("psych")) install.packages("psych", dependencies=TRUE)
library(psych)

# set working directory
setwd("C:/Users/wchan/google drive/stat6108/2020/assignment")

# import data
mydata <- read.table("hw3(2020).dat")                  
p <- ncol(mydata)
nobs <- nrow(mydata)

# compute summary statistics and correlation matrix of the variables
describe(mydata)
mycor <- cor(mydata)
mycor

# Bartlett's test and KMO measure of sampling adequacy
cortest.bartlett(mycor, n=nobs)
KMO(mycor) 

# Complete factor solutions using principal component extraction
principal(mycor, nfactors=p, n.obs=nobs, rotate="none")

# Scree plot and parallet analysis
fa.parallel(mycor, n.obs=nobs, n.iter=100, fa="pc", nfactors=p)

# PC solutions with 3 factors extracted and varimax rotation
fit_pc <- principal(mycor, n.obs=nobs, nfactors=3, residuals=TRUE, rotate="varimax") 
fit_pc

# output residuals
error <- as.data.frame(fit_pc["residual"])
round(error,3)


