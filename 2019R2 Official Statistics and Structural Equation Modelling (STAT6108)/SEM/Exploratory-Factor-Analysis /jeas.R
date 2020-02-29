# install and upload package "psych"
if (!require("psych")) install.packages("psych", dependencies=TRUE)
library(psych)

# set working directory
setwd("C:/Users/wchan/google drive/stat6108/data")

# import data
mydata <- read.table("jeas.dat")                  

# compute summary statistics and correlation matrix of the variables
sum <- describe(mydata)
mycor <- cor(mydata)

# compute no. of variables and sample size
p <- ncol(mydata)
nobs <- nrow(mydata)

# Bartlett's test and KMO measure of sampling adequacy
bartlett <- cortest.bartlett(mycor, n=nobs)
kmo <- KMO(mycor) 

# Initial factor solutions using principal component extraction
fit0 <- principal(mycor, nfactors=p, n.obs=nobs, rotate="none")

# Scree plot and parallet analysis
fa.parallel(mycor, n.obs=nobs, n.iter=100, fa="pc", nfactors=p)

# PC solutions with 3 factors extracted
fit_pc <- principal(mycor, n.obs=nobs, nfactors=3, residuals=TRUE, rotate="none") 
# output residuals
error <- as.data.frame(fit_pc["residual"])

# PAF solutions with 3 factors extracted
fit_paf <- fa(mycor, n.obs=nobs, nfactors=3, fm="pa", rotate="none") 

# Varimax rotation: PC solutions with 3 factors extracted
fit_varimax <- principal(mydata, nfactors=3, rotate="varimax", scores=TRUE) 
# output factor scores
fs_varimax=as.data.frame(fit_varimax["scores"]) 

# Oblimin rotation: PC solutions with 3 factors extracted
fit_oblimin=principal(mydata, nfactors=3, rotate="oblimin", scores=TRUE) 
# output factor scores
fs_oblimin=as.data.frame(fit_oblimin["scores"]) 

# Factor-based scales
attach(mydata)
fb1 <- (V2+V3+V7+V8+(6-V10)+(6-V11))/6
fb2 <- (V4+(6-V5)+V6+V9+(6-V12))/5
fb3 <- V1

# compute summary statistics and correlation matrix of factor scores
fs <- data.frame(fs_varimax, fs_oblimin, fb1, fb2, fb3)
mean_fs <- describe(fs)
cor_fs <- cor(fs)


sink("jeas.out", split=TRUE)
cat("\n EFA Example: Junior Executive Attitude Survey \n")
cat("\n Summary Statistics: \n", "No. of observation:", nobs, "No. of Variables:", p, "\n")
sum
cat("\n Correlation Matrix:\n")
mycor
cat("\n Bartlett's Test of Sphericity:\n")
bartlett
cat("\n KMO Index \n")
kmo
cat("\n Complete factor Solutions: \n")
fit0
cat("\n\n Initial 3-factor solutions using PC \n")
fit_pc
cat("\n\n Residual Matrix \n")
round(error,3)
cat("\n Initial 3-factor solutions using PAF \n")
fit_paf
cat("\n Varimax rotated solutions using PC \n")
fit_varimax
cat("\n\n weight matrix based on Varimax rotation \n")
fit_varimax["weights"]
cat("\n", "First 10 factor Scores based on varimax:", "\n")
head(fs_varimax, n=10)
cat("\n Oblimin rotated solutions using PC \n")
fit_oblimin
cat("\n\n weight matrix based on oblimin rotation \n")
fit_oblimin["weights"]
cat("\n", "First 10 factor Scores based on oblimin:", "\n")
head(fs_oblimin, n=10)
cat("\n Summary statistics and correlation matrix of factor scores \n")
mean_fs
round(cor_fs,3)
sink()

# If input data is a full covariance/correlation matrix
# mycov=read.table("jeas.cov")
# mycov=as.matrix(mycov)
# mycor=cov2cor(mycov)

# If input data is a lower triangular correlation matrix
# Install package lavaan
# if (!require("lavaan")) install.packages("lavaan", dependencies=TRUE)
# library(lavaan)
# myvec=scan("jeas_lower.cor")
# mycor=getCov(myvec)


