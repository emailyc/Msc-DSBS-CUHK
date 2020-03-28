# Example 2: College Academic Performance (Raykov, 2006)

# set work directory
setwd("c:/users/wchan/google drive/stat6108/data")

# load the lavaan package
library(lavaan)
data <- scan("college.cov")

# write the input data into a full covariance matrix 
college.cov <- getCov(data, names=c("GPA_R", "GPA_E", "SAT", "IQ", "Motiv"))

# Specify Model 1
college1.model <- "
# regression equation (B)
GPA_R ~ SAT 
GPA_E ~ SAT + IQ + Motiv

# error variance of e (psi)
GPA_R ~~ GPA_R
GPA_E ~~ GPA_E

# variance-covariance of x (psi)
SAT ~~ SAT
IQ ~~ IQ
Motiv ~~ Motiv 
SAT ~~ IQ + Motiv
IQ ~~ Motiv
"

# Fit Model 1 to the data 
fit1 <- lavaan(college1.model, sample.cov=college.cov, sample.nobs=150, fixed.x=FALSE)
mi1 <- modindices(fit1, sort.=TRUE)


# Specify Model 2 (based on modification indices from Model 1)
college2.model <- "
# Regression Equation (B)
GPA_R ~ SAT 
GPA_E ~ SAT + IQ + Motiv

# variance-covariance of x (psi)
SAT ~~ SAT
IQ ~~ IQ
Motiv ~~ Motiv 
SAT ~~ IQ + Motiv
IQ ~~ Motiv

# error Variance and Covariance (psi)
GPA_R ~~ GPA_R
GPA_E ~~ GPA_E
GPA_R ~~ GPA_E
"

# Fit Model 2 to the data 
fit2 <- lavaan(college2.model, sample.cov=college.cov, sample.nobs=150, fixed.x=FALSE)
mi2 <- modindices(fit2, sort.=TRUE)


sink("college.out", split=TRUE)
writeLines("\n Example 2: College Academic Performance (Raykov, 2006) \n")
writeLines("\n Output for Model 1 \n")
writeLines("\n Sample covariance matrix \n")
list(college.cov)
writeLines("\n residual matrix \n")
residuals(fit1, type="raw")
writeLines("\n standardized residual matrix \n")
residuals(fit1, type="cor")
writeLines("\n free parameters in Model 1 \n")
inspect(fit1)
summary(fit1, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
writeLines("\n modification indices \n")
list(mi1)
writeLines("\n Output for Model 2 \n")
writeLines("\n residual matrix \n")
residuals(fit2, type="raw")
writeLines("\n standardized residual matrix \n")
residuals(fit2, type="cor")
writeLines("\n free parameters in Model 2 \n")
inspect(fit2)
summary(fit2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
writeLines("\n modification indices \n")
list(mi2)
writeLines("\n Comparing Model 1 and Model 2 \n")
lavTestLRT(fit1, fit2)
sink()
