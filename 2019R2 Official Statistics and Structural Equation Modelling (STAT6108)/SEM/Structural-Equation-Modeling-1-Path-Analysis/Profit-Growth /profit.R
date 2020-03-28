# Example 3: Profit Growth

# set work directory
setwd("c:/users/wchan/google drive/stat6108/data")

# load the lavaan package
library(lavaan)
ability <- scan("profit.cov")

# write the input data into a full covariance matrix 
ability.cov <- getCov(ability, names=c("time1","time2", "time3", "time4"))

# specify Model 1 (first order model)
model1 <- "
# regression Equation (Beta)
time2 ~ time1 
time3 ~ time2
time4 ~ time3

# Variance of x (Psi)
time1 ~~ time1

# error variance (Psi)
time2 ~~ time2
time3 ~~ time3
time4 ~~ time4
"

# specify Model 2 (second order model)
model2 <- "
# regression Equation (Beta)
time2 ~ time1 
time3 ~ time1 + time2
time4 ~ time2 + time3

# Variance of x (Psi)
time1 ~~ time1

# error variance (Psi)
time2 ~~ time2
time3 ~~ time3
time4 ~~ time4
"

# specify Model 3 (first order model with latent variables)
model3 <- "
# measurement equations (Lambda)
Profit1 =~ 1*time1
Profit2 =~ 1*time2
Profit3 =~ 1*time3
Profit4 =~ 1*time4

# structural equations (Beta)
Profit2 ~ Profit1 
Profit3 ~ Profit2
Profit4 ~ Profit3

# constraining error variances (Theta)
time1 ~~ vare*time1
time2 ~~ vare*time2
time3 ~~ vare*time3
time4 ~~ vare*time4
"

# Fit Models 1-3 to the data 
fit1 <- lavaan(model1, sample.cov=ability.cov, sample.cov.rescale=FALSE, sample.nobs=200, fixed.x=FALSE)
fit2 <- lavaan(model2, sample.cov=ability.cov, sample.cov.rescale=FALSE, sample.nobs=200, fixed.x=FALSE)
fit3 <- lavaan(model3, sample.cov=ability.cov, sample.cov.rescale=FALSE, sample.nobs=200, auto.var=TRUE)

# save the output
sink("profit.out", split=TRUE)
writeLines("\n Example 3: Profit Growth\n")
writeLines("\n Output for Model 1\n")
inspect(fit1)
summary(fit1, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
writeLines("\n Output for Model 2\n")
inspect(fit2)
summary(fit2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
lavTestLRT(fit1, fit2)
writeLines("\n Output for Model 3\n")
inspect(fit3)
summary(fit3, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
sink()