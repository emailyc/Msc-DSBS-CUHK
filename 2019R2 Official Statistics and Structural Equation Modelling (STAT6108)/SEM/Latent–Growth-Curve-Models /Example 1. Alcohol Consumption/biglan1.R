# Example 1: Alcohol Consumption

# set work directory and load the packages
setwd("c:/users/wchan/google drive/stat6108/data")
library(lavaan)
library(semPlot)

# data preparation
alc.corr <- matrix(
c(1.000,   0.486,   0.399,
  0.486,   1.000,   0.533,
  0.399,   0.533,   1.000),
nrow=3, ncol=3)
alc.sd <- c(7.390, 7.990, 8.080)
alc.mean <- c(8.310, 10.000, 10.810)

varname <- c("time1", "time2", "time3")
alc.cov <- cor2cov(alc.corr, alc.sd, names=varname)
names(alc.mean) <- varname

# specify Model 1 (Evaluating linear growth using Wald test)
model1 <- "
# measurement model
int =~ 1*time1 + 1*time2 + 1*time3
growth =~ 0*time1 + 1*time2 + la32*time3
# factor variance and covariance
int ~~ int + growth
growth ~~ growth
# error variance (constrained)
time1 ~~ c1*time1
time2 ~~ c1*time2
time3 ~~ c1*time3
# intercepts
time1 + time2 + time3 ~ 0*1
int + growth ~ 1
# evaluating linear growth
linear := la32-2
"
# Fit Model 1 to data 
fit1 <-lavaan(model1, sample.cov=alc.cov, sample.mean=alc.mean, sample.nobs=343)

# specify Model 2 (Evaluating linear growth using LR test)
model2 <- "
# measurement model
int =~ 1*time1 + 1*time2 + 1*time3
growth =~ 0*time1 + 1*time2 + 2*time3
# error variance (constrained)
time1 ~~ c1*time1
time2 ~~ c1*time2
time3 ~~ c1*time3
"
# Fit Model 2 to data 
fit2 <-lavaan(model2, sample.cov=alc.cov, sample.mean=alc.mean, sample.nobs=343, auto.var=TRUE, 
              auto.cov.lv.x=TRUE, meanstructure=TRUE, int.ov.free=FALSE, int.lv.free=TRUE)

# save the output
sink("biglan1.out", split=TRUE)
writeLines("\n Example 1: Alcohol Consumption\n")
writeLines("\n Output for Model 1 (Evaluating linear growth using Wald test)\n")
inspect(fit1)
summary(fit1, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Output for Model 2 (Evaluating linear growth using LR test)\n")
summary(fit2, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Model Comparisons\n")
lavTestLRT(fit1, fit2)
sink()

# create path diagram
semPaths(fit1,"path","est",nCharNodes=5)
