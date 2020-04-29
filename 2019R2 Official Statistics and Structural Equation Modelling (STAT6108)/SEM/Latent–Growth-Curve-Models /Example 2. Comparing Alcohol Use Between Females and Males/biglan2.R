# Example 2: Comparing Alcohol Use Between Females and Males

# set work directory and load lavaan packages
setwd("c:/users/wchan/google drive/stat6108/data")
library(lavaan)

# data preparation

# group 1: Females
female.corr <- matrix(
c(1.0000,	0.4641,	0.4200,
  0.4641,	1.0000,	0.5614,
  0.4200,	0.5614,	1.0000),
nrow=3, ncol=3)
female.sd <- c(1.3282, 1.5136, 1.5346)
female.mean <- c(1.4430, 1.7230, 1.8310)

# group 2: Males
male.corr <- matrix(
c(1.0000,	0.4708,	0.3915,
  0.4708,	1.0000,	0.6679,
  0.3915,	0.6679,	1.0000),
nrow=3, ncol=3)
male.sd <- c(1.3932, 1.4910, 1.6520)
male.mean <- c(1.5540, 1.8640, 2.2800)

varname <- c("time1", "time2", "time3")
female.cov <- cor2cov(female.corr, female.sd, names=varname)
male.cov <- cor2cov(male.corr, male.sd, names=varname)
names(female.mean) <- names(male.mean) <- varname

# specify Model 1 (Using Wald test to compare the groups)
model1 <- "
# measurement model
int =~ 1*time1 + 1*time2 + 1*time3
growth =~ 0*time1 + 1*time2 + c(la1,la2)*time3
# factor variance and covariance
int ~~ c(ps11,ps12)*int
growth ~~ c(ps21,ps22)*growth
int ~~ c(ps31,ps32)*growth
# error variance (constrained)
time1 ~~ c(theta1,theta2)*time1
time2 ~~ c(theta1,theta2)*time2
time3 ~~ c(theta1,theta2)*time3
# intercepts and factor means
time1 + time2 + time3 ~ 0*1
int ~ c(al11,al12)*1
growth ~ c(al21,al22)*1
# comparing females and males
la_d := la1-la2
ps1_d := ps11-ps12
ps2_d := ps21-ps22
ps3_d := ps31-ps32
al1_d := al11-al12
al2_d := al21-al22
"
# Fit Model 1 to data 
fit1 <-lavaan(model1, sample.cov=list(Females=female.cov, Males=male.cov), sample.mean=list(Females=female.mean, Males=male.mean),
              sample.nobs=c(196, 95))

# specify Model 2 (Using LRT test to compare the groups)
model2 <- "
# measurement model
int =~ 1*time1 + 1*time2 + 1*time3
growth =~ 0*time1 + 1*time2 + time3
# error variance (constrained)
time1 ~~ c(theta1,theta2)*time1
time2 ~~ c(theta1,theta2)*time2
time3 ~~ c(theta1,theta2)*time3
"
# Fit Model 2 to data 
fit2 <-lavaan(model2, sample.cov=list(Females=female.cov, Males=male.cov), sample.mean=list(Females=female.mean, Males=male.mean),
              sample.nobs=c(196, 95), auto.var=TRUE, auto.cov.lv.x=TRUE, meanstructure=TRUE, int.ov.free=FALSE, int.lv.free=TRUE,
              group.equal=c("loadings","lv.variances","lv.covariances","means"))

# save the output
sink("biglan2.out", split=TRUE)
writeLines("\n Example 2: Comparing Alcohol Use Between Females and Males\n")
writeLines("\n Output for Model 1 (Using Wald test to compare the groups)\n")
summary(fit1, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Output for Model 2 (Using LRT test to compare the groups)\n")
summary(fit2, fit.measures=TRUE, standardized=TRUE)
lavTestLRT(fit1,fit2)
sink()
