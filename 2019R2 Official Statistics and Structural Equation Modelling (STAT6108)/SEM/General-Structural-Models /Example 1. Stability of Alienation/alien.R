# Example 1: Stability of Alienation (Wheaton et al., 1977)

# set work directory
setwd("c:/users/wchan/google drive/stat6108/data")

# load the lavaan package
library(lavaan)

# write the input data into a full covariance matrix 
alien <- scan("alien.cov")
alien.cov <- getCov(alien, names=c("anomia67", "powerl67", "anomia71", "powerl71", "educ", "sei"))

# specify Model 1
model1 <- "
# measurement model
 SES =~ educ + sei
 ALIEN67 =~ anomia67 + powerl67
 ALIEN71 =~ anomia71 + powerl71

# structural model
 ALIEN67 ~ a*SES
 ALIEN71 ~ b*ALIEN67 + c*SES

# effect decomposition
 direct := c
 indirect := a*b
 total := direct + indirect
"

# Fit Model 1 to data
fit1 <- lavaan(model1, sample.cov=alien.cov, sample.cov.rescale=FALSE, sample.nobs=932, auto.var=TRUE,
               auto.fix.first=TRUE)
mi1 <- modificationIndices(fit1, sort.=TRUE)

# specify Model 2 (with error covariances added)
model2 <- "
# measurement model
SES =~ educ + sei
ALIEN67 =~ anomia67 + powerl67
ALIEN71 =~ anomia71 + powerl71

# structural model
ALIEN67 ~ a*SES
ALIEN71 ~ b*ALIEN67 + c*SES

# error covariances
anomia67 ~~ anomia71
powerl67 ~~ powerl71

# effect decomposition
direct := c
indirect := a*b
total := direct + indirect

# comparing direct vs indirect effect
effect_d := indirect - direct
"

# fit Model 2 to data
fit2 <- lavaan(model2, sample.cov=alien.cov, sample.cov.rescale=FALSE, sample.nobs=932, auto.var=TRUE,
               auto.fix.first=TRUE)

# save the output
sink("alien.out", split=TRUE)
writeLines("\n Example 1: Stability of Alienation\n")
writeLines("\n Output for Model 1\n")
inspect(fit1)
summary(fit1, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
writeLines("\n Modification indices\n")
list(mi1)
writeLines("\n Output for Model 2\n")
inspect(fit2)
summary(fit2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
writeLines("\n Comparing Model 1 and Model 2\n")
lavTestLRT(fit1, fit2)
sink()
