# Example 1. Effect of SES on SWB: A comparison between USA and China

# set work directory
setwd("c:/users/wchan/google drive/stat6108/data")

# load the lavaan package
library(lavaan)
library(semPlot)

# data preparation

# group 1: USA
usa.corr <- "
1.00
 .48  1.00
 .22   .34  1.00
 .27   .22   .39  1.00
 .23   .22   .20   .12  1.00
 .27   .30   .23   .16   .64   1.00"
usa.sd <- c(1.36, 1.20, 1.19, 3.24, 3.90, 2.72)
usa.mean <- c(3.84, 3.29, 2.60,	6.44,	20.42, 10.07)

# group 2: China
china.corr <- "
1.00
 .44  1.00
 .22   .20  1.00
 .30   .18   .38  1.00
 .27   .27   .21   .08  1.00
 .27   .12   .25   .20   .66  1.00"
china.sd <- c(1.33, 1.28, 1.08, 2.65, 3.76, 2.68)
china.mean <- c(3.52, 3.08, 2.09, 5.36, 19.67, 9.56)
 
# assgin variable labels
varname <- c("parent", "edu", "occup", "income", "general", "work")
usa.cov <- getCov(usa.corr, sds=usa.sd, names=varname)
china.cov <- getCov(china.corr, sds=china.sd, names=varname)
names(usa.mean) <- names(china.mean) <- varname
 
# specify Model 1 (configural invariance)
model1 <- "
# measurement model
SES =~ 1*parent + edu + occup + income
SWB =~ 1*general + work
# error variance
parent ~~ parent
edu ~~ edu
occup ~~ occup
income ~~ income
general ~~ general
work ~~ work
# structural paths
SWB ~ SES
# factor and disturbance variance
SES ~~ SES
SWB ~~ SWB
# intercepts
parent + edu + occup + income + general + work ~ 1
"

# Fit Model 1 to data 
fit1 <-lavaan(model1, sample.cov=list(USA=usa.cov, China=china.cov), sample.mean=list(USA=usa.mean, China=china.mean),
              sample.nobs=c(200, 220))

# specify Model 2 (strong measurement invariance)
model2 <- "
# measurement model
SES =~ parent + edu + occup + income
SWB =~ general + work
# structural paths
SWB ~ c(ga1,ga2)*SES
# means/intercepts of latent variables
SES ~ c(al_x1,al_x2)*1
SWB ~ c(al_y1,al_y2)*1
# constraint the intercepts of the factors in China
al_x2 == 0
al_y2 == 0
# Define new parameters
# difference of gamma_11 between USA and China
gamma_d := ga2-ga1
# mean of SWB in USA
mean_SWB_USA := ga1*al_x1+al_y1
"
 
# Fit Model 2 to data 
fit2 <-lavaan(model2, sample.cov=list(USA=usa.cov, China=china.cov), sample.mean=list(USA=usa.mean, China=china.mean),
              sample.nobs=c(200, 220), auto.var=TRUE, auto.fix.first=TRUE, meanstructure=TRUE, int.ov.free=TRUE, 
              group.equal=c("loadings","intercepts"))

# specify Model 3 (using LR test to compare the effect of SES on SWB)
model3 <- "
# measurement model
SES =~ parent + edu + occup + income
SWB =~ general + work
# structural paths
SWB ~ c(ga,ga)*SES
# means/intercepts of latent variables
SES ~ c(al_x1,al_x2)*1
SWB ~ c(al_y1,al_y2)*1
# constraint the intercepts of the factors in China
al_x2 == 0
al_y2 == 0
# mean of SWB in USA
mean_SWB_USA := ga*al_x1+al_y1
"

# Fit Model 3 to data 
fit3 <-lavaan(model3, sample.cov=list(USA=usa.cov, China=china.cov), sample.mean=list(USA=usa.mean, China=china.mean),
              sample.nobs=c(200, 220), auto.var=TRUE, auto.fix.first=TRUE, meanstructure=TRUE, int.ov.free=TRUE, 
              group.equal=c("loadings","intercepts"))

# save the output
sink("china.out", split=TRUE)
writeLines("\n Example 1. Effect of SES on SWB: A comparison between USA and China\n")
writeLines("\n Output for Model 1 (Configural Invariance)\n")
inspect(fit1)
summary(fit1, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Output for Model 2 (Strong Factorial Invariance)\n")
summary(fit2, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Output for Model 3 (using LR test to compare the effect of SES on SWB)\n")
summary(fit3, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Model Comparisons\n")
lavTestLRT(fit1, fit2, fit3)
sink()

# output path diagrams
semPaths(fit1,"path","est",layout="tree2")
semPaths(fit2,"path","est",style="lisrel",layout="spring")
