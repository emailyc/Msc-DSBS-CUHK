# Example 3: Drug Use

# set work directory and load lavaan package
setwd("c:/users/wchan/google drive/stat6108/data")
library(lavaan)

# data preparation
data <- read.table("drug.dat")
data <- as.matrix(data)
corr <- data[1:12,]
sd <- data[13,]
mean <- data[14,]
cov <- cor2cov(corr, sd)
rownames(cov) <- colnames(cov)

# specify Model 1 (Curve-of-factors LGM)
model1 <- "
# measurement model
Time1 =~ la1*V1 + 1*V5 + la2*V9
Time2 =~ la1*V2 + 1*V6 + la2*V10
Time3 =~ la1*V3 + 1*V7 + la2*V11
Time4 =~ la1*V4 + 1*V8 + la2*V12
Int =~ 1*Time1 +1*Time2 + 1*Time3 + 1*Time4
Growth =~ 0*Time1 +1*Time2 + la3*Time3 + la4*Time4

# error variance
V1 ~~ V1 + V2 + V3 + V4
V2 ~~ V2 + V3 + V4
V3 ~~ V3 + V4
V4 ~~ V4
V5 ~~ V5 + V6 + V7 + V8
V6 ~~ V6 + V7 + V8
V7 ~~ V7 + V8
V8 ~~ V8
V9 ~~ V9 + V10 + V11 + V12
V10 ~~ V10 + V11 + V12
V11 ~~ V11 + V12
V12 ~~ V12

# intercepts
Time1 + Time2 + Time3 + Time4 ~ 0*1
Int + Growth ~ 1
V1 + V2 + V3 + V4 ~ 0*1
V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 ~ 1

# evaluating linear growth
linear1 := la3-2
linear2 := la4-3
"

# Fit Model 1 to data 
fit1 <-lavaan(model1, sample.cov=cov, sample.mean=mean, sample.nobs=357, auto.var=TRUE, auto.cov.lv.x=TRUE)

# specify Model 2 (Testing Linear Growth using LR test)
model2 <- "
# measurement model
Time1 =~ la*V1 + 1*V5 + lb*V9
Time2 =~ la*V2 + 1*V6 + lb*V10
Time3 =~ la*V3 + 1*V7 + lb*V11
Time4 =~ la*V4 + 1*V8 + lb*V12
Int =~ 1*Time1 +1*Time2 + 1*Time3 + 1*Time4
Growth =~ 0*Time1 +1*Time2 + 2*Time3 + 3*Time4

# error variance
V1 ~~ V1 + V2 + V3 + V4
V2 ~~ V2 + V3 + V4
V3 ~~ V3 + V4
V4 ~~ V4
V5 ~~ V5 + V6 + V7 + V8
V6 ~~ V6 + V7 + V8
V7 ~~ V7 + V8
V8 ~~ V8
V9 ~~ V9 + V10 + V11 + V12
V10 ~~ V10 + V11 + V12
V11 ~~ V11 + V12
V12 ~~ V12

# intercepts
Time1 + Time2 + Time3 + Time4 ~ 0*1
Int + Growth ~ 1
V1 + V2 + V3 + V4 ~ 0*1
V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 ~ 1
"

# Fit Model 2 to data 
fit2 <-lavaan(model2, sample.cov=cov, sample.mean=mean, sample.nobs=357, auto.var=TRUE, auto.cov.lv.x=TRUE)

# save the output
sink("drug.out", split=TRUE)
writeLines("\n Example 3: Drug Use\n")
writeLines("\n Output for Model 1 (Curve-of-factors LGM)\n")
summary(fit1, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Output for Model 2 (Testing Linear Growth using LRT)\n")
summary(fit2, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Model Comparisons\n")
lavTestLRT(fit1, fit2)
sink()
