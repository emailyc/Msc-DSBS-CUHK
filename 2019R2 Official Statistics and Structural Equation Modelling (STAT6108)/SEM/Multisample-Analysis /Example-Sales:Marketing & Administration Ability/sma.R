# Example: Sales/Marketing and Administration Ability

# set work directory
setwd("c:/users/wchan/google drive/stat6108/data")

# load the lavaan package
library(lavaan)

# data preparation
varname <- c("sales1", "sales2", "admin1", "admin2")
cov1 <- scan("sma_male.cov")
male.cov <- getCov(cov1, names=varname)
cov2 <- scan("sma_female.cov")
female.cov <- getCov(cov2, names=varname)

# specify Model 1 (configural invariance)
model1 <- "
# measurement model
S_M =~ sales1 + sales2
ADM =~ admin1 + admin2
# factor Variances and covariance
S_M ~~ 1*S_M
ADM ~~ 1*ADM
S_M ~~ ADM
# error Variances
sales1 ~~ sales1
sales2 ~~ sales2
admin1 ~~ admin1
admin2 ~~ admin2
"
# Fit Model 1 to data 
fit1 <-lavaan(model1, sample.cov=list(Group1=male.cov, Group2=female.cov),  sample.nobs=c(265, 300))

# specify Model 2 (weak factorial invariance)
model2 <- "
# measurement model
S_M =~ c(eq1,eq1)*sales1 + c(eq2,eq2)*sales2
ADM =~ c(eq3,eq3)*admin1 + c(eq4,eq4)*admin2
# factor Variances and covariance
S_M ~~ c(1,NA)*S_M
ADM ~~ c(1,NA)*ADM
S_M ~~ ADM
# error Variances
sales1 ~~ sales1
sales2 ~~ sales2
admin1 ~~ admin1
admin2 ~~ admin2
"
# Fit Model 2 to data 
fit2 <-lavaan(model2, sample.cov=list(Group1=male.cov, Group2=female.cov),  sample.nobs=c(265, 300))

# specify Model 3 (Model 2 + equal factor variances)
model3 <- "
# measurement model
S_M =~ c(eq1,eq1)*sales1 + c(eq2,eq2)*sales2
ADM =~ c(eq3,eq3)*admin1 + c(eq4,eq4)*admin2
# factor Variances and covariance
S_M ~~ c(1,1)*S_M
ADM ~~ c(1,1)*ADM
S_M ~~ ADM
# error Variances
sales1 ~~ sales1
sales2 ~~ sales2
admin1 ~~ admin1
admin2 ~~ admin2
"
# Fit Model 3 to data 
fit3 <-lavaan(model3, sample.cov=list(Group1=male.cov, Group2=female.cov),  sample.nobs=c(265, 300))

# specify Model 4 (Model 3 + equal factor correlation)
model4 <- "
# measurement model
S_M =~ c(eq1,eq1)*sales1 + c(eq2,eq2)*sales2
ADM =~ c(eq3,eq3)*admin1 + c(eq4,eq4)*admin2
# factor Variances and covariance
S_M ~~ c(1,1)*S_M
ADM ~~ c(1,1)*ADM
S_M ~~ c(eq5,eq5)*ADM
# error Variances
sales1 ~~ sales1
sales2 ~~ sales2
admin1 ~~ admin1
admin2 ~~ admin2
"
# Fit Model 4 to data 
fit4 <-lavaan(model4, sample.cov=list(Group1=male.cov, Group2=female.cov),  sample.nobs=c(265, 300))

# specify Model 5 (Model 4 + equal error variances)
model5 <- "
# measurement model
S_M =~ c(eq1,eq1)*sales1 + c(eq2,eq2)*sales2
ADM =~ c(eq3,eq3)*admin1 + c(eq4,eq4)*admin2
# factor Variances and covariance
S_M ~~ c(1,1)*S_M
ADM ~~ c(1,1)*ADM
S_M ~~ c(eq5,eq5)*ADM
# error Variances
sales1 ~~ c(eq6,eq6)*sales1
sales2 ~~ c(eq7,eq7)*sales2
admin1 ~~ c(eq8,eq8)*admin1
admin2 ~~ c(eq9,eq9)*admin2
"
# Fit Model 5 to data 
fit5 <-lavaan(model5, sample.cov=list(Group1=male.cov, Group2=female.cov),  sample.nobs=c(265, 300))

# specify Model 6 (equality of unstructured covariance matrices)
model6 <- "
# measurement model
F1 =~ 1*sales1
F2 =~ 1*sales2
F3 =~ 1*admin1
F4 =~ 1*admin2
# factor Variance
F1 ~~ c(eq1,eq1)*F1
F2 ~~ c(eq2,eq2)*F2
F3 ~~ c(eq3,eq3)*F3
F4 ~~ c(eq4,eq4)*F4
# factor covariance
F2 ~~ c(eq5,eq5)*F1
F3 ~~ c(eq6,eq6)*F1
F3 ~~ c(eq7,eq7)*F2
F4 ~~ c(eq8,eq8)*F1
F4 ~~ c(eq9,eq9)*F2
F4 ~~ c(eq10,eq10)*F3
"
# Fit Model 6 to data 
fit6 <-lavaan(model6, sample.cov=list(Group1=male.cov, Group2=female.cov),  sample.nobs=c(265, 300))

# save the output
sink("sma.out", split=TRUE)
writeLines("\n Example: Sales/Marketing and Administration Ability\n")
writeLines("\n Output for Model 1 (Configural Invariance)\n")
summary(fit1, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Output for Model 2 (Weak Factorial Invariance)\n")
summary(fit2, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Output for Model 3 (Model 2 + equal factor variances)\n")
summary(fit3, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Output for Model 4 (Model 3 + equal factor correlation)\n")
summary(fit4, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Output for Model 5 (Model 4 + equal error variances)\n")
summary(fit5, fit.measures=TRUE, standardized=TRUE)
writeLines("\n Model Comparisons\n")
lavTestLRT(fit1, fit2, fit3, fit4, fit5)
writeLines("\n Output for Model 6 (equality of unstructured covariance matrices)\n")
summary(fit6, fit.measures=TRUE, standardized=TRUE)
sink()
