# ch4

d<-read.csv("hmeq1.csv")		# read in dataset
(n<-nrow(d))				# get and display sample size
names(d)				# display names of d

set.seed(123)				# set random seed
r<-2/3
id<-sample(1:n,size=round(r*n),replace=F)
d1<-d[id,]				# training dataset
d2<-d[-id,]				# testing dataset

summary(glm(BAD~LOAN+MORTDUE+VALUE+YOJ+DEROG+DELINQ+CLAGE+NINQ+CLNO
	+DEBTINC,data=d1,binomial))	# apply glm, last option binomial means logistic reg.
readline("Hit <Return> to continue:")

summary(glm(BAD~LOAN+MORTDUE+YOJ+DEROG+DELINQ+CLAGE+NINQ+CLNO+DEBTINC,
	data=d1,binomial))		# fit the logistic reg. model without VALUE
readline("Hit <Return> to continue:")

summary(glm(BAD~YOJ+DEROG+DELINQ+CLAGE+NINQ+DEBTINC,data=d1,binomial))
lreg<-glm(BAD~YOJ+DEROG+DELINQ+CLAGE+NINQ+DEBTINC,data=d1,binomial)

pr<-lreg$fit>0.5			# pr=T if prob>0.5
table(pr,d1$BAD)			# classification table

(b<-lreg$coef)				# save and display the coefficient
names(d2)				# display names in d2

x<-cbind(1,d2[,c(7,8,9,10,11,13)])	# select the suitable columns from d2
b<-as.matrix(b)				# convert b into matrix
x<-as.matrix(x)				# convert x into matrix
prob<-exp(x%*%b)/(1+exp(x%*%b))		# compute pr using (4.3)
pr<-prob>0.5				# create label for prediction
table(pr,d2$BAD)
readline("Hit <Return> to continue:")

# using predict() function
pr<-predict(lreg,d2)			# use predict() on the testind data d2
prob<-exp(pr)/(1+exp(pr))		# compute pr using (4.3)
cl<-prob>0.5				# create label for prediction
table(cl,d2$BAD)
readline("Hit <Return> to continue:")

# Dummy variable in logistic regression

table(d1$DEROG)				# dist. of DEROG
table(d1$DELINQ)			# dist. of DELINQ
g1<-as.numeric(d1$DEROG>0)		# convert DEROG to binary var. g1
h1<-as.numeric(d1$DELINQ>0)		# convert DELINQ to binary var. h1

table(g1)				# dist. of g1
table(h1)				# dist. of h1

lreg<-glm(BAD~YOJ+CLAGE+NINQ+DEBTINC+g1+h1+g1*YOJ+g1*CLAGE+g1*NINQ+g1*DEBTINC
	+h1*YOJ+h1*CLAGE+h1*NINQ+h1*DEBTINC,data=d1,binomial)
summary(lreg)
readline("Hit <Return> to continue:")

lreg<-glm(BAD~CLAGE+NINQ+DEBTINC+g1+h1+h1*CLAGE+h1*DEBTINC,data=d1,binomial)
summary(lreg)

pr<-lreg$fit>0.5
table(pr,d1$BAD)
readline("Hit <Return> to continue:")

# lift chart

ysort<-d1$BAD[order(lreg$fit,decreasing=T)]	# sort y according to lreg$fit
n<-length(ysort)				# get length of ysort
perc1<-cumsum(ysort)/1:n			# compute cumulative percentage
plot(perc1,type="l")				# plot perc with line type
abline(h=sum(d1$BAD)/n)				# add the baseline
readline("Hit <Return> to continue:")


perc2<-cumsum(ysort)/sum(ysort)			# cum perc of success 
pop<-(1:n)/n					# x-coordinate
plot(pop,perc2,type="l")			# plot
lines(pop,pop)					# add the reference line
readline("Hit <Return> to continue:")


# stepwise
lreg<-glm(BAD~LOAN+MORTDUE+VALUE+YOJ+DEROG+DELINQ+CLAGE+NINQ+CLNO+DEBTINC,data=d1,binomial)
step(lreg)					# perform stepwise selection


# multinomial logit

d<-read.csv("iris.csv")				# read in data
names(d)					# display names

library(nnet)					# load nnet library
mnl<-multinom(Species~Sepal_len+Sepal_wid+Petal_len+Petal_wid,data=d)	# perform MNL
summary(mnl)					# display summary of MNL

pred<-predict(mnl)				# prediction
table(pred,d$Species)				# tabulate pred vs true species
