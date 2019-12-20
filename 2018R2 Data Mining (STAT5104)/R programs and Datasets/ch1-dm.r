# Chapter 1 Introduction

set.seed(12345)			# set random seed
d<-matrix(rnorm(5000),ncol=10)	# generate d
x1<-replace(d,d<(-2),NA)	# left truncation
x2<-replace(d,d>2,NA)		# right truncation
x3<-replace(x1,x1>2,NA)		# truncated at both ends
c1<-x1[complete.cases(x1),]	# complete cases
c2<-x2[complete.cases(x2),]
c3<-x3[complete.cases(x3),]

apply(d,2,mean)			# compute column mean
apply(c1,2,mean)
apply(c2,2,mean)
apply(c3,2,mean)

apply(d,2,sd)			# compute column sd
apply(c1,2,sd)
apply(c2,2,sd)
apply(c3,2,sd)
readline()

source("scale.r")		# load scale() function
d<-matrix(c(2,0,1,3,1,0,1,0,0,4,0,1),nrow=4,byrow=T)
d				# display d
cat2dum(d[,1])			# apply cat2dum to 1st col. of d
cat2dum(d[,2])			# apply cat2dum to 2nd col.
cat2dum(d[,3])			# apply cat2dum to 3rd col.
scale.dum(d)
readline()

d1<-scale.dum(d)		# apply scale.dum to d
d1
dist(d1,diag=T,upper=T,method="manhattan")	# compute distance matrix of d1
readline()

d<-read.csv("hmeq.csv",na.strings="")	# read in data set
dim(d)					# display the dimension of d
dc<-d[complete.cases(d),]		# select and save complete cases to dc
dim(dc)					# display dimension of dc
names(dc)				# display the names in dc
d1<-dc[(dc$BAD==1),]			# select BAD=1 
dim(d1)					# note that there is only 300 rows and 13 columns
d0<-dc[(dc$BAD==0),]			# select BAD=0
dim(d0)					# note that there is only 3064 rows and 13 columns

source("mdist.r")		# load the file contains the mdist function
md0<-mdist(d0[,-c(1,5,6)])	# compute distance and exclude columns 1,5,6
md1<-mdist(d1[,-c(1,5,6)])	# and save results to md0 and md1

readline("Hit <Return> to continue:")
par(mfrow=c(2,1))		# set up a 2x1 multframe graphic
plot(md0)			# plot md0
plot(md1)			# plot md1

readline("Hit <Return> to continue:")
c<-qchisq(0.99,df=10)
c
x0<-d0[md0<c,]		# select observations with md0<c from d0
x1<-d1[md1<c,]		# select observations with md1<c from d1
dim(x0)			# x0 contains 2789 cases
dim(x1)			# x1 contains 278 cases
x<-rbind(x0,x1)		# combine the matrices x0 and x1 row-wise
dim(x)			# x have 3067 rows and 13 columns

write.table(x,file="hmeq1.csv",sep=",",row.names=F) # save x to hmeq.csv
