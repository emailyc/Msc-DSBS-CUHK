# ch5 ANN

library(nnet)				# load library nnet
d<-read.csv("iris.csv")			# read in data

iris.nn<-nnet(Species~Sepal_len+Sepal_wid+Petal_len+Petal_wid,data=d,size=2,linout=T) 
summary(iris.nn)			# summary of output
readline()

pred<-round(iris.nn$fitted.values)	# prediction
table(d$Species,pred)			# classification table
readline()

# set trace=F to turn off the obj function value in iterations
iris.nn<-nnet(Species~Sepal_len+Sepal_wid+Petal_len+Petal_wid,data=d,size=2,linout=T,trace=F) 
summary(iris.nn)			# summary of output
readline()

pred<-round(iris.nn$fitted.values)	# prediction
table(d$Species,pred)			# classification table
readline()

# improved version of nnet()

source("ann.r")				# load ann()
iris.nn<-ann(d[,1:4],d[,5],size=2,linout=T,try=10) 
iris.nn$value				# display best value
summary(iris.nn)			# summary of output
pred<-round(iris.nn$fitted.values)	# prediction
table(pred,d$Species)			# classification table
readline()

# ANN with logistic output

y<-as.factor(d$Species)			# change the output to factor
iris.nn<-ann(d[,1:4],y,size=2,maxit=200,try=10)
iris.nn$value

summary(iris.nn)
pred<-max.col(iris.nn$fit)
table(d$Species,pred)
readline()

# Backpropagation algorithm

logistic<-function(x) {1/(1+exp(-x))} 				# define activiation function
x<-matrix(c(0.4,0.7,0.8,0.9,1.3,1.8,-1.3,-0.9),ncol=2,byrow=T)	# input x
x<-cbind(1,x)							# attach a column of ones
y<-c(0,0,1,0) 							# target value
w1<-matrix(c(0.1,-0.2,0.1,0.4,0.2,0.9),byrow=T,nrow=2) 		# hidden layer weights
w2<-c(0.2,-0.5,0.1)						# output layer weights

h<-rbind(1,logistic(w1%*%t(x)))		# compute h = [1,f(w1 X')]
out<-logistic(w2%*%h)			# compute output value = f(w2 h)
e1<-y-out				# compute output error = y - out
sse<-sum(e1^2)				# compute sum of squared error = sum(error^2)
e1
sse					# display sse

lr<-0.5					# learning rate
del2<-out*(1-out)*e1			# output layer
del_w2<-2*lr*del2%*%t(h)		# change in w2
new_w2<-w2+del_w2			# new output layer weights

del1<-h*(1-h)*(w2%*%del2)		# hidden layer
del1<-del1[c(2,3),]			# select only the 2nd and 3rd row
del_w1<-2*lr*del1%*%x			# change in w1
new_w1<-w1+del_w1			# new hidden layer weights

err<-y-logistic(new_w2%*%rbind(1,logistic(new_w1%*%t(x))))	# new error
sse<-sum(err^2)							# final error
sse
readline()


# ANN for prediction
d<-read.csv("iris.csv")
n<-nrow(d)
r<-0.8

set.seed(12345)
id<-sample(1:n,size=round(r*n),replace=F)	# index for testing dataset
d1<-d[id,]					# training dataset
d2<-d[-id,]					# testing dataset
				
iris.nn<-ann(d1[,1:4],d1[,5],size=2,linout=T,maxit=200,try=10)
iris.nn$value	
summary(iris.nn)
readline()

w1<-matrix(iris.nn$wts[1:10],nr=2,byrow=T) 	# weights matrix from i to h
w2<-matrix(iris.nn$wts[11:13],nr=1,byrow=T)	# weights matrix from h to o
w1
w2

logistic<-function(x) { 1/(1+exp(-x)) }		# define the logistic function
x<-cbind(1,d2[,1:4])				# form the matrix x, note that a column
dim(x)						# of ones corresponds to the bias in w1

out1<-logistic(w1%*%t(x))			# This is the output h'
out1<-rbind(1,out1)				# append a row of ones on top of out1
dim(out1)					# corresponds to the bias in w2
readline()

out2<-t(w2%*%out1)		# compute fitted values
dim(out2)

pr<-round(out2)			# round to nearest integer
table(pr,d2[,5])		# classification table for testing dataset
readline() 

# using the predict() function
pr<-predict(iris.nn,d2)
table(round(pr),d2[,5])		# round pr to nearest integer
readline()


# logistic output
y<-as.factor(d1[,5])				# change y to factor
iris.nn<-ann(d1[,1:4],y,size=2,maxit=200,try=10)
iris.nn$value

w1<-matrix(iris.nn$wts[1:10],nr=2,byrow=T)
w2<-matrix(iris.nn$wts[11:19],nr=3,byrow=T)
x<-cbind(1,d2[,1:4])				# form the matrix x, note that a column
out1<-logistic(w1%*%t(x))			# This is the output h'
out1<-rbind(1,out1)				# append a row of ones on top of out1
out2<-t(w2%*%out1)				# compute fitted values
dim(out2)					# dimension of out2

readline()
# using the predict() function
pr<-max.col(out2)				# find the column corresponds to max
table(pr,d2[,5])				# classification table
readline()


# HMEQ exmaple
d<-read.csv("hmeq1.csv")		# read in dataset
n<-nrow(d)
r<-2/3

set.seed(123)				# set the random seed
id<-sample(1:n,round(r*n),replace=F) 	# sample 2140 random integers from 1 to 3211
d1<-d[id,]				# training dataset
d2<-d[-id,]				# testing dataset
dim(d1)

y1<-as.factor(d1[,1])			# convert into factor
y2<-as.factor(d2[,1])
readline()

# with columns 8,9,13			# using variables appears in CTREE
source("ann.r")				# load ann()
hmeq.nn<-ann(d1[,c(8,9,13)],y1,size=3,maxit=500,try=30) # use logistic output
hmeq.nn$value				# display best value
summary(hmeq.nn)			# summary of output
pred<-(hmeq.nn$fitted.values>0.5)	# prediction
table(pred,y1)				# classification table

pr<-predict(hmeq.nn,d2)			# predict on testing data
pred<-(pr>0.5)
table(pred,y2)				# classification table for testing data
readline()

# using stand
source("stand.r")
z<-stand(d[,c(8,9,13)])
z1<-z[id,]
z2<-z[-id,]
hmeq.nn<-ann(z1,y1,size=3,maxit=500,try=30) # use logistic output
hmeq.nn$value				# display best value
summary(hmeq.nn)			# summary of output
pred<-(hmeq.nn$fitted.values>0.5)	# prediction
table(pred,y1)				# classification table

pr<-predict(hmeq.nn,z2)			# predict on testing data
pred<-(pr>0.5)
table(pred,y2)				# classification table for testing data
readline()

# using scale
source("scale.r")
z<-scale.con(d[,c(8,9,13)])
z1<-z[id,]
z2<-z[-id,]
hmeq.nn<-ann(z1,y1,size=3,maxit=500,try=30) # use logistic output
hmeq.nn$value				# display best value
summary(hmeq.nn)			# summary of output
pred<-(hmeq.nn$fitted.values>0.5)	# prediction
table(pred,y1)				# classification table

pr<-predict(hmeq.nn,z2)			# predict on testing data
pred<-(pr>0.5)
table(pred,y2)				# classification table for testing data


# Titanic data

# define function to recode character vector v to numeric vector
codechr<-function(v) {
  n<-length(v)		# length of v
  h<-levels(v)		# store levels in v to h
  k<-length(h)		# no. of categories in v
  p<-outer(v,h,"==")	# nxk matrix of logical values 
  q<-matrix(1:k,nrow=n,ncol=k,byrow=T)	# nxk matrix, each row is 1:k
  apply(p*q,1,sum)	# output numeric code
}  


d<-read.csv("titanic.csv")	# read in data
n<-nrow(d)			# get sample size
v1<-d[,1]			# recode column 1 to x1
x1<-codechr(v1)
v2<-d[,2]			# recode column 2 to x2
x2<-codechr(v2)
v3<-d[,3]			# recode column 3 to x3
x3<-codechr(v3)
v4<-d[,4]			# recode column 4 to x4
x4<-codechr(v4)
x<-cbind(x1,x2,x3,x4)		# combine column-wise to form x

set.seed(12345)			# set random seed
r<-0.9				# set sampling ratio
id<-sample(1:n,round(r*n))	# generate id for d1
d1<-x[id,]			# training data
d2<-x[-id,]			# testing data

# linear output
titan.nn<-ann(d1[,1:3],d1[,4],size=3,linout=T,maxit=200,try=20)
titan.nn$value			# display best value
pr<-round(titan.nn$fit)		# prediction for training data
table(pr,d1[,4])		# classification table for training data

pr<-round(predict(titan.nn,d2))	# prediction for testing data
table(pr,d2[,4])		# classification table for testing data
readline()

# logistic output
y<-factor(d1[,4])		# change target as factor
titan.nn<-ann(d1[,1:3],y,size=3,maxit=200,try=20)
titan.nn$value			# display best value
pr<-(titan.nn$fit>0.5)		# prediction for training data
table(pr,d1[,4])		# classification table for training data

pr<-predict(titan.nn,d2)	# prediction for testing data
pr<-(pr>0.5)
table(pr,d2[,4])		# classification table for testing data



