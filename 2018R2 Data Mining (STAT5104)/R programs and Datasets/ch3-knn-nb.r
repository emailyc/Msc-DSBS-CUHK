# Ch3

# KNN
# iris data
d<-read.csv("iris.csv")				# read in dataset
set.seed(123)					# set random seed
n<-nrow(d)					# get sample size
r<-2/3						# set sampling ratio
id<-sample(1:n,size=round(r*n),replace=F)	# generate random integers for sampling
d1<-d[id,]					# training dataset
d2<-d[-id,]					# testing dataset
dim(d1)						# display dimension of d1
dim(d2)						# display dimension of d2
names(d)					# display names in d

library(class)				# load library class
cl<-factor(d1[,5])			# create a factor object for the class label of d1
iris.knn<-knn(d1[,1:4],d2[,1:4],cl,k=3)	# apply knn with k=3

table(iris.knn,d2$Species)		# classification table
readline("Hit <Return> to continue:")


# hmeq data
d<-read.csv("hmeq1.csv")
(n<-nrow(d))					# get and display sample size
names(d)

set.seed(123)					# set random seed
r<-2/3						# set sampling ratio
id<-sample(1:n,size=round(r*n),replace=F)	# create id for training data
d1<-d[id,]					# training data
d2<-d[-id,]					# testing data
x1<-d[id,c(1:4,7:13)]				# exclude columns 5 and 6
x2<-d[-id,c(1:4,7:13)]

cl<-factor(x1[,1])				# create factor for the label of x1
hmeq.knn<-knn(x1[,2:11],x2[2:11],cl,k=3)	# apply knn with k=3
table(hmeq.knn,x2[,1])				# classification table
readline("Hit <Return> to continue:")


# Using standardize transformation
source("stand.r")	# load the function stand
x<-rbind(x1,x2)		# combine x1 and x2 row-wise
n<-dim(x)[1]		# get row dim of x
n1<-dim(x1)[1]		# get row dim of x1
z<-stand(x)		# standardize x
z1<-z[1:n1,]		# get z1 from z
z2<-z[(n1+1):n,]	# get z2 from z


# improved version k_nn
source("k_nn.r")				  # use improved version k_nn

hmeq.knn<-k_nn(z1[,2:11],z2[,2:11],cl,x2[,1],v=5) # k_nn using z1 and z2 with k=1 to 5
table(hmeq.knn,x2[,1])				  # misclassification table
						
readline("Hit <Return> to continue:")


# Scale to [0,1]
source("scale.r")			# load scale function
z1<-scale.con(d1[,-c(1,5,6)])		# transform continuous or ordinal var.		
z2<-scale.con(d2[,-c(1,5,6)])
w1<-scale.dum(d1[,5:6])			# transform binary or categorical var.
w2<-scale.dum(d2[,5:6])

z1<-cbind(z1,w1)			# combine z and w column-wise
z2<-cbind(z2,w2)
hmeq.knn<-k_nn(z1,z2,cl,x2[,1],v=5)	# k_nn using z1 and z2 with k=1 to 5
table(hmeq.knn,x2[,1])			# misclassification table

readline("Hit <Return> to continue:")


# Titanic data
d<-read.csv("titanic.csv")	# read in data
set.seed(12345)			# set random seed
n<-nrow(d)			# get sample size
r<-0.9				# set sampling ratio
id<-sample(1:n,round(r*n))	# generate id for d1
d1<-d[id,]			# training data
d2<-d[-id,]			# testing data

cl<-factor(d1[,4])		# change target variable to factor
x1<-d1[,1:3]
x2<-d2[,1:3]
w1<-scale.dum(x1)		# using scale.dum() on x1 and x2
w2<-scale.dum(x2)

titan.knn<-k_nn(w1,w2,cl,d2[,4],v=5)	# knn with k=1 to 5
table(titan.knn,d2[,4])			# classification table

readline("Hit <Return> to continue:")


# Naive Bayes Classifier
library(e1071)				# load library e1071
d<-read.csv("iris.csv")			# read in IRIS data
n<-nrow(d)				# get sample size
r<-2/3					# set sampling ratio
set.seed(123)				# set random seed
id<-sample(1:150,size=100,replace=F)	# create id for d1	
d1<-d[id,]				# training data
d2<-d[-id,]				# testing data

cl<-factor(d1[,5])			# define class label
iris.nb<-naiveBayes(d1[,1:4],cl)	# apply naiveBayes
pr<-predict(iris.nb,d2[,1:4])		# predict
table(pr,d2[,5])			# classification table

readline("Hit <Return> to continue:")

d<-read.csv("titanic.csv")		# read in Titanic data
n<-nrow(d)
r<-0.9
set.seed(12345)
id<-sample(1:n,round(r*n))		# default is replace=F
d1<-d[id,]				# training data
d2<-d[-id,]				# testing data

cl<-factor(d1[,4])			# define class label
titan.nb<-naiveBayes(d1[,1:3],cl)	# apply naiveBayes
pr<-predict(titan.nb,d2[1:3])		# predict
table(pr,d2[,4])			# classification table

readline("Hit <Return> to continue:")

d<-read.csv("hmeq1.csv")		# read in HMEQ data
set.seed(123)				# set random seed
id<-sample(1:3067,2045)

c1<-factor(d[,1])			# change categorical var to factor
c8<-factor(d[,8])
c9<-factor(d[,9])
c11<-factor(d[,11])
x2<-d[,c(2,3,4,7,10,12,13)]		# select continuous var.
x<-cbind(c1,c8,c9,c11,x2)		# combine var. column-wise to form x

d1<-x[id,]				# training data
d2<-x[-id,]				# testing data
cl<-factor(d1[,1])			# class label
hmeq.nb<-naiveBayes(d1[,2:11],cl)	# apply naiveBayes
pr<-predict(hmeq.nb,d2[,2:11])		# predict
table(pr,d2[,1])			# classification table


