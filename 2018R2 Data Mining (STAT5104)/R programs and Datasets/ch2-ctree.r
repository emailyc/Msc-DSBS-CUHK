# ch2 CTREE

# iris data
d<-read.csv("iris.csv")		# read in iris
dim(d)				# display dimension of d
names(d)			# display var names in d

library(rpart)			# load rpart library

ctree<-rpart(Species~Sepal_len+Sepal_wid+Petal_len+Petal_wid,data=d,method="class")
readline("Hit <Return> to continue:")

library(rpart.plot)		# load rpart.plot library
rpart.plot(ctree,extra=101)	# use rpart.plot with option extra=101
readline("Hit <Return> to continue:")

print(ctree)			# print ctree

# plot Petal_wid vs Petal_len with diff color for each species
plot(d$Petal_len,d$Petal_wid,pch=21,bg=c("red","blue","green")[d$Species])
abline(h=1.75)			# add a horizontal line
abline(v=2.45)			# add a vertical line
readline("Hit <Return> to continue:")

pr<-predict(ctree)		# pr has 3 columns of prob.
cl<-max.col(pr)			# find the column index of max in each row of pr
table(cl,d$Species)		# cross tabulation table
readline("Hit <Return> to continue:")


# hmeq data
d<-read.csv("hmeq1.csv")			# read in dataset
(n<-nrow(d))					# get no. of row and display
names(d)					# display var names

set.seed(5104)					# set the random seed
r<-2/3						# set sampling ratio
id<-sample(1:n,size=round(r*n),replace=F) 	# sample r*n random integers from 1 to n
d1<-d[id,]					# training dataset
d2<-d[-id,]					# testing dataset
dim(d1)
dim(d2)
names(d)

library(rpart)					# load rpart library
y<-d1$BAD					# create target y
ctree<-rpart(y~.,data=d1[,2:13],method="class")	# alternate way to specify model
readline("Hit <Return> to continue:")

rpart.plot(ctree,extra=101,box.palette="Grays")	# use grays level instead of color
readline("Hit <Return> to continue:")

# use minsplit and maxdepth to control the CTREE
ctree<-rpart(y~.,data=d1[,2:13],method="class",minsplit=40,maxdepth=4) 
readline("Hit <Return> to continue:")

rpart.plot(ctree,extra=101,box.palette="Grays")	# use grays level instead of color

print(ctree)
readline("Hit <Return> to continue:")

pr<-predict(ctree)	# prediction on training dataset d1
cl<-max.col(pr)		# find col. index of max in each row of pr
table(cl,d1$BAD)	# classification table
readline("Hit <Return> to continue:")

pr<-predict(ctree,d2)	# or using predict() function on d2
cl<-max.col(pr)
table(cl,d2$BAD)	# classification table


# Titanic data
d<-read.csv("titanic.csv")	# read in data
names(d)			# display variables

set.seed(12345)			# set random seed
(n<-nrow(d))			# get and display sample size
r<-0.9				# set sampling ratio
id<-sample(1:n,round(r*n))	# create random sample
d1<-d[id,]			# select training data
d2<-d[-id,]			# select testing data
dim(d1)
dim(d2)

ctree<-rpart(Survive~Class+Age+Sex,data=d1,method="class")
readline("Hit <Return> to continue:")

rpart.plot(ctree,extra=101,box.palette="Grays")	# use grays level instead of color
readline("Hit <Return> to continue:")

pr<-predict(ctree)				# prediction on training data d1
cl<-max.col(pr)
table(cl,d1$Survive)

table(d2$Survive,d2$Class,d2$Sex,d2$Age)      	# display multi-way table

pr<-predict(ctree,d2)				# prediction on testing data d2
cl<-max.col(pr)
table(cl,d2$Survive)



# random forest using HMEQ data
d<-read.csv("hmeq1.csv")		# read in dataset
names(d)				# display variables
dim(d)					# display dimension

set.seed(5104)				# set the random seed
n<-nrow(d)				# get sample size
r<-2/3					# set sampling ratio
id<-sample(1:n,round(r*n),replace=F) 	# sample r*n random integers from 1 to n
d1<-d[id,]				# training dataset
d2<-d[-id,]				# testing dataset

n1<-nrow(d1)			# get sample size of d1
n2<-nrow(d2)			# get sample size of d2
ns<-100				# set no. of tree in random forest

pred<-matrix(0,nrow=n2,ncol=ns)	# initialize the prediction results to zero
for (i in 1:ns) {		# loop for ns
  id<-sample(1:n1,replace=T)	# draw n1 random integers with replacement
  ds<-d1[id,]			# draw random sample
  ctree<-rpart(ds[,1]~.,data=ds[,2:13],method="class",minsplit=40,maxdepth=4) 
  pr<-predict(ctree,d2)		# prediction on d2
  cl<-max.col(pr)-1		# change predition to 0 or 1
  pred[,i]<-cl			# save prediction to pred
}

pr<-apply(pred,1,mean)		# compute row mean of pred
pr1<-(pr>0.5)+0			# make final prediction
table(pr1,d2$BAD)		# classfication table for random forest




