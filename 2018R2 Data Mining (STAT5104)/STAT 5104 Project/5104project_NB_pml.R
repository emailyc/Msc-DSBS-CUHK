# Naive Bayes Classifier
library(e1071)				# load library e1071
d<-read.csv("pml-training.csv")			# read in pml data

#Remove the first seven columns
d <- d[,-(1:7)];

#Remove NearZeroVariance variables
nzv <- caret::nearZeroVar(d,saveMetrics=TRUE)
d <- d[,nzv$nzv == FALSE]

#Clean variables with mostly NA
trainNA <- apply(d, 2, function(col){sum(is.na(col))/length(col)})
d <- d[,which(trainNA < .1)]

n<-nrow(d)				# get sample size
r<-0.7					# set sampling ratio 70%
set.seed(123)				# set random seed
id<-sample(1:n,size=r*n,replace=F)	# create id for d1	
d1<-d[id,]				# training data
d2<-d[-id,]				# testing data


cl<-factor(d1[,53])			# define class label
pml.nb<-naiveBayes(d1[,1:52],cl)	# apply naiveBayes
pr<-predict(pml.nb,d2[,1:52])		# predict
table(pr,d2[,53])			# classification table

