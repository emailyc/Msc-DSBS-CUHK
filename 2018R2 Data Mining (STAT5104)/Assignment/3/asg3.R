library(dplyr)
library(ROCR)
library(ggplot2)
set.seed(17920)

kmstat<-function(x,k) {
  km<-kmeans(x,k)			# k-means clustering with k=3
  ng<-km$size				# sample size
  n<-dim(x)[1]
  ssw<-sum(km$withinss)				# compute total within group ss
  ssb<-km$betweenss				# between group ss
  out<-list((n-k)*ssb/((k-1)*ssw),ng,km$cluster)# save stat, ng and cluster index into a list
  names(out)<-c("stat","size","cluster")	# apply names to list
  out						# output
}

km<-function(x,k,try=5) {		# default no. of trial is 5
  
  res0<-kmstat(x,k)			# save the result of the first trial
  r0<-res0$stat				# save the stat from the first trial
  
  for (i in 2:try) {
    res<-kmstat(x,k)  			# new trial 
    if (res$stat>r0) {			# if new trial is better
      r0<-res$stat			# update r0 and res
      res0<-res
    }
  }
  cat("cluster size=",res0$size,"\n")	# display cluster size
  cat("stat=",res0$stat,"\n")		# display stat
  res0$cluster				# output cluster label
}  

# function for standardize transformation
stand<-function(x) {
  n<-dim(x)[1]		# row dim of x
  p<-dim(x)[2]		# column dim of x
  m<-apply(x,2,mean)	# compute column mean
  s<-apply(x,2,sd)	# compute column sd
  m<-matrix(m,nr=n,nc=p,byrow=T)  # convert m into nxp matrix, each row is m
  s<-matrix(s,nr=n,nc=p,byrow=T)  # convert s into nxp matrix, each row is s
  (x-m)/s		# output standardize score
}

ann<-function(x,y,size,maxit=100,linout=F,trace=F,try=5) {
  ann1<-nnet::nnet(y~.,data=x,size=size,maxit=maxit,linout=linout,trace=trace)
  v1<-ann1$value
  
  for (i in 2:try) {
    ann<-nnet::nnet(y~.,data=x,size=size,maxit=maxit,linout=linout,trace=trace)
    if (ann$value<v1) {
      v1<-ann$value
      ann1<-ann
    }
  }
  ann1
} 



d <- read.csv("tele.csv", header = TRUE, sep = ",") #read data
inTrain <- caret::createDataPartition(d$Change,     #create index for train / test partition
                                      p = .8,
                                      list = FALSE)

d0 <- d[inTrain,]                                   #select observations for training 
d1 <- d[-inTrain,]                                  #select observations for testing



y0 <- factor(d0[,18],
             labels = c("stay", "change"),
             levels = c(0, 1))
y1 <- factor(d1[,18],
             labels = c("stay", "change"),
             levels = c(0, 1))

continuous <- c(5, 7, 8, 10, 11, 13, 14, 16)      #Column index for continuous variables
x0 <- d0[, continuous]                            #Select continuous variables for training data
x1 <- d1[, continuous]                            #Select continuous variables for testing data

fit <- glm(y0 ~ .,
           data=x0,
           family = binomial(link='logit'))       #Logistic Regression

tel.lreg <- fit %>%
  MASS::stepAIC(.,
                trace = FALSE,
                direction = "backward")



train.probabilities <- predict(tel.lreg ,type='response')
train.predicted.classes <- ifelse(train.probabilities > 0.5, 1, 0) %>%
  factor(.,
         labels = c("stay", "change"),
         levels = c(0, 1))
train.classification.table <- table(y0, train.predicted.classes)
train.err.rate <- (train.classification.table[1,2] +
                     train.classification.table[2,1]) / sum(train.classification.table)
train.classification.table

test.probabilities <- predict(tel.lreg, newdata = x1 ,type='response')
test.predicted.classes <- ifelse(test.probabilities > 0.5, 1, 0) %>%
  factor(.,
         labels = c("stay", "change"),
         levels = c(0, 1))
test.classification.table <- table(y1, test.predicted.classes)
test.err.rate <- (test.classification.table[1,2] +
                    test.classification.table[2,1]) / sum(test.classification.table)
q1.err.rate = test.err.rate
test.classification.table


x0 <- d0[, continuous]                            #Select continuous variables for training data
x1 <- d1[, continuous]                            #Select continuous variables for testing data
y0 <- d0[,18]
y1 <- d1[,18]                                     #label



tele.nn = ann(x = x0, y = y0, size = 7, linout = T, try = 30)

train.predicted.classes <- round(tele.nn$fitted.values) %>%
  factor(.,
         labels = c("stay", "change"),
         levels = c(0, 1))
train.classification.table <- table(y0, train.predicted.classes)
train.err.rate <- (train.classification.table[1,2] + train.classification.table[2,1])  / sum(train.classification.table)
train.classification.table

test.predicted.classes <- predict(tele.nn, newdata = x1) %>%
  round(.) %>%
  factor(.,
         labels = c("stay", "change"),
         levels = c(0, 1))
test.classification.table <- table(y1, test.predicted.classes)
test.err.rate <- (test.classification.table[1,2] +
                    test.classification.table[2,1]) / sum(test.classification.table)
test.classification.table


ann.test.err.rates <- c(test.err.rate)
for (i in 8:9)
{
  tele.nn = ann(x = x0, y = y0, size = i, linout = T, try = 30)
  test.predicted.classes <- predict(tele.nn, newdata = x1) %>%
    round(.) %>%
    factor(.,
           labels = c("stay", "change"),
           levels = c(0, 1))
  test.classification.table <- table(y1, test.predicted.classes)
  test.err.rate <- (test.classification.table[1,2] +
                      test.classification.table[2,1]) / sum(test.classification.table)
  ann.test.err.rates = c(ann.test.err.rates, test.err.rate)
}

names(ann.test.err.rates) <- c("Size = 7", "Size = 8", "Size = 9")


x <- d[, continuous]            #Extract all the continuous variables in d
z <- stand(x)


tel.km2 <- km(x = z, try = 10, k = 2)
tel.km3 <- km(x = z, try = 10, k = 3)
tel.km4 <- km(x = z, try = 10, k = 4)
tel.km5 <- km(x = z, try = 10, k = 5)


tel.km2 <- km(x = z, try = 10, k = 2)
tel.km3 <- km(x = z, try = 10, k = 3)
tel.km4 <- km(x = z, try = 10, k = 4)
tel.km5 <- km(x = z, try = 10, k = 5)


