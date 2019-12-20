

library(dplyr)
set.seed(17920)

library(class)
k_nn<-function(x0,x1,cl0,cl1,v,l=0,prob=F,use.all=T) {
  err0=1					# initialize error rate
  if (length(v)==1) v<-c(1:v)			# change v to an integer vector 1:v
  
  for (k in v) {		
    res<-knn(x0,x1,cl0,k,l,prob,use.all)	# apply knn
    ctab<-table(res,cl1)			# save c-table
    err<-1-sum(diag(ctab))/sum(ctab)		# compute error rate
    if (err<err0) {				# update if err<err0
      k0<-k
      res0<-res
      err0<-err
      ctab0<-ctab
    }
    cat("k=",k," error rate=",err,"\n")		# display results
  }  
  cat("best k=",k0," error rate=",err0,"\n")	# display best result
  res0						# output res0
}

# function to scale continuous or ordinal variables to [0,1]
scale.con<-function(d) {
  n<-dim(d)[1]		# row dim of d
  p<-dim(d)[2]		# column dim of d
  
  cmin<-apply(d,2,min)	# column min of d
  cmax<-apply(d,2,max)	# column max of d
  range<-cmax-cmin	# column range
  cmin<-matrix(cmin,nr=n,nc=p,byrow=T)	  # change cmin to a nxp matrix
  range<-matrix(range,nr=n,nc=p,byrow=T)  # change range to a nxp matrix
  
  (d-cmin)/range	# transform d
}  


##Q1

###a)
d <- read.csv("tele.csv", header = TRUE, sep = ",")           #read data
d[,18] <- factor(d$Change,
                 levels = c(0, 1),
                 labels = c("stay", "change"))

inTrain <- caret::createDataPartition(d$Change,               #create index for train / test partition
                                      p = .8,
                                      list = FALSE)


d0 <- d[inTrain,]                                             #select observations for training 
d1 <- d[-inTrain,]                                            #select observations for testing

###b)
continuous <- c(5, 7, 8, 10, 11, 13, 14, 16)      #Column index for continuous variables
x0 <- d0[, continuous]                            #Select continuous variables for training data
x1 <- d1[, continuous]                            #Select continuous variables for testing data

tele.knn <- k_nn(x0, x1, d0[,18], d1[,18], v = 10)#KNN


classification.table <- table(tele.knn, d1[,18])
err.rate <- (classification.table[1,2] + classification.table[2,1])  / sum(classification.table)
classification.table



###c)
x0 <- d0[, -c(2, 3, 18)]#Extract continuous and integer variables (except columns 2 and 3) for training
x1 <- d1[, -c(2, 3, 18)]#Extract continuous and integer variables (except columns 2 and 3) for testing

tele.knn <- k_nn(x0, x1, d0[, 18], d1[, 18], v = 10)  #KNN


classification.table <- table(tele.knn, d1[,18])
err.rate <- (classification.table[1,2] + classification.table[2,1])  / sum(classification.table)
classification.table

###d)
z0 <- scale.con(x0)                                           #Scale training data 
z1 <- scale.con(x1)                                           #Scale testing data 

tele.knn <- k_nn(z0, z1, d0[,18], d1[,18], v = 10)            #KNN

classification.table <- table(tele.knn, as.factor(d1[,18]))
err.rate <- (classification.table[1,2] + classification.table[2,1])  / sum(classification.table)
classification.table

##Q2

###a)
x <- d[, continuous]            #Extract all the continuous variables in d
c2 <- as.factor(d$Intl_plan)    #Convese Intl_plan to category variable
c3 <- as.factor(d$Vmail_plan)   #Convese Vmail_plan to category variable

###b)
dc <- cbind(c2, c3, x)    #Combine c2, c3 and x to form a matrix dc 
d0 <- dc[inTrain,]        #Extract observations for training 
c0 <- d$Change[inTrain]   #Extract training label
d1 <- dc[-inTrain,]       #Extract observations for testing
c1 <- d$Change[-inTrain]  #Extract testing label

###c)
tele.nb<-e1071::naiveBayes(d0, c0)        #Naive Bayes
pr<-predict(tele.nb,d1)                   #Predict

classification.table <- table(pr,c1)
err.rate <- (classification.table[1,2] + classification.table[2,1])  / sum(classification.table)
classification.table


