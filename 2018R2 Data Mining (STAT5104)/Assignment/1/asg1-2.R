library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
set.seed(17920)

dc <- read.csv("tele.csv", header = TRUE, sep = ",")            #read data
inTrain <- createDataPartition(dc$Change, p = .9) %>% unlist(.) #create index for train / test partition
d0 <- dc[inTrain,]                                              #select observations for training 
d1 <- dc[-inTrain,]                                             #select observations for testing

control <- rpart.control(maxdepth = 3)
ctree <- rpart(data = d0, formula = Change ~ ., control = control, method = 'class')

rpart.plot(ctree)             #print plot
print(ctree)
asRules(ctree, compact=FALSE) #print rules

oneSum <- sum(d0$Change)      #calculate total number of observaation where Change == 1
#in the training set
zeroSum <- nrow(d0) - oneSum  #calculate total number of observaation where Change == 0
#in the training set

test <- predict(ctree)                              #predict using training data 
cl_test <- max.col(test) - 1                        #rename columns from  1, 2 to 0, 1
test_table <- table(cl_test, d0$Change)             #put data into classification table 
test_table

validation <- predict(ctree, newdata = d1)          #predict using testing data 
cl_validation <- max.col(validation) - 1            #rename columns from  1, 2 to 0, 1
validation_table <- table(cl_validation, d1$Change) #put data into classification table 
validation_table







