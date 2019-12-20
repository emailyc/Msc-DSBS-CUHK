---
bibliography: References.bib
title: STAT5104 Data Mining Project
author:  |
  - CHAN Yiu Fung (1155010561)
  - CHUNG Wai Tung (1155118014)
  - LAM Siu Hung (1006201460)
  - LAU Chiu Kit (1155120306)
  - WONG Tsz Wing (1004666311)
  - WONG Yiu Chung (1155017920)
date: <center>12 May, 2019</center>

abstract: |
  This is the abstract.

  It consists of two paragraphs.

output:
  pdf_document:
    fig_caption: true
    number_sections: true
  html_document:
    keep_md: no
    toc: true # table of content true
    toc_depth: 3  # upto three depths of headings (specified by #, ## and ###)
    number_sections: true  ## if you want number sections at each table header
    theme: united  # many options for theme, this one is my favorite.
    highlight: tango  # specifies the syntax highlighting style
    css: my.css   # you can add your custom css, should be in same folder
  word_document: default
--- 

\newpage
\tableofcontents
\newpage





```r
check.packages <- function(pkg)
{
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])];
    if (length(new.pkg)) 
    {
      install.packages(new.pkg, dependencies = TRUE);
      sapply(pkg, require, character.only = TRUE);
    }
}

# packeges required by project
packages<-c("caret",
            "rpart",
            "e1071",
            "klaR",
            "rattle",
            "doParallel",
            "parallel",
            "randomForest",
            "gbm",
            "MLmetrics",
            "dplyr",
            "ggplot2",
            "GGally",
            "lattice");
check.packages(packages);

library(dplyr);
library(caret);
```


```r
set.seed(5104);
```


# Introduction
The research topic, Human Activity Recognition (HAR), is becoming more and more popular among the computing research community.  In the traditional HAR research, researchers mainly focused on predicting which activity was performed at a specific point of time. Meanwhile, latest researchers have shifted the focus on how well the activities have been performed.  In real-life, we can apply the ideas, for example, in sports training. 
In this report, we explored the Weight Lifting Exercises Dataset (Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H., 2013) and attempted to assess if the participants performed the specific weight lifting exercise, Unilateral Dumbbell Biceps Curl (hereafter refers to the exercises), correctly from the data collected via various sensors attached on the body.  The type of mistakes in the exercise can also be identified.
Six male participants aged between 20-28 years were asked to wear a number of body sensors to perform one set of 10 repetitions of the exercise. The outcomes can be grouped into five classes, one corresponding to the specified execution of the exercise, while the other 4 classes corresponding to some common mistakes.  Each sensor generated a set of readings with three numbers. 

# The Data

The data for this project come from [this source](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv). 

The dataset contains 160 variables, which include one target variable classes and 159 readings from the sensors. This dataset is unique in a way that while there are many variables, each are fundamentally the same, i.e. each set of three columns represents a sensor attached on different parts of the body.  Each sensor generates data according to its rotation around a spatial axis, giving spatial data on three dimensions. 

The target variable classes is defined as below: 

<ol type="a">
  <li>Class A: exactly according to the specification (i.e. correctly perform the exercise);</li>
  <li>Class B: throwing the elbows to the front; </li>
  <li>Class C: lifting the dumbbell only halfway;</li>
  <li>Class D: lowering the dumbbell only halfway; and</li>
  <li>Class E: throwing the hips to the front.</li>
</ol>

## Data preparation

For the following reasons:

* Perdictor with well-defined meaning
* Similar scale
* Similar range
* All continous

scaling / standardising may not yield the best result since this may cause distortion. Data are not rescaled or normalised in this report.

### Load data

```r
dataURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv";
data <- read.csv(dataURL, header = TRUE);
```

### Data cleaning 
The data are further processed by:  
* Removing the first seven fields which are just descriptive data  
* Removing near zero variance fields  
* Removing fields with more than 10% missing values


```r
#Remove the first seven columns
data <- data[,-(1:7)];

#Remove NearZeroVariance variables
nzv <- nearZeroVar(data, saveMetrics=TRUE);
data <- data[,nzv$nzv == FALSE];

#Clean variables with mostly NA
dataNA <- apply(data, 2, function(col){sum(is.na(col))/length(col)});
data <- data[,which(dataNA < .1)];
```
53 collumns remains in the dataset post-cleaning. The following table lists the remaining variables 


```r
str(data)
```

```
## 'data.frame':	19622 obs. of  53 variables:
##  $ roll_belt           : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
##  $ pitch_belt          : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
##  $ yaw_belt            : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
##  $ total_accel_belt    : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ gyros_belt_x        : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.02 0.03 ...
##  $ gyros_belt_y        : num  0 0 0 0 0.02 0 0 0 0 0 ...
##  $ gyros_belt_z        : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 -0.02 0 ...
##  $ accel_belt_x        : int  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
##  $ accel_belt_y        : int  4 4 5 3 2 4 3 4 2 4 ...
##  $ accel_belt_z        : int  22 22 23 21 24 21 21 21 24 22 ...
##  $ magnet_belt_x       : int  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
##  $ magnet_belt_y       : int  599 608 600 604 600 603 599 603 602 609 ...
##  $ magnet_belt_z       : int  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
##  $ roll_arm            : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
##  $ pitch_arm           : num  22.5 22.5 22.5 22.1 22.1 22 21.9 21.8 21.7 21.6 ...
##  $ yaw_arm             : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
##  $ total_accel_arm     : int  34 34 34 34 34 34 34 34 34 34 ...
##  $ gyros_arm_x         : num  0 0.02 0.02 0.02 0 0.02 0 0.02 0.02 0.02 ...
##  $ gyros_arm_y         : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 ...
##  $ gyros_arm_z         : num  -0.02 -0.02 -0.02 0.02 0 0 0 0 -0.02 -0.02 ...
##  $ accel_arm_x         : int  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
##  $ accel_arm_y         : int  109 110 110 111 111 111 111 111 109 110 ...
##  $ accel_arm_z         : int  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
##  $ magnet_arm_x        : int  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
##  $ magnet_arm_y        : int  337 337 344 344 337 342 336 338 341 334 ...
##  $ magnet_arm_z        : int  516 513 513 512 506 513 509 510 518 516 ...
##  $ roll_dumbbell       : num  13.1 13.1 12.9 13.4 13.4 ...
##  $ pitch_dumbbell      : num  -70.5 -70.6 -70.3 -70.4 -70.4 ...
##  $ yaw_dumbbell        : num  -84.9 -84.7 -85.1 -84.9 -84.9 ...
##  $ total_accel_dumbbell: int  37 37 37 37 37 37 37 37 37 37 ...
##  $ gyros_dumbbell_x    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ gyros_dumbbell_y    : num  -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 ...
##  $ gyros_dumbbell_z    : num  0 0 0 -0.02 0 0 0 0 0 0 ...
##  $ accel_dumbbell_x    : int  -234 -233 -232 -232 -233 -234 -232 -234 -232 -235 ...
##  $ accel_dumbbell_y    : int  47 47 46 48 48 48 47 46 47 48 ...
##  $ accel_dumbbell_z    : int  -271 -269 -270 -269 -270 -269 -270 -272 -269 -270 ...
##  $ magnet_dumbbell_x   : int  -559 -555 -561 -552 -554 -558 -551 -555 -549 -558 ...
##  $ magnet_dumbbell_y   : int  293 296 298 303 292 294 295 300 292 291 ...
##  $ magnet_dumbbell_z   : num  -65 -64 -63 -60 -68 -66 -70 -74 -65 -69 ...
##  $ roll_forearm        : num  28.4 28.3 28.3 28.1 28 27.9 27.9 27.8 27.7 27.7 ...
##  $ pitch_forearm       : num  -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.8 -63.8 -63.8 ...
##  $ yaw_forearm         : num  -153 -153 -152 -152 -152 -152 -152 -152 -152 -152 ...
##  $ total_accel_forearm : int  36 36 36 36 36 36 36 36 36 36 ...
##  $ gyros_forearm_x     : num  0.03 0.02 0.03 0.02 0.02 0.02 0.02 0.02 0.03 0.02 ...
##  $ gyros_forearm_y     : num  0 0 -0.02 -0.02 0 -0.02 0 -0.02 0 0 ...
##  $ gyros_forearm_z     : num  -0.02 -0.02 0 0 -0.02 -0.03 -0.02 0 -0.02 -0.02 ...
##  $ accel_forearm_x     : int  192 192 196 189 189 193 195 193 193 190 ...
##  $ accel_forearm_y     : int  203 203 204 206 206 203 205 205 204 205 ...
##  $ accel_forearm_z     : int  -215 -216 -213 -214 -214 -215 -215 -213 -214 -215 ...
##  $ magnet_forearm_x    : int  -17 -18 -18 -16 -17 -9 -18 -9 -16 -22 ...
##  $ magnet_forearm_y    : num  654 661 658 658 655 660 659 660 653 656 ...
##  $ magnet_forearm_z    : num  476 473 469 469 473 478 470 474 476 473 ...
##  $ classe              : Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
```

### Slicing into training and testing sets
The training data set is sliced into 80% for training and 20% for testing.

```r
inTrain <- createDataPartition(y=data$classe, p=0.80, list=FALSE); #Data slicing
train <- data[inTrain,];
test <- data[-inTrain,];
```

### Overview of cleaned dataset

```r
dim(train);
```

```
## [1] 15699    53
```


## Principal Component Analysis

Principal Component Analysis (PCA) is a dimention reduction technique. A reduced dataset allows faster processing and smaller storage. In the context of data mining, PCA reduce the number of variables to be used in a model by focusing only on the components accounting for the majority of the variance. Highly correlated variables are also removed as a result of PCA.

In this report, PCA is perofrmed on the original dataset to reduce the number of dimention while retaining 99% of the information.

```r
prComp <- caret::preProcess(train[,-length(train)], method = "pca", thresh = 0.99);
trainPC <- predict(prComp, train[,1:ncol(train)-1]);
trainPC$classe <- train$classe;
testPC <- predict(prComp, test[,1:ncol(test)-1]);
testPC$classe <- test$classe;
```
Here, PCA is able to reduce the dimention of the datasets from 52 to 38 while retaining 99% of the information. This reduces model complexity and improves scalibility. 

As a side note, PCA is ususlly performed on scaled  or normalised dataset to prevent the resulting principle sub-space from being dominated by variables with large scales. As mentioned above, because the variables in the dataset are of similar nature, scaling or normalised provdes little added benifits. Hence such procedures are not used. 


![](Project_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

# Methods

## Learning Modles
Seven learning methods are adopted in this report. Namely: 

1. Decision Tree;
1. K-Nearest Neighbor;
1. Multinomial Logistic Regression;
1. Naive Bayes;
1. Neuro Network;
1. Random Forest;
1. Tree Bagging.  

The methods can be classified as eager learner (Decision Tree, Tree Bagging, Random Forest, and Neuro Network) and lazy learner (K-Nearest Neighbor and Naive Bayes). The library `Caret` is used to generate training models .


## Resampling: Cross Validation

Cross Validation is performed on each training methods to infer model performance.

### Choosing between LOOCV and K-Fold

Leave-One-Out Cross-Validation (LOOCV) and K-Fold are common resampling methods for accessing model performance. While LOOCV estimates test error with lowest bias (averaging validation errors across n models), K-Fold CV is much less computationally intensive. Yet there is another advantage to using K-fold CV. This has to do with a bias-variance trade-off.

Estimates produced by LOOCV is plagued by high variance compared to that produced by K-fold CV. This is because each statistics (accuracy, AUC, F1, log loss etc) in LOOCV are produced by models trained on virtually identical datasets. The final averaged statistic is an average of statistics from n models which are highly positively correlated. On the other hand, K-fold CV outputs K (which is usually much less than n) statistics which are less correlated as there are less overlap among models. The average of strongly correlated quantities has higher variance than the average of weakly correlated quantities; hence the estimated statistics from LOOCV tends to have higher variance that that from K-fold. 

The dataset in the report consists of relatively large number of observations (38 rows). Hence a 10 fold cross-validation is performed.

### Performance Measures for Multi-Class Problems

The following are handful of many viable model performance measures for choosing the best model out of the many models `caret::train` create using different parameters. FOr example, `caret::train` tries different $k$ in KNN. 

* Accuracy and Kappa
* Area Under ROC Curve
* F1
* Logarithmic Loss


```r
tc <- caret::trainControl(method = "cv", #resampling method = cross validation
                          number = 10,   #10-fold validation
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          verboseIter=FALSE,
                          allowParallel=TRUE);

metric <- "logLoss";
```


```r
#Parallel Processing, leaves you one core for other stuff.
#Plz try not to do CPU intensive tasks while modelling.
cl <- parallel::makeCluster(parallel::detectCores()- 1);
doParallel::registerDoParallel(cl);
```

## Lazy Learners

Lazy learners simply store the training data without performing further munging, until a test dataset is presented. During model training, Lazy Learners require sigificantly less computational operation as there is no new algorithm being developed; for the same reason, Lazy Learners are slow when used for prediction  because new data are used to compute predictions instead of relying on a pre-calculated algorithm.

Naive Bayes and K-Nearest-Neighbour (KNN) are used in this section. Both lazy learners are expected to perform well on large datasets like the one used in this report. KNN relies heavily on Euclidean distance (L2 norm) between observations and is more appropriate on scaled or normalised data. Hence, this model is expected to perform less well than other data mining models used in this report.  

```r
if (file.exists("nb.rds"))
{
        nb <- readRDS(file = "nb.rds");
        nbTime <- readRDS(file = "nbTime.rds");
} else
{
        nbTime <- system.time(
          nb <- caret::train(classe ~ .,        #naive bayes
                             data = trainPC,
                             method = "nb",
                             metric = metric, 
                             trControl= tc)
          );       
        saveRDS(nb, file = "nb.rds");
        saveRDS(nbTime, file = "nbTime.rds");
}

if (file.exists("knn.rds"))
{
        knn <- readRDS(file = "knn.rds"); 
        knnTime <- readRDS(file = "knnTime.rds"); 
} else
{
        knnTime <- system.time(
          knn <- caret::train(classe ~ .,       #knn
                              data = trainPC,
                              method = "knn",
                              metric = metric,
                              trControl= tc)
          );     
        saveRDS(knn, file = "knn.rds");
        saveRDS(knnTime, file = "knnTime.rds");
}
```

## Multinomial logistic regression


```r
if (file.exists("multinom.rds"))
{
        multinom <- readRDS(file = "multinom.rds");
        multinomTime <- readRDS(file = "multinomTime.rds");
} else
{
        multinomTime <- system.time(
          multinom <- caret::train(classe ~ .,          #Multinomial Logistic Regression
                                   data = trainPC,
                                   method = "multinom",
                                   metric = metric,
                                   trControl= tc)
          );
        saveRDS(multinom, file = "multinom.rds");
        saveRDS(multinomTime, file = "multinomTime.rds");
}
```

## Tree based models

Tree-based methods tend to perform well on unprocessed data (i.e. without normalizing, centering, scaling features).

Decision Trees often produce predictions with low bias but high variance. The more complex the tree, the more apparent this becomes (overfitting). Methods have been proposed to overcome this issue. This includes Bootstrap Aggregation (Bagging), as well as Random Forest. 

The idea behind tree bagging is to create many trees, each trained from bootstrapped data from the original dataset. Each tree is slightly different from each other because they are trained with mildly different datasets. Classification decision is then performed by popular vote across all trees. This method reduces variance by averaging decisions among many trees. There is a caveat though: tress turn out to be very similar to each other when there exists a (or few) extremely strong predictor, following by some moderately strong predictors.  Each tree will have similar node splitting because of these strong predictors, which renders each tree to have practicality the same decision rules. Unfortunately, as mentioned above, the variance of the averages of highly correlated quantities is also high. This means tree bagging provides little improvements in terms of variance reduction.

Random Forest enhances tree bagging through a tweak: at each node split, the algorithm randomly picks a subset of size $m$ predictors out of all $p$, then choose the best predictor for this node split as normally seen in decision trees. This way, each tree is more likely to be different from each other. And hence their averages are less varying. The choice of $m$ is often the square root of $p$ but other method of choosing $m$ also exists.


```r
if (file.exists("ctree.rds"))
{
        ctree <- readRDS(file = "ctree.rds");
        ctreeTime <- readRDS(file = "ctreeTime.rds");
} else
{
        ctreeTime <- system.time(
          ctree <- caret::train(classe ~ .,       #decision tree
                                data = trainPC,
                                method = "rpart",
                                metric = metric,
                                trControl= tc)
          );
        saveRDS(ctree, file = "ctree.rds");
        saveRDS(ctreeTime, file = "ctreeTime.rds");
}

if (file.exists("treebag.rds"))
{
        treebag <- readRDS(file = "treebag.rds");
        treebagTime <- readRDS(file = "treebagTime.rds");
} else
{
        treebagGrid <- expand.grid(.mtry = ncol(trainPC) - 1);
        treebagTime <- system.time(
          treebag <- caret::train(classe ~ .,      #Tree bagging
                                  data = trainPC,
                                  method = "rf",
                                  metric = metric,
                                  tuneGrid = treebagGrid,
                                  trControl= tc)
          );
        saveRDS(treebag, file = "treebag.rds");
        saveRDS(treebagTime, file = "treebagTime.rds");
}

if (file.exists("rf.rds"))
{
        rf <- readRDS(file = "rf.rds");
        rfTime <- readRDS(file = "rfTime.rds");
} else
{
        rfGrid <- expand.grid(.mtry = sqrt(ncol(trainPC) - 1));
        rfTime <- system.time(
          rf <- caret::train(classe ~ .,        #Random Forest
                             data = trainPC,
                             method = "rf",
                             metric = metric,
                             tuneGrid = rfGrid,
                             trControl= tc)
          );
        saveRDS(rf, file = "rf.rds");
        saveRDS(rfTime, file = "rfTime.rds");
}
```
Note that in the code above, both models `treebag` and `rf` employ the training method rf. This is because tree bagging is in fact a special case of Random Forest where $m$ = $p$. 

## Neuro-Net

R doesn't provide an easy way to model multilayer perceptron (Neuro Network). Hence a single-layer perceptron is modeled below. Neuro Networks tend to be scale invariant (just like tree based models): rescaling the input vector is equivalent to changing the weights and biases of the network, result in the exact same outputs as before.

```r
if (file.exists("NN.rds"))
{
        NN <- readRDS(file = "NN.rds");
        NNTime <- readRDS(file = "NNTime.rds");
} else
{
        nnetGrid <-  expand.grid(
                size = seq(from = 1, to = 10, by = 1),
                decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7));
        NNTime <- system.time(
          NN <- caret::train(classe ~ .,
                             data = trainPC,
                             method = "nnet",
                             metric = metric,
                             tuneGrid = nnetGrid,
                             trControl= tc, verbose=FALSE));
        saveRDS(NN, file = "NN.rds");
        saveRDS(NNTime, file = "NNTime.rds");
}
```
The parameter `size` specifies the number of units in the hidden layer. Sizes ranging from 1 to 10 are experimented for best results.
The parameter `decay` specifies the regularisation of the number of nodes: model with high node counts are more heavily penalised


```r
#parallel::stopCluster(cl);
```

## Compare Models

```r
results <- caret::resamples(list(NaiveBayes = nb,
                          KNearestNeighbor = knn,
                          MultinomialLogit = multinom,
                          DecisionTree = ctree,
                          TreeBagging = treebag,
                          RandomForest = rf,
                          NeuroNetwork = NN));

results$metrics
```

```
##  [1] "Accuracy"               "AUC"                   
##  [3] "Kappa"                  "logLoss"               
##  [5] "Mean_Balanced_Accuracy" "Mean_Detection_Rate"   
##  [7] "Mean_F1"                "Mean_Neg_Pred_Value"   
##  [9] "Mean_Pos_Pred_Value"    "Mean_Precision"        
## [11] "Mean_Recall"            "Mean_Sensitivity"      
## [13] "Mean_Specificity"       "prAUC"
```
There are a total of 14 metrics for comparing models.

# Findings

## Comparing models

Averages of LogLoss, Accuracy, F1 and AUC are used to assess the performances of the models.


```r
summaryStat <- summary(results)$statistics;

scales <- list(x=list(relation="free"), y=list(relation="free"));
metrics <- c("Accuracy", "AUC", "logLoss", "Mean_F1");

lattice::dotplot(results, scales=scales,  metric=metrics, main="Model Performances");
```

![](Project_files/figure-latex/result graphs-1.pdf)<!-- --> 

```r
times <- c(nbTime[[3]], knnTime[[3]],
           multinomTime[[3]],
           ctreeTime[[3]], treebagTime[[3]], rfTime[[3]],
           NNTime[[3]]) %>% round(., 3)
models <- c("Naive Bayes", "KNN",
            "Multi Nomial",
            "Decision Tree", "Tree Bagging", "Random Forest",
            "Neuro-Net")
elapsedTime <- data.frame(models = models, times = times) %>% arrange(times);


timePlot <- ggplot2::ggplot(data=elapsedTime, aes(x=reorder(models, times), y=times)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ggtitle("Training Time") +
  ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
  ggplot2::xlab("Models") + 
  ggplot2::ylab("Time (seconds)") + 
  ggplot2::geom_text(ggplot2::aes(label=times), vjust=-.5);
timePlot
```

![](Project_files/figure-latex/result graphs-2.pdf)<!-- --> 

Among the seven models, Random Forest, Tree Bagging and KNN outperform the other four models. Surprisingly, KNN appears to outperform all other models holistically: lowest log Loss value at 0.1274666, highest Mean F1, Accuracy, and AUC at 0.9618464, 0.9640746, 0.9979557 respectively. In addition, KNN has the second lowest learning time (which is less surprising given its lazy learning nature) at `knnTime[[3]]` seconds (wall clock), beaten by Decision Tree only.

The training data used in the report remain at their original scale. KNN is supposed to suffer from  neighbors being aligned along the direction of the axis with the smaller range. This somewhat reaffirms the notion of the dataset having variables with similar scales and ranges. 

KNN performs well at various metrics, as well as having a low training time. Thus, KNN is chosen as the final model to be tested.


#Model Performance

```r
confusion <- confusionMatrix(predict(knn, testPC), testPC$classe);
confusion
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1104   15    0    3    0
##          B    4  727    7    0    4
##          C    4   16  665   23    3
##          D    3    1    7  614    1
##          E    1    0    5    3  713
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9745          
##                  95% CI : (0.9691, 0.9792)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9678          
##                                           
##  Mcnemar's Test P-Value : 0.0008817       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9892   0.9578   0.9722   0.9549   0.9889
## Specificity            0.9936   0.9953   0.9858   0.9963   0.9972
## Pos Pred Value         0.9840   0.9798   0.9353   0.9808   0.9875
## Neg Pred Value         0.9957   0.9899   0.9941   0.9912   0.9975
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2814   0.1853   0.1695   0.1565   0.1817
## Detection Prevalence   0.2860   0.1891   0.1812   0.1596   0.1840
## Balanced Accuracy      0.9914   0.9765   0.9790   0.9756   0.9930
```
`KNN` is able to predict future data with 0.9745093 accuracy and 0.9677523 Kappa. 


p.s. models were builts on machine with the following specs

* Processor: Intel Xeon(R) CPU E5-2603 v4 @ 1.70GHz x 12
* Memory: 31.3 GiB
* Graphics NVS 315/PCIe/SSE2
* OS type: 64-bit
* OS: ubuntu 16.04 LTS

\newpage

# References

```r
papaja::r_refs(file = "r-references.bib")
```
