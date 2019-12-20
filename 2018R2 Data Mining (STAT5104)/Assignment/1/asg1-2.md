---
title: <center><h1> 2018R2 Data Mining (STAT5104)  Assignment 1 Q2</h1></center><br />
author: <center>Yiu Chung WONG 1155017920</center>
output:
  html_document:
    keep_md: yes
    code_folding:
  pdf_document: default
  word_document: default
--- 




```r
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
set.seed(17920)
```

###a)

```r
dc <- read.csv("tele.csv", header = TRUE, sep = ",")
inTrain <- createDataPartition(dc$Change, p = .9) %>% unlist(.)
d0 <- dc[inTrain,]
d1 <- dc[-inTrain,]
```

###b)

```r
control <- rpart.control(maxdepth = 3)
ctree <- rpart(data = d0, formula = Change ~ ., control = control, method = 'class')
```

###c)

```r
rpart.plot(ctree)
```

![](asg1-2_files/figure-html/print-1.png)<!-- -->

```r
print(ctree)
```

```
## n= 2960 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 2960 418 0 (0.85878378 0.14121622)  
##    2) Day_Mins< 264.45 2781 308 0 (0.88924847 0.11075153)  
##      4) CustServ_Calls< 3.5 2563 198 0 (0.92274678 0.07725322) *
##      5) CustServ_Calls>=3.5 218 108 1 (0.49541284 0.50458716)  
##       10) Day_Mins>=160.2 130  32 0 (0.75384615 0.24615385) *
##       11) Day_Mins< 160.2 88  10 1 (0.11363636 0.88636364) *
##    3) Day_Mins>=264.45 179  69 1 (0.38547486 0.61452514)  
##      6) Vmail_plan=yes 44   6 0 (0.86363636 0.13636364) *
##      7) Vmail_plan=no 135  31 1 (0.22962963 0.77037037)  
##       14) Eve_Mins< 187.75 47  21 0 (0.55319149 0.44680851) *
##       15) Eve_Mins>=187.75 88   5 1 (0.05681818 0.94318182) *
```

```r
asRules(ctree, compact=FALSE)
```

```
## 
##  Rule number: 15 [Change=1 cover=88 (3%) prob=0.94]
##    Day_Mins>=264.4
##    Vmail_plan=no
##    Eve_Mins>=187.8
## 
##  Rule number: 11 [Change=1 cover=88 (3%) prob=0.89]
##    Day_Mins< 264.4
##    CustServ_Calls>=3.5
##    Day_Mins< 160.2
## 
##  Rule number: 14 [Change=0 cover=47 (2%) prob=0.45]
##    Day_Mins>=264.4
##    Vmail_plan=no
##    Eve_Mins< 187.8
## 
##  Rule number: 10 [Change=0 cover=130 (4%) prob=0.25]
##    Day_Mins< 264.4
##    CustServ_Calls>=3.5
##    Day_Mins>=160.2
## 
##  Rule number: 6 [Change=0 cover=44 (1%) prob=0.14]
##    Day_Mins>=264.4
##    Vmail_plan=yes
## 
##  Rule number: 4 [Change=0 cover=2563 (87%) prob=0.08]
##    Day_Mins< 264.4
##    CustServ_Calls< 3.5
```

```r
oneSum <- sum(d0$Change)
zeroSum <- nrow(d0) - oneSum
```

Rule Number 4: Day_Mins< 264.45 and CustServ_Calls< 3.5 then Change = 0<br />
Support = 2563 / 3288 = 0.7795012, Confidence = 1 - (198 / 2563) = 0.9227468, Capture = (2563 - 198) / 2542 = 0.9303698

Rule Number 10: Day_Mins< 264.45 and CustServ_Calls>= 3.5 and Day_Mins>=160.2 then Change = 0<br />
Support = 130 / 3288 = 0.0395377, Confidence = 1 - (32 / 130) = 0.7538462, Capture = (130 - 32) / 2542 = 0.0385523

Rule Number 11: Day_Mins< 264.45 and CustServ_Calls>= 3.5 and Day_Mins< 160.2 then Change = 1<br />
Support = 88 / 3288 = 0.026764, Confidence = 1 - (10 / 88) = 0.8863636, Capture = (88 - 10) / 418 = 0.2344498

Rule Number 6: Day_Mins>=264.45 and Vmail_plan=yes then Change = 0<br />
Support = 44 / 3288 = 0.013382, Confidence = 1 - (6 / 44) = 0.8636364, Capture = (44 - 6) / 2542 = 0.0149489

Rule Number 14: Day_Mins>=264.45 and Vmail_plan=yes and then Eve_Mins<187.75 Change = 0<br />
Support = 47 / 3288 = 0.0142944, Confidence = 1 - (21 / 47) = 0.5531915, Capture = (47 - 21) / 2542 = 0.0102282

Rule Number 15: Day_Mins>=264.45 and Vmail_plan=yes and then Eve_Mins>=187.75 Change = 1<br />
Support = 88 / 3288 = 0.026764, Confidence = 1 - (5 / 88) = 0.9431818, Capture = (88 - 5) / 2542 = 0.1985646

###d)

```r
test <- predict(ctree)
cl_test <- max.col(test) - 1
test_table <- table(cl_test, d0$Change)
test_table
```

```
##        
## cl_test    0    1
##       0 2527  257
##       1   15  161
```

```r
validation <- predict(ctree, newdata = d1)
cl_validation <- max.col(validation) - 1
validation_table <- table(cl_validation, d1$Change)
validation_table
```

```
##              
## cl_validation   0   1
##             0 270  37
##             1   1  20
```
d0 error rate: (15 + 257) / 2960  = 0.0918919

d1 error rate: (1 + 37) / 328  = 0.1158537
