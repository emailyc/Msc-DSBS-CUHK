---
title: <center><h1> 2018R2 Data Mining (STAT5104)  Assignment 1</h1></center><br />
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
set.seed(5104)
```

##mdist function

```r
mdist<-function(x) { 
   t<-as.matrix(x)        # transform x to a matrix 
   m<-apply(t,2,mean)     # compute column mean 
   s<-var(t)              # compute sample covariance matrix 
   mahalanobis(t,m,s)     # using built-in mahalanobis function 
} 
```

## Outlier detection

###a) 

```r
d <- read.csv("Telephone.csv", header = TRUE)
d0 <- filter(d, Change == 0)
d1 <- filter(d, Change == 1)
```

### b)

```r
x0 <- select(d0, -c(2,3,18))
x1 <- select(d1, -c(2,3,18))
md0 <- mdist(x0)
md1 <- mdist(x1)
```

### c)

```r
cutoff <- qchisq(0.99,df = ncol(d) - 3) 
dc0 <- filter(d0, md0 < cutoff)
dc1 <- filter(d1, md1 < cutoff)
```

There are 37 outliners in `d0`; there are 8 outliners in `d1`

###d)

```r
dc <- rbind(dc0, dc1)
write.table(dc, "tele.csv", sep = ",")
```
