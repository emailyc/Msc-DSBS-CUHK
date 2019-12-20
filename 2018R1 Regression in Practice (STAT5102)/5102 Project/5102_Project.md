---
title: <center> <h1>"5102 Project"</h1> </center>
author: "Yiu Chung Wong"
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---

####In this study, we investigate the dataset obtained from The World Bank: World Development Indicators. A Generalised Linear Model is used to perform a prediction analysis on the response variable: Mortality rate, using ???(number) predictor variables.


```
## Loading required package: lattice
```

```
## 
## Attaching package: 'mice'
```

```
## The following objects are masked from 'package:base':
## 
##     cbind, rbind
```

## Read Data


# Preliminary Data Manipulation
---

First we remove a few variables:

*  Year
*  YearCode
*  Country Name
*  Country Code

This is because they contribute no added value to our subsequent analysis.

The variable "Age dependency ratio (% of working-age population)" includes people who are below 15 or above than 64. Mean while, the variable "Age dependency ratio, young (% of working-age population)" only includes people below 15. To seperate these two, we will subtract the seond from the first.
The original "Age dependency ratio (% of working-age population)" is renamed to "Age dependency ratio (% of working-age population)".




##Missing Data

The data set contains some obviously problematic data. For instance, some variables contains more then 90% missing data. Here, we remove variables with more than 5% missing data, and remove cases with more than 15% missing points.



## Impute Data with Multiple Imputation by Chained Equations (MICE)

Dessription

```
## Warning: Number of logged events: 502
```

The data is now ready for analysis.

# Exploratory Data Analysis
---

##Distributions of **Mortality Rate Under 5, Per 1000/Births**
![](5102_Project_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The response variable for this project is the quantitative variable, **Mortality Rate Under 5, Per 1000/Births**; let's call it `U5.Mortality` from here onward. 
Exploring the relstionship between `U5.Mortality` and each of the predictor variables would very much be benifitil. However, there are still 23 variables remaining in the dataset, even after deletion. Hence we will focus on the predictor variables instead.







# Exploring correlations with OLS regression



## Normality Test


```
## Warning in ks.test(resid(OLS), "pnorm", 0, 1): ties should not be present
## for the Kolmogorov-Smirnov test
```



# Multiple Regression



