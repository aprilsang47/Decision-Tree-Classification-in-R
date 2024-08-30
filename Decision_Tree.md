Decision Tree
================
April Sang
24/11/2018

### In this analysis, I am using a data set named salaries.csv that contains data on professors and their salaries.

#### \-

#### You will find several ways of producing decision tree with various interfaces below.

``` r
salary <- read.csv('salaries.csv', header = T, sep = ",")
head(salary)
```

    ##        rank discipline yrs.since.phd yrs.service  sex salary
    ## 1      Prof          B            19          18 Male 139750
    ## 2      Prof          B            20          16 Male 173200
    ## 3  AsstProf          B             4           3 Male  79750
    ## 4      Prof          B            45          39 Male 115000
    ## 5      Prof          B            40          41 Male 141500
    ## 6 AssocProf          B             6           6 Male  97000

#### 1 General Decision Tree

``` r
#install.packages("tree")
library(tree)
salaryt <- tree(salary~., data=salary)
par(mfrow=c(1,1))
plot(salaryt)
text(salaryt, pretty=0,cex = 0.6) # provides more understandable split labelling for the following steps.
```

![](Decision_Tree_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#Perform 20-fold cross-validation using cv.tree.
set.seed(6421)
cv.salaryt <- cv.tree(salaryt, K=20)
#min(cv.salaryt$dev)
plot(cv.salaryt, type="b")
```

![](Decision_Tree_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
cv.salaryt 
```

    ## $size
    ## [1] 8 7 6 4 3 2 1
    ## 
    ## $dev
    ## [1] 222749582678 218837618667 218751273169 220290119067 234412250039
    ## [6] 238295262199 364550608892
    ## 
    ## $k
    ## [1]         -Inf   5614210125   5836069835   7433689756  10918968544
    ## [6]  12019214831 137614097729
    ## 
    ## $method
    ## [1] "deviance"
    ## 
    ## attr(,"class")
    ## [1] "prune"         "tree.sequence"

``` r
#CV suggests 8 terminal nodes.

#It seems like the 6th sized trees result in the lowest deviance. We can then prune the tree.
p.salaryt <- prune.tree(salaryt, best=6)
plot(p.salaryt)
text(p.salaryt,pretty=0,cex = 0.6)
```

![](Decision_Tree_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

    Ex. giving a prediction for an Assistant Professor, in an applied department, who got their PhD in 2012 (6 years ago), has 5 years of service(usually counted as total amount of time as a university professor), and is male.
    
    #testData <- c('AsstProf','B',6,5)
    #testData
    
    Based on the prune tree, the prediction for ('AsstProf','B',6,5) is $87180.

#### \-

#### Setup a training and testing set to predict:

``` r
set.seed(763)
trainindex <- sample(1:nrow(salary), 200)
proftrain <- salary[trainindex, ]
proftest <- salary[-trainindex, ]

#fit the model on the training set
saltrain <- tree(salary~., data=proftrain)
plot(saltrain)
text(saltrain, pretty=0,cex = 0.6)
```

![](Decision_Tree_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
yhat <- predict(saltrain, newdata=proftest[,-6])
MSE = mean((yhat-proftest[,6])^2)
MSE
```

    ## [1] 616355582

``` r
#predicted salary: $80,250
```

#### 2 Plotting Classification Trees with the plot.rpart

``` r
#install.packages("rpart.plot")
library(rpart)                     
library(rattle) 
```

    ## Rattle: A free graphical interface for data science with R.
    ## Version 5.2.0 Copyright (c) 2006-2018 Togaware Pty Ltd.
    ## Type 'rattle()' to shake, rattle, and roll your data.

``` r
library(rpart.plot)

form <- as.formula(salary ~ .)
tree.1 <- rpart(form,data=salary,control=rpart.control(minsplit=20,cp=0))
prp(tree.1)             
```

![](Decision_Tree_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Interatively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj 
```

    ## Warning: ignoring snip=TRUE for quartz_off_screen device

![](Decision_Tree_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
prp(new.tree.1)
```

![](Decision_Tree_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

#### 3 Plotting Classification Trees with the rattle pckages

``` r
tree.2 <- rpart(form,salary)            
prp(tree.2)                                  
```

![](Decision_Tree_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
fancyRpartPlot(tree.2)  
```

![](Decision_Tree_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
#tree.2 is a much more reasonable tree for the segmentationData, results from just accepting the rpart() deaults.This tree is plotted with prp() using the default settings and in the next line, the tree is plotted using the fancyRpartPlot() function from the rattle package.
```

    Reference: 
    https://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
