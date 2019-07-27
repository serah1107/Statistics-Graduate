# Chapter 9 Lab: Support Vector Machines
# Support Vector Classifier
set.seed(1)
x=matrix(rnorm(20*2), ncol=2); y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1 ; plot(x, col=(3-y))
dat=data.frame(x=x, y=as.factor(y))
library(e1071)
svmfit=svm(y~., data=dat, kernel="linear", cost=10,scale=FALSE);plot(svmfit, dat)
svmfit$index;summary(svmfit)
svmfit=svm(y~., data=dat, kernel="linear", cost=0.1,scale=FALSE);plot(svmfit, dat)
svmfit$index
tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)));summary(tune.out)
bestmod=tune.out$best.model;summary(bestmod)
xtest=matrix(rnorm(20*2), ncol=2);ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred=predict(bestmod,testdat);table(predict=ypred, truth=testdat$y)
svmfit=svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
ypred=predict(svmfit,testdat);table(predict=ypred, truth=testdat$y)
x[y==1,]=x[y==1,]+0.5;plot(x, col=(y+5)/2, pch=19)
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=1e5);summary(svmfit);plot(svmfit, dat)
svmfit=svm(y~., data=dat, kernel="linear", cost=1);summary(svmfit);plot(svmfit,dat)
# Support Vector Machine
set.seed(1)
x=matrix(rnorm(200*2), ncol=2 );x[1:100,]=x[1:100,]+2;x[101:150,]=x[101:150,]-2;y=c(rep(1,150),rep(2,50));dat=data.frame(x=x,y=as.factor(y));plot(x, col=y)
train=sample(200,100);svmfit=svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)


plot(svmfit, dat[train,])
summary(svmfit)
svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5);plot(svmfit,dat[train,])
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)));summary(tune.out)
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newx=dat[-train,]))
# ROC Curves
library(ROCR)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}
svmfit.opt=svm(y~., data=dat[train,], kernel="radial",gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")
svmfit.flex=svm(y~., data=dat[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")
# SVM with Multiple Classes
set.seed(1);x=rbind(x, matrix(rnorm(50*2), ncol=2));y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2;dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1));plot(x,col=(y+1))
svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1);plot(svmfit, dat)
# Application to Gene Expression Data
library(ISLR)
names(Khan);dat=data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out=svm(y~., data=dat, kernel="linear",cost=10);summary(out)
table(out$fitted, dat$y);dat.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te);table(pred.te, dat.te$y)
##ex 4,5,8
###ex4---------------------------------------------------------------------------
####linear
set.seed(1)
transl <- 3; X <- matrix(rnorm(100 * 2), ncol = 2)
X[1:30, ] <- X[1:30, ] + transl; X[31:60, ] <- X[31:60, ] - transl
y <- c(rep(0, 60), rep(1, 40));dat <- data.frame(x = X, y = as.factor(y))
plot(X, col = y + 1)
###split the dataset 
train <- sample(100, 80);dat.train <- dat[train, ];dat.test <- dat[-train, ]
###fit the svm model 
library(e1071)
svm.lin <- svm(y ~ ., data = dat.train, kernel = 'linear', scale = FALSE);summary(svm.lin)
plot(svm.lin, data = dat.train);table(predict = svm.lin$fitted, truth = dat.train$y)
####polynomial
svm.poly <- svm(y ~ ., data = dat.train, kernel = 'polynomial', scale = FALSE);plot(svm.poly, data = dat.train)
table(predict = svm.poly$fitted, truth = dat.train$y)
####radial
svm.rad <- svm(y ~ ., data = dat.train, kernel = 'radial', scale = FALSE);plot(svm.rad, data = dat.train)
table(predict = svm.rad$fitted, truth = dat.train$y)
lin.pred <- predict(svm.lin, dat.test);table(predict = lin.pred, truth = dat.test$y)
poly.pred <- predict(svm.poly, dat.test);table(predict = poly.pred, truth = dat.test$y)
rad.pred <- predict(svm.rad, dat.test);table(predict = rad.pred, truth = dat.test$y)
###ex5---------------------------------------------------------------------------
##a)-----------------------------------------------------------------------------
library(knitr);knitr::opts_chunk$set(echo = TRUE);library(tidyverse);library(ggthemes);library(caret);library(e1071)
theme_set(theme_tufte(base_size = 14))
df <- data.frame(replicate(2, rnorm(500)));df <- as.tibble(df)
class_func <- function(x, y) {
  x^2 + y < 1
}
df <- df %>%  rename(Var1 = X1, Var2 = X2) %>%  mutate(Class = ifelse(class_func(Var1, Var2),'Class A', 'Class B'),Class = factor(Class))
##b)-----------------------------------------------------------------------------
inTrain <- sample(nrow(df), nrow(df)*0.6, replace = FALSE)
training <- df[inTrain,];testing <- df[-inTrain,]
ggplot(df, aes(Var1, Var2, col = Class)) +
  geom_point(size = 2)
##c)-----------------------------------------------------------------------------
logreg_fit <- glm(Class ~ ., data = training, family = 'binomial');summary(logreg_fit)
##d)-----------------------------------------------------------------------------
pred <- predict(logreg_fit, testing, type = 'response');pred <- ifelse(pred >= 0.5, 'Class B', 'Class A')
mean(pred == testing$Class)
ggplot(data.frame(testing,pred), aes(Var1, Var2, col = pred)) +
  geom_point(size = 2)
##e)-----------------------------------------------------------------------------
logreg_fit <- glm(Class ~ poly(Var1, 2) + Var2, data = training, family = 'binomial');summary(logreg_fit)
##f)-----------------------------------------------------------------------------
pred <- predict(logreg_fit, testing, type = 'response');pred <- ifelse(pred >= 0.5, 'Class B', 'Class A')
mean(pred == testing$Class)
data_frame(pred = pred, class = testing$Class) %>%
  ggplot(aes(pred, class,col=pred)) +
  geom_jitter()
##g)-----------------------------------------------------------------------------
svm_fit <- svm(Class ~ ., data = training,kernel = 'linear',scale = FALSE);plot(svm_fit, testing)
mean(predict(svm_fit, testing) == testing$Class)
##h)-----------------------------------------------------------------------------
svm_poly <- svm(Class ~ ., data = training,kernel = 'polynomial', degree = 2,scale = FALSE);plot(svm_poly, testing)
mean(predict(svm_poly, testing) == testing$Class)
svm_radial <- svm(Class ~ ., data = training,kernel = 'radial',scale = FALSE);plot(svm_radial, testing)
mean(predict(svm_radial, testing) == testing$Class)
###ex8---------------------------------------------------------------------------
##a)-----------------------------------------------------------------------------
require(ISLR); require(tidyverse); require(ggthemes);require(caret); require(e1071)
set.seed(1);data('OJ')
inTrain <- sample(nrow(OJ), 800, replace = FALSE)
training <- OJ[inTrain,];testing <- OJ[-inTrain,]
##b)-----------------------------------------------------------------------------
svm_linear <- svm(Purchase ~ ., data = training,kernel = 'linear',cost = 0.01);summary(svm_linear)
##c)-----------------------------------------------------------------------------
mean(predict(svm_linear, training) != training$Purchase);mean(predict(svm_linear, testing) != testing$Purchase)
##d)-----------------------------------------------------------------------------
svm.tune = tune(svm, Purchase ~ ., data = training, kernel = "linear", ranges = list(cost = 10^seq(-2, 1, by = 0.25)));summary(svm.tune)
##e)-----------------------------------------------------------------------------
svm_linear <- svm(Purchase ~ ., kernel = "linear", data = training, cost = svm.tune$best.parameter$cost)
mean(predict(svm_linear, training) != training$Purchase);mean(predict(svm_linear, testing) != testing$Purchase)
##f)-----------------------------------------------------------------------------
svm_radial <- svm(Purchase ~ ., kernel = "radial", data = training);summary(svm_radial)
mean(predict(svm_radial, training) != training$Purchase);mean(predict(svm_radial, testing) != testing$Purchase)
svm.tune2 = tune(svm, Purchase ~ ., data = training, kernel = "radial", ranges = list(cost = 10^seq(-2, 1, by = 0.25)));summary(svm.tune2)
svm_radial <- svm(Purchase ~ ., kernel = "radial", data = training, cost = svm.tune2$best.parameter$cost)
mean(predict(svm_radial, training) != training$Purchase);mean(predict(svm_radial, testing) != testing$Purchase)
##g)-----------------------------------------------------------------------------
svm_poly <- svm(Purchase ~ ., kernel = "polynomial", data = training,degree=2);summary(svm_poly)
mean(predict(svm_poly, training) != training$Purchase);mean(predict(svm_poly, testing) != testing$Purchase)
svm.tune3 = tune(svm, Purchase ~ ., data = training, kernel = "polynomial", degree=2, ranges = list(cost = 10^seq(-2, 1, by = 0.25)));summary(svm.tune3)
svm_poly <- svm(Purchase ~ ., kernel = "polynomial", data = training, cost = svm.tune3$best.parameter$cost)
mean(predict(svm_poly, training) != training$Purchase);mean(predict(svm_poly, testing) != testing$Purchase)

