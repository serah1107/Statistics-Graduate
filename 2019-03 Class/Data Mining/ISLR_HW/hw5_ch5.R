# Chaper 5 Lab: Cross-Validation and the Bootstrap
# The Validation Set Approach
library(ISLR);set.seed(1);train=sample(392,196)
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
set.seed(2);train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# Leave-One-Out Cross-Validation
glm.fit=glm(mpg~horsepower,data=Auto);coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto);coef(lm.fit)
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit);cv.err$delta
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
};cv.error
# k-Fold Cross-Validation
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
};cv.error.10
# The Bootstrap
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio,1:100)
set.seed(1);alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio,alpha.fn,R=1000)
# Estimating the Accuracy of a Linear Regression Model
boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
set.seed(1);boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1);boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
#exercises 2,5,7,9
##2-----------------------------------------------------------------------
###g)---------------------------------------------------------------------
n = 1:100000
plot(n, 1 - (1 - 1/n)^n, typ = "l", log = "x");abline(h = 1 - exp(-1), lty = "dotted")
###h)---------------------------------------------------------------------
n = 10000;store = rep(NA, n)
for (i in 1:n) {
  store[i] = sum(sample(1:100, rep = T) == 4) > 0
}
mean(store)
##5----------------------------------------------------------------------
###a)---------------------------------------------------------------------
library(ISLR)
set.seed(1)
glm.fit = glm(default ~ income + balance, data = Default, family = "binomial");summary(glm.fit)
###b)---------------------------------------------------------------------
#i:
train = sample(dim(Default)[1], dim(Default)[1] / 2)
#ii:
glm.fit = glm(default ~ income + balance, data = Default[train,], family = "binomial");summary(glm.fit)
#iii:
glm.probs = predict(glm.fit, newdata = Default[-train, ], type="response")
glm.pred=rep("No",5000);glm.pred[glm.probs>0.5] = "Yes"
#iv:
mean(glm.pred != Default[-train, ]$default)
###c)---------------------------------------------------------------------
simul = function() {
  train = sample(dim(Default)[1], dim(Default)[1] / 2)
  glm.fit = glm(default ~ income + balance, data = Default[train,], family = "binomial")
  summary(glm.fit)
  glm.probs = predict(glm.fit, newdata = Default[-train, ], type="response")
  glm.pred=rep("No",5000)
  glm.pred[glm.probs>0.5] = "Yes"
  return(mean(glm.pred != Default[-train, ]$default))
}
simul() ; simul() ; simul();
###d)---------------------------------------------------------------------
train <- sample(dim(Default)[1], dim(Default)[1] / 2)
glm.fit <- glm(default ~ income + balance + student, data = Default[train,], family = "binomial")
pred.glm <- rep("No", length(probs))
probs <- predict(glm.fit, newdata = Default[-train, ], type = "response")
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)
##7----------------------------------------------------------------------
###a)---------------------------------------------------------------------
glm.fit = glm(Direction ~ Lag1 + Lag2, data= Weekly, family = binomial);summary(glm.fit)
###b)---------------------------------------------------------------------
glm.fit = glm(Direction ~ Lag1 + Lag2, data= Weekly[-1,], family = binomial);summary(glm.fit)
###c)---------------------------------------------------------------------
glm.probs = predict(glm.fit, newdata = Weekly[1,], type = "response")
glm.pred=ifelse(glm.probs>0.5,"Up","Down");glm.pred
Weekly[1,]$Direction
###d)---------------------------------------------------------------------
error <- rep(0, dim(Weekly)[1])
for (i in 1:dim(Weekly)[1]) {
  glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ],  family = "binomial")
  pred.up <- predict.glm(glm.fit, Weekly[i, ], type = "response") > 0.5
  true.up <- (Weekly[i, ]$Direction == "Up")
  if (pred.up != true.up)
    error[i] <- 1
}
error
###e)---------------------------------------------------------------------
mean(error)
##9----------------------------------------------------------------------
library(MASS);set.seed(1);attach(Boston)
###a)---------------------------------------------------------------------
mu_hat=mean(medv);mu_hat
###b)---------------------------------------------------------------------
se_hat = sd(medv) /sqrt(length(medv));se_hat
###c)---------------------------------------------------------------------
boot.ftn=function(data, index) return(mean(data[index]))
library(boot)
bstrap=boot(medv, boot.ftn, 1000);bstrap
###d)---------------------------------------------------------------------
CI=c(bstrap$t0 - 2 * 0.4119, bstrap$t0 + 2 * 0.4119);CI
t.test(medv)
###e)---------------------------------------------------------------------
medv.med = median(medv);medv.med
###f)---------------------------------------------------------------------
boot.ftn= function(data, index) return(median(data[index]))
boot(medv, boot.ftn, 1000)
###g)---------------------------------------------------------------------
tenth = quantile(medv, c(0.1));tenth
###h)---------------------------------------------------------------------
boot.ftn= function(data, index) return(quantile(data[index], c(0.1)))
boot(medv,boot.ftn, 1000)
