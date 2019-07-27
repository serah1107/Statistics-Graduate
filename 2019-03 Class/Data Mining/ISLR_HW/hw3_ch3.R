#ch3 lab--------------------------------------------------------
library(MASS)
library(ISLR)
data(Boston)
lm.fit=lm(medv~stat ,data=Boston )
plot(lstat,medv);abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)
lm.fit=lm(medv~stat+age ,data=Boston )
summary(lm.fit)
lm.fit5=lm(medv~poly(lstat ,5))
summary(lm.fit5)
lm.fit=lm(Sales~.+ Income :Advertising +Price :Age ,data=Carseats )
Summary(lm.fit)

LoadLibraries=function (){
  + library(ISLR)
  + library(MASS)
  + print("The libraries have been loaded.")
  + }
#ex8----------------------------------------------------------
library(ISLR)
data(Auto)
##a------------------------------------------------------------------
fit <- lm(mpg ~ horsepower, data = Auto)
summary(fit)
predict(fit, data.frame(horsepower = 98), interval = "confidence")
predict(fit, data.frame(horsepower = 98), interval = "prediction")
##b------------------------------------------------------------------
plot(Auto$horsepower, Auto$mpg, main = "Scatterplot of mpg vs. horsepower", xlab = "horsepower", ylab = "mpg", col = "blue")
abline(fit, col = "red")
##c------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(fit)
#ex9----------------------------------------------------------
##a-----------------------------------------------------------------
pairs(Auto)
##b-----------------------------------------------------------------
names(Auto)
View(cor(Auto[1:8]))
##c-----------------------------------------------------------------
fit2 <- lm(mpg ~ . - name, data = Auto)
summary(fit2)
##d-----------------------------------------------------------------
par(mfrow = c(2, 2))
plot(fit2)
##e-----------------------------------------------------------------
fit3 <- lm(mpg ~ cylinders * displacement+displacement * weight, data = Auto[, 1:8])
summary(fit3)
##f-----------------------------------------------------------------
par(mfrow = c(2, 2))
plot(log(Auto$horsepower), Auto$mpg)
plot(sqrt(Auto$horsepower), Auto$mpg)
plot((Auto$horsepower)^2, Auto$mpg)
#ex13----------------------------------------------------------
##a-----------------------------------------------------------------
set.seed(1)
x <- rnorm(100)
##b-----------------------------------------------------------------
eps <- rnorm(100, sd = sqrt(0.25))
##c-----------------------------------------------------------------
y <- -1 + 0.5 * x + eps
length(y)
##d-----------------------------------------------------------------
plot(x, y)
##e-----------------------------------------------------------------
fit3 <- lm(y ~ x)
summary(fit3)
##f-----------------------------------------------------------------
plot(x, y)
abline(fit3, col = "red")
abline(-1, 0.5, col = "blue")
legend("topleft", c("Least square", "Regression"), col = c("red", "blue"), lty = c(1, 1))
##g----------------------------------------------------------------
fit4 <- lm(y ~ x + I(x^2))
summary(fit4)
##h-----------------------------------------------------------------
set.seed(1)
eps <- rnorm(100, sd = 0.125)
x <- rnorm(100)
y <- -1 + 0.5 * x + eps
plot(x, y)
fit5 <- lm(y ~ x)
summary(fit5)
abline(fit5, col = "red")
abline(-1, 0.5, col = "blue")
legend("topleft", c("Least square", "Regression"), col = c("red", "blue"), lty = c(1, 1))
##i-----------------------------------------------------------------
set.seed(1)
eps <- rnorm(100, sd = 0.5)
x <- rnorm(100)
y <- -1 + 0.5 * x + eps
plot(x, y)
fit6 <- lm(y ~ x)
summary(fit6)
abline(fit6, col = "red")
abline(-1, 0.5, col = "blue")
legend("topleft", c("Least square", "Regression"), col = c("red", "blue"), lty = c(1, 1))
##j-----------------------------------------------------------------
confint(fit3) ; confint(fit5) ; confint(fit6)
#ex14----------------------------------------------------------
##a--------------------------------------------------------------
set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100)/10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)
##b--------------------------------------------------------------
cor(x1, x2)
plot(x1, x2)
##c--------------------------------------------------------------
fit1 <- lm(y ~ x1 + x2)
summary(fit1)
##d--------------------------------------------------------------
fit2 <- lm(y ~ x1)
summary(fit2)
##e--------------------------------------------------------------
fit3 <- lm(y ~ x2)
summary(fit3)
##g--------------------------------------------------------------
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)
fit4 <- lm(y ~ x1 + x2)
fit5 <- lm(y ~ x1)
fit6 <- lm(y ~ x2)
summary(fit4) ; summary(fit5) ; summary(fit6) 
plot(fit4) ; plot(fit4) ; plot(fit4) 