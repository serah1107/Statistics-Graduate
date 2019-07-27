#ch2 lab--------------------------------------------------------
x=1:10
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)

fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)


plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
cylinders = as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red",varwidth = T)
plot(cylinders, mpg, col="red",varwidth = T,horizontal =T)
plot(cylinders, mpg, col="red",varwidth = T,horizontal =T,xlab="cylinders",ylab="MPG")

#새로운 기법 
#ex9----------------------------------------------------------
install.packages("ISLR")
require(ISLR)
data(Auto)
fix(Auto)
Auto = na.omit(Auto)
Auto$origin <- as.factor(Auto$origin)
#b-----------------------------------------
quant_data=Auto[,c(1:7)]
apply(quant_data,2, range)
#c-----------------------------------------
colMeans(quant_data)
apply(quant_data,2, sd)
#d-----------------------------------------
Auto2 = Auto[-c(10:85),]
quant_data2=Auto2[,c(1:7)]
apply(quant_data2,2, range)
colMeans(quant_data2)
apply(quant_data,2, sd)
#e-----------------------------------------
library(GGally)
library(ggplot2)
ggpairs(quant_data)
plot( Auto$mpg ~ Auto$origin , xlab ="Origin", ylab="MPG")
#f------------------------------------------
model <- lm(mpg ~ cylinders+year+weight+horsepower+displacement+acceleration+origin, 
            data= Auto)

t(coefficients(model))
anova(model      )
#ex10-----------------------------------------------------------------------
#a----------------------------------------
library(MASS)
head(Boston)
dim(Boston)
#b-----------------------------------------
Boston$chas <- as.numeric(Boston$chas)
Boston$rad <- as.numeric(Boston$rad)
pairs(Boston)
ggpairs(Boston)
#c----------------------------------------
cor(Boston)
#d----------------------------------------
Boston.hicrim <- subset(Boston, crim > 1)
Boston.hitax <- subset(Boston, tax > 500)
Boston.hiptratio <- subset(Boston, ptratio > 20)
pairs(Boston.hicrim[,c(1,2,3,7,8,14)])
plot(Boston.hitax$tax, Boston.hitax$zn)
pairs(Boston.hiptratio[,c(2,4,8,9,11)])
#e----------------------------------------
length(Boston$chas[Boston$chas==1])
#f----------------------------------------
median(Boston$ptratio)
#g------------------------------------------
g10 <- rbind(Boston[Boston$medv==min(Boston$medv),],sapply(Boston, range))
rownames(g10) <- c("Lowest medv 1", "Lowest medv 2", "Min", "Max")
g10
#h----------------------------------------
length(Boston$rm[Boston$rm>7])
length(Boston$rm[Boston$rm>8])
h10 <- rbind(sapply(Boston[Boston$rm>8,], range), sapply(Boston, range))
rownames(h10) <- c("Min rm>8", "Max rm>8", "Boston min", "Boston max")
h10
