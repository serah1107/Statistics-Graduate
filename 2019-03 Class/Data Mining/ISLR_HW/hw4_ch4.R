#ch4------------------------------------------------------------------------
##lab----------------------------------------------------------------------
# The Stock Market Data
library(ISLR);names(Smarket);dim(Smarket)
summary(Smarket);pairs(Smarket);cor(Smarket);cor(Smarket[,-9]);
attach(Smarket);plot(Volume)
# Logistic Regression
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fits);coef(glm.fits);summary(glm.fits)$coef;summary(glm.fits)$coef[,4]
glm.probs=predict(glm.fits,type="response");glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down",1250);glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction);(507+145)/1250;mean(glm.pred==Direction);train=(Year<2005)
Smarket.2005=Smarket[!train,];dim(Smarket.2005);Direction.2005=Direction[!train]
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response");glm.pred=rep("Down",252);glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005);mean(glm.pred==Direction.2005);mean(glm.pred!=Direction.2005) #52% correct rate
glm.fits=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response");glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up";table(glm.pred,Direction.2005);mean(glm.pred==Direction.2005)
106/(106+76) #58%   correct rate
predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")
# Linear Discriminant Analysis
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train);lda.fit;plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005);names(lda.pred);lda.class=lda.pred$class
table(lda.class,Direction.2005);mean(lda.class==Direction.2005) #56% correct rate
sum(lda.pred$posterior[,1]>=.5);sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1];lda.class[1:20];sum(lda.pred$posterior[,1]>.9)
# Quadratic Discriminant Analysis
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train);qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class;table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)#60% correct rate
# K-Nearest Neighbors
library(class)
train.X=cbind(Lag1,Lag2)[train,];test.X=cbind(Lag1,Lag2)[!train,];train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)#K=1
table(knn.pred,Direction.2005)
(83+43)/252 #50% correct rate
knn.pred=knn(train.X,test.X,train.Direction,k=3)  #K=3
table(knn.pred,Direction.2005);mean(knn.pred==Direction.2005) #54% correct rate
# An Application to Caravan Insurance Data
dim(Caravan);attach(Caravan);summary(Purchase);348/5822
standardized.X=scale(Caravan[,-86])
var(Caravan[,1]);var(Caravan[,2]);var(standardized.X[,1]);var(standardized.X[,2])
test=1:1000;train.X=standardized.X[-test,];test.X=standardized.X[test,]
train.Y=Purchase[-test];test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1);mean(test.Y!=knn.pred);mean(test.Y!="No")
table(knn.pred,test.Y);9/(68+9);knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y);5/26;knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y);4/15
glm.fits=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fits,Caravan[test,],type="response")
glm.pred=rep("No",1000); glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y);glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes";table(glm.pred,test.Y);11/(22+11)
##10----------------------------------------------------------------------
###a)------------------------------------------------------
require(ISLR); require(tidyverse) ;require(corrplot)
data('Weekly') ;glimpse(Weekly); summary(Weekly)
cor_test <- cor.mtest(Weekly[,1:8], conf.level = .90)
corrplot(cor(Weekly[,1:8]), method = 'color', 
         order = 'hclust', addrect = 3,
         p.mat = cor_test$p, sig.level = 0.1, tl.col = 'black')
###b)------------------------------------------------------
glm1 <- glm(Direction ~ . - Year - Today, data = Weekly, family = 'binomial')
summary(glm1)
###c)------------------------------------------------------
attach(Weekly)
prd <- predict(glm1, type = "response");prd_v <- ifelse(prd >= 0.5, 'Up', 'Down')
table(prd_v, Direction);acc <- paste('Accuracy:', round(mean(pred.glm == Weekly$Direction),4));acc
###d)------------------------------------------------------
train <- Weekly[Weekly$Year <= 2008,]
test <- Weekly[Weekly$Year > 2008,]
glm2 <- glm(Direction ~ Lag2, data = train, family = 'binomial')
prd2 <- predict(glm2, newdata = test, type = 'response');prd_v2 <- ifelse(prd2 >= 0.5, 'Up', 'Down');
table(prd_v2, test$Direction);acc <- paste('Accuracy:', round(mean(prd_v2 == test$Direction),4));acc

kable(table(pred_values, test$Direction), 
      format = 'html') %>%
  kable_styling() %>%
  add_header_above(c('Predicted' = 1, 'Observed' = 2)) %>%
  column_spec(1, bold = T) %>%
  add_footnote(label = acc)
###e)------------------------------------------------------
library(MASS)
mlda <- lda(Direction ~ Lag2, data = train)
prd3 <- predict(mlda, newdata = test);prd_v3 <- prd3$class
table(prd_v3, test$Direction);acc <- paste('Accuracy:', round(mean(prd_v3 == test$Direction),4));acc
###f)------------------------------------------------------
mlda <- qda(Direction ~ Lag2, data = train)
prd3 <- predict(mlda, newdata = test);prd_v3 <- prd3$class
table(prd_v3, test$Direction);acc <- paste('Accuracy:', round(mean(prd_v3 == test$Direction),4));acc
###g)------------------------------------------------------
library(class)
mknn <- knn(train = data.frame(train$Lag2), test = data.frame(test$Lag2),cl = train$Direction, k = 1)
table(mknn, test$Direction);acc <- paste('Accuracy:', round(mean(mknn == test$Direction),4));acc
###i)------------------------------------------------------
#knn==========================
acc <- list('1' = 0.5)
for (i in 1:20) {
  mknn <- knn(train = data.frame(train$Lag2), test = data.frame(test$Lag2), cl = train$Direction, k = i)
  acc[as.character(i)] = round(mean(mknn == test$Direction),4)
}
acc <- unlist(acc)
data_frame(acc = acc) %>%
  mutate(k = row_number()) %>%
  ggplot(aes(k, acc)) +
  geom_col(aes(fill = k == which.max(acc))) +
  labs(x = 'K', y = 'Accuracy', title = 'KNN Accuracy for different K') +
  scale_x_continuous(breaks = 1:20) +
  coord_cartesian(ylim = c(min(acc), max(acc))) +
  guides(fill = FALSE)
#lda==============================
mlda2 <- lda(Direction ~ Lag2 + Lag4, data = train)
prd5 <- predict(mlda2, newdata = test); prd_v5 <- prd5$class
acc <- paste('Accuracy:', round(mean(prd_v5 == test$Direction),4));acc
#qda==============================
mqda2 <- qda(Direction ~ Lag2 + Lag1, data = train)
prd6 <- predict(mqda2, newdata = test);prd_v6 <- prd6$class
acc <- paste('Accuracy:', round(mean(prd_v6 == test$Direction),4));acc
#glm==============================
mglm3 <- glm(Direction ~ (. - Today - Volume)*(. - Today - Volume), data = train, family = 'binomial')
step <- stepAIC(mglm3, direction = 'both', trace = 0)
prd7 <- predict(step, newdata = test, type = 'response');prd_v7 <- ifelse(prd7 >= 0.5, 'Up', 'Down')
acc <- paste('Accuracy:', round(mean(prd_v7 == test$Direction),4));acc
##11----------------------------------------------------------------------
###a)------------------------------------------------------
attach(Auto)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
Auto <- data.frame(Auto, mpg01)
###b)------------------------------------------------------
cor(Auto[, -9])
library(corrplot) ; corrplot.mixed(cor(Auto[, -9]), upper="circle")
par(mfrow=c(2,3))
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")
###c)------------------------------------------------------
set.seed(123)
sam <- sample(1:dim(Auto)[1], dim(Auto)[1]*.7, rep=FALSE)
train<- Auto[sam, ]; test= Auto[-sam, ]
mpg01.test <- mpg01[-sam]
###d)------------------------------------------------------
mlda3 <- lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = train)
prd.lda <- predict(mlda3, test)
table(prd.lda$class, mpg01.test);round(mean(prd.lda$class != mpg01.test),4)
###e)------------------------------------------------------
mqda3 = qda(mpg01 ~ cylinders + horsepower + weight + acceleration, data=train)
prd.qda <- predict(mqda3, test)
table(prd.qda$class, mpg01.test);round(mean(prd.qda$class != mpg01.test),4)
###f)------------------------------------------------------
mglm3 <- glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = train, family = binomial)
summary(mglm3);prob <- predict(mglm3, test, type = "response")
prd.glm <- rep(0, length(prob));prd.glm[prob > 0.5] <- 1
table(prd.glm, mpg01.test) ; round(mean(prd.glm != mpg01.test),4)
###g)------------------------------------------------------
train.X <- cbind(cylinders, weight, displacement, horsepower)[sam, ]
test.X <- cbind(cylinders, weight, displacement, horsepower)[-sam, ]
train.mpg01 <- mpg01[sam]
set.seed(1)
err <- list('1' = 0.5)
for (i in 1:100) {
  mknn <- knn(train = train.X ,test = test.X, cl = train.mpg01, k = i)
  err[as.character(i)] = round(mean(mknn != mpg01.test),4)
}
err <- unlist(err)
data_frame(err = err) %>%
  mutate(k = row_number()) %>%
  ggplot(aes(k, err)) +
  geom_col(aes(fill = k == which.min(err))) +
  labs(x = 'K', y = 'Test Error', title = 'KNN Test Error for different K') +
  scale_x_continuous(breaks = 1:20) +
  coord_cartesian(ylim = c(min(err), max(err))) +
  guides(fill = FALSE)
min(err)
