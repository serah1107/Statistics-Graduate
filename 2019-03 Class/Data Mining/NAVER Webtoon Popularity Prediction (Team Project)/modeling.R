##total data------------------------------------------------------
setwd("C:/Users/user/Desktop/데마 팀플")
library(tidyverse) ; library(caret) ; library(readr) ; library(dplyr) ;library(readxl)
library(lubridate);library(plotly);library(ggthemes) ; library(ggplot2) ;library(data.table) ;library(gridExtra)
memory.limit(size=56000) #memory increased  to 7GB
total <-read.csv("total_final_v5.csv") ; 
##factor
total$new <- as.factor(total$new)
total$writer_talk <- as.factor(total$writer_talk)
total$music <- as.factor(total$music)
total$cut_toon <- as.factor(total$cut_toon)
total$age <- as.factor(total$age)
total$month <- as.factor(total$month)
total$year <- as.factor(total$year)
# total$comment_24hour <- log(total$comment_24hour+1)
# total$naver_stock <- log(total$naver_stock)
# total$kakao_stock <- log(total$kakao_stock)
# total$prolog_interval <- scale(total$prolog_interval)
lm1<-lm(comment_24hour~., data=total)

library(randomForest) # basic implementation
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)       
set.seed(123); spl <- sample(1:2, nrow(total[,c(1:25)]), replace = T, prob = c(0.8, 0.2))
mytrain <- total[(spl==1),c(1:25)] ; myvalid <- total[(spl==2),c(1:25)]
m1 <- randomForest(  formula = log(comment_24hour+1) ~ .,  data    = mytrain[,-c(2,3)])
plot(m1)
sqrt(m1$mse[which.min(m1$mse)])
y_pred = predict(m1, myvalid)

varImpPlot(m1)

# total$upload_date=as.Date(total$upload_date);total$upload_next_date=as.Date(total$upload_next_date)
#total$upload_time=format(as.POSIXct(total$upload_time), "%H:%M:%S")
###factor: new, writer_talk, music, cut_toon, age, month, year, daily~sports, episode~story,day
# total$name = as.numeric(total$name)
# total$new = as.numeric(total$new);total$writer_talk = as.numeric(total$writer_talk);total$music = as.numeric(total$music);total$cut_toon = as.numeric(total$cut_toon);
# total$age = as.numeric(total$age);total$month = as.numeric(total$month);total$year = as.numeric(total$year);
# total$daily= as.numeric(total$daily);total$thriller= as.numeric(total$thriller);total$historical= as.numeric(total$historical);total$sports= as.numeric(total$sports);
# total$comic= as.numeric(total$comic);total$action= as.numeric(total$action);total$episode= as.numeric(total$episode);total$drama= as.numeric(total$drama);
# total$fantasy= as.numeric(total$fantasy);total$drama= as.numeric(total$drama);total$genuine= as.numeric(total$genuine);total$omnibus= as.numeric(total$omnibus);
# total$story= as.numeric(total$story);total$emotion= as.numeric(total$emotion);total$day= as.numeric(total$day);
##each total------------------------------------------------
total_name = unique(total$name) ; total_length = vector() ; each<-list() #each total
for(i in 1:length(total_name)){
  total_length[i]<-nrow(total[which(total$name==total_name[i]),])
  each[[i]]<-total[which(total$name==total_name[i]),]
}
name_length <-cbind(as.character(total_name),total_length)
##lm---------------------------------------------------------------------------------
total2<-total[,-c(2,3,26:32)]
total2$comment_24hour=scale(total2$comment_24hour)
total2$comment_24hour <- log(total2$comment_24hour+1)
total2$naver_stock <- log(total2$naver_stock)
total2$kakao_stock <- log(total2$kakao_stock)
total2$prolog_interval <- scale(total2$prolog_interval)

outliersZ <- function(data, zCutOff = 1.96, values = FALSE, digits = 2) {
  #compute standard deviation (sample version n = n [not n-1])
  stdev <- sqrt(sum((data - mean(data, na.rm = T))^2, na.rm = T) / sum(!is.na(data)))
  #compute absolute z values for each value
  absZ <- abs(data - mean(data, na.rm = T)) / stdev
  #subset data that has absZ greater than the zCutOff and replace them with replace
  #can also replace with other values (such as max/mean of data)
  data[absZ > zCutOff] <- NA
  if (values == TRUE) {
    return(round(absZ, digits)) #if values == TRUE, return z score for each value
  } else {
    return(round(data, digits)) #otherwise, return values with outliers replaced
  }
  print(absZ)
}
zCutOff = 18

data[which(absZ > zCutOff)] #1988  2011  2036  5849 19289

outliersZ <- function(data) {
  absZ=data ; stdev<-vector()
  for(i in 1:ncol(data)){
  stdev[i] <- sqrt(sum((data[,i] - mean(data[,i], na.rm = T))^2, na.rm = T) / sum(!is.na(data[,i])))
  absZ[,i] <-data.frame(abs(data[,i] - mean(data[,i], na.rm = T)) / stdev[i])
  }
  return(absZ)
}

absZ<-outliersZ(total2)
which(absZ$comment_24hour>18)#1988  2011  2036  5849 19289
which(absZ$grade_parti>18) #5849 6102
which(absZ$picture_other_total2>18)

which(absZ>18)
order(absZ, decreasing=TRUE)[1:10]
order(absZ$grade_parti, decreasing=TRUE)[1:10]

mod <- lm(comment_24hour ~ ., data=total2)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") 
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

influential =which(cooksd> 4*mean(cooksd, na.rm=T)) 
head(total2[influential, ])
View(total2[influential, ])

total3<-total2[-influential, ]
mod2 <- lm(comment_24hour ~ ., data=total3)

car::outlierTest(mod)


###rf-------------------------------------------------------
library(caret)
# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "RMSE"
set.seed(seed)
mtry <- sqrt(ncol(mytrain)-1)
#mtry <- (ncol(mytrain)-1)/3

tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(log(comment_24hour+1)~., data=mytrain, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(12)
rf_random <- train(log(comment_24hour+1)~., data=mytrain, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
##xgboost--------------------------------------------------------------------
library(xgboost)

data_train <- as.matrix(data.frame(mytrain[,-1]))
data_valid <- as.matrix(data.frame(myvalid[,-1]))

label_train <- as.matrix(data.frame(mytrain[,1]))
label_test <- as.matrix(data.frame(myvalid[,1]))

# label_test <- as.vector(label_test)
# label_test <- data.frame(myvalid[,1])

# str(data_train)
# 
# data_train <- as(data_train,"dgCMatrix")
# 

# Build the model
bst <- xgboost(data=data_train, label=label_train,max_depth = 2, eta = 0.025, nthread = 2, nrounds = 2500,objective= "reg:linear", eval_metric = "rmse")
pred <- predict(bst,data_valid)
err <- mean(as.numeric(pred > 0.5) != label_test)
print(paste("test-error=", err))

dtrain <- xgb.DMatrix(data = data_train, label=label_train)
dtest <- xgb.DMatrix(data = data_valid, label=as.matrix(label_test))
watchlist <- list(train=dtrain, test=dtest)

bst2 <- xgb.train(data=dtrain, max_depth=2,eta = 0.025, nthread = 2, nrounds = 50000,objective= "reg:linear", eval_metric = "rmse", watchlist=watchlist)
pred2 <- predict(bst2,data_valid)
err2 <- mean(as.numeric(pred2 > 0.5) != label_test)
print(paste("test-error=", err2))

# importance_matrix <- xgb.importance(colnames(data_train), model = bst2)
# 
# xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")
# 
# (gg <- xgb.ggplot.importance(importance_matrix, measure = "Frequency", rel_to_first = TRUE))
# gg + ggplot2::ylab("Frequency")

#default parameters
params <- list(booster = "gbtree", objective = "reg:linear", eta = 0.025, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv(params = params, data = dtrain, nrounds = 900, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)


#modeling2------------------------------------------------------------------
set.seed(123); spl <- sample(1:2, nrow(initial), replace = T, prob = c(0.7, 0.3))
mytrain <- initial[spl==1,] ; myvalid <- initial[spl==2,]

lm1<-lm(comment_24hour~.,data=mytrain)
summary(lm1)
lm2<-step(lm1)
#library(randomForest)
rf1<-randomForest(comment_24hour~.,data=mytrain,importance = TRUE)
varImpPlot(rf1)

g1<-glm(exp(comment_24hour)~., family="poisson", data=mytrain)
summary(g1)

library(tree)
tr1<-tree(comment_24hour~.,data=mytrain)
plot(tr1); text(tr1)

library(e1071)
sv1<-svm(comment_24hour~.,data=mytrain)
sv2<-svm(comment_24hour~.,data=mytrain, kernel="linear")
tuneout_radial <- tune(svm,comment_24hour~.,data=mytrain, kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000)))
summary(tune.out)

library(glmnet)
x <- model.matrix(comment_24hour~., initial)[,-1]
y <-initial$comment_24hour

set.seed(123)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

cv.lasso <- cv.glmnet(x[train,], y[train], alpha=1)
lasso.coef = predict(cv.lasso, type = "coefficients", s=cv.lasso$lambda.min) # coefficients
lasso.prediction = predict(cv.lasso, s=cv.lasso$lambda.min, newx = x[test,]) # coefficients
mean((ytest-lasso.prediction)^2)


library(caret)
# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 123
metric <- "RMSE"
set.seed(seed)
mtry <- (ncol(mytrain)-1)/3

tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(ty~., data=mytrain, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(123)
rf_random <- train(ty~., data=mytrain, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)



res<-read.csv("res.csv")
colnames(res)
res<-res[,-2]


res$cut_toon<-as.factor(res$cut_toon)
res$new<-as.factor(res$new)
res$writer_talk<-as.factor(res$writer_talk)
res$music<-as.factor(res$music)
res$age<-as.factor(res$age)
res$year<-as.factor(res$year)
res$month<-as.factor(res$month)
res$genre<-as.factor(res$genre)
res$cut_number<-log(res$cut_number+1)
res$naver_stock<-log(res$naver_stock)
res$kakao_stock<-log(res$kakao_stock)
res$prolog_interval<-log(res$prolog_interval+1)
res$story_debut<-log(res$story_debut)
library(tree)
lm1<-lm(inde~.,data=res)
dim(res)
View(res)

mean((res$inde-predict(lm1))^2)







