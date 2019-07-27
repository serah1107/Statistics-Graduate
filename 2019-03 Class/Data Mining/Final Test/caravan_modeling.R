setwd("C:/Users/user/Desktop/대학원3학기/data mining/종합시험/")
library(tidyverse) ; library(dplyr) ; library(ggplot2) ; library(DataExplorer)
##data-------------------------------------------------------------
Caravan = read.csv("Caravan.csv"); head(Caravan)
ncol(Caravan); nrow(Caravan)
##eda-------------------------------------------------------------
a<-table(Caravan$Purchase)
colors=c("red","green")
pie(a,main = "CUSTOMERS OF CARAVAN POLICY",col=colors);box()
#plot_boxplot(Caravan,by = 'Purchase')
#plot_density(Caravan)
##a)---------------------------------------------------------------
set.seed(1234)
train <- 1:1000
Caravan.train <- Caravan[train, ];Caravan.test <- Caravan[-train, ]
##b)---------------------------------------------------------------
library(gbm) ; library(mlr);library(xgboost)
set.seed(1234)
traintask.caravan <- makeClassifTask(data = Caravan.train, target = "Purchase", fixup.data = "quiet")
##gbm
gbm_learner <- makeLearner("classif.gbm", par.vals = list(distribution = "bernoulli", n.trees = 1000, shrinkage = 0.01), predict.type = "prob")
mod.gbm <- train(gbm_learner, traintask.caravan)
# max.features=relative.influence(getLearnerModel(mod.gbm), n.trees = 1000, scale = TRUE) %>% sort(decreasing = TRUE) %>% head(10) 
summary(  getLearnerModel(mod.gbm),   cBars = 10,  method = relative.influence)
##xgboost
xgb_learner <- makeLearner("classif.xgboost",  predict.type = "prob",  par.vals = list(objective = "binary:logistic",nrounds = 1000, eta = 0.01))
mod.xgb <- train(xgb_learner, traintask.caravan)
xgb_model<-getLearnerModel(mod.xgb)
importance_matrix <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix, top_n = 10);importance_matrix
##c)---------------------------------------------------------------
threshold <- c(Yes = 0.2, No = 0.8) #Setting up 20% Probability Threshold
##gbm
pred <- predict(mod.gbm, newdata = data.frame(Caravan.test))
pred <- setThreshold(pred, threshold = threshold)
calculateConfusionMatrix(pred)
#31/127 *100
##xgboost
pred2 <- predict(mod.xgb, newdata = data.frame(Caravan.test))
pred2 <- setThreshold(pred2, threshold = threshold)
calculateConfusionMatrix(pred2)
#29/169*100
##knn
traintask.knn.caravan <- createDummyFeatures(traintask.caravan)
testtask.knn.caravan <- createDummyFeatures(makeClassifTask(data = Caravan.test, target = "Purchase", fixup.data = "quiet"))
knn_learner <- makeLearner("classif.knn", par.vals = list(prob = TRUE))
set.seed(1234)
mod.knn <- train(knn_learner, traintask.knn.caravan)
pred3 <- predict(mod.knn, testtask.knn.caravan)
calculateConfusionMatrix(pred3)
#23/273*100
##logistic
logit.caravan <- glm(Purchase ~ ., data = Caravan.train, family = "binomial")
pred4 <- predict(logit.caravan, Caravan.test, type = "response")
pred4 <- ifelse(pred4 > 0.2, 1, 0)
table(Caravan.test$Purchase, pred4)
#58/350*100