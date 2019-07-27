library(dplyr) ; library(readr); library(tidyverse);
library(highcharter) ;library(lubridate);library(stringr);library(xts)
library(ggplot2);library(mice);library(DMwR);library(randomForest)
library(DataExplorer)
setwd("C:/Users/user/Desktop/project")
train= read_csv("dengue_labels_train.csv")
data = read_csv("dengue_features_train.csv")
test = read_csv("dengue_features_test.csv")
##data----------------------------------------------------
data2 = cbind(data,train$total_cases) #1456
colnames(data2)[colnames(data2)=="train$total_cases"] <-"total_cases"
#na detection----------------------------------------------------------------
which(is.na(data2))
plot_missing(data2)
###na deletion & outlier----------------------------------------------------------------------
complete <- data2[complete.cases(data2),] %>% mutate(miss = 0)
missing <- data2[!complete.cases(data2),] %>% mutate(miss = 1)
missing <- missing[(apply(select(missing,-total_cases), MARGIN = 1, function(x) sum(is.na(x))) <= 10),]
data3<- rbind(complete , missing) #1446
outlier <- lofactor(data3 %>% filter(miss==0)%>%select(-city,-week_start_date), k = 90)
plot(density(outlier), main = "outlier score")
sort(outlier, decreasing = T)[1:50]
out <- order(outlier, decreasing = T)[1:5]
data3 <- data3 [-out,] #5
###knn
complete0 = complete
missing0 = missing
complete_miss <- complete0
set.seed(1004)
random1 <- sample(1:nrow(complete_miss), as.integer(nrow(complete_miss)*nrow(missing0)/(nrow(complete0)+nrow(missing0)))) 
random2 <- sample(c(2:3,5:24), as.integer(nrow(complete_miss)*nrow(missing0)/(nrow(complete0)+nrow(missing0))), replace = T)
for (i in 1:length(random1)) complete_miss[random1[i], random2[i]] <- NA
blank= complete_miss[, -c(1,4,25:26)]
complete_knn <- knnImputation(data.frame(blank), k = 50)
blank2 = complete0[, -c(1,4,25:26)]
regr.eval(select(blank2, 9), select(complete_knn,9))
# mae         mse        rmse        mape 
# 10.69415357  7.85206396  2.80215345  0.03576064 
##median & mean-------------------------------------------------------------------
##MEAN----------------------------------------------------------------------------
library(Hmisc)
complete_miss2 = complete_miss
for (i in 1:length(random1)) complete_miss2[random1[i], random2[i]] <- NA
complete_miss2$precipitation_amt_mm=impute(complete_miss2$precipitation_amt_mm, mean)  # replace with mean
library(DMwR)
actuals <- complete0$precipitation_amt_mm
predicteds <- complete_miss2$precipitation_amt_mm
regr.eval(actuals, predicteds)
# mae       mse      rmse      mape 
# 0.2331869 7.5199168 2.7422467       NaN 
#MEDIAN-----------------------------------------------------------------------------
complete_miss2 = complete_miss
for (i in 1:length(random1)) complete_miss2[random1[i], random2[i]] <- NA
complete_miss2$precipitation_amt_mm=impute(complete_miss2$precipitation_amt_mm, median)  # replace with median
library(DMwR)
actuals <- complete0$precipitation_amt_mm
predicteds <- rep(mean(complete_miss2$precipitation_amt_mm, na.rm=T), length(actuals))
regr.eval(actuals, predicteds)
# mae        mse       rmse       mape 
# 32.97844 1862.88183   43.16111        Inf 
#rpart-----------------------------------------------------------------------------
library(rpart)
complete_miss3 = complete_miss
for (i in 1:length(random1)) complete_miss3[random1[i], random2[i]] <- NA
anova_mod1 <- rpart(precipitation_amt_mm~., data=complete_miss3[which(!is.na(complete_miss3$precipitation_amt_mm)), -c(1,4,25:26)], method="anova", na.action=na.omit)  # since ptratio is numeric.
start_pred1 <- predict(anova_mod1, complete_miss3[which(is.na(complete_miss3$precipitation_amt_mm)),-c(1,4,25:26)])
names(start_pred1) <- 1:13
actuals <- complete0$precipitation_amt_mm[is.na(complete_miss3$precipitation_amt_mm)]
regr.eval(actuals, start_pred1) 
# mae       mse      rmse      mape 
# 4.321871 30.754341  5.545660       Inf 
######=> mean----------------------------------------------------------------------------------------
# unique(store2$Size)
data3$precipitation_amt_mm=impute(data3$precipitation_amt_mm, mean)  # replace with mean
data3$ndvi_ne=impute(data3$ndvi_ne, mean)  # replace with mean
data3$ndvi_se=impute(data3$ndvi_se, mean)  # replace with mean
data3$ndvi_nw=impute(data3$ndvi_nw, mean)  # replace with mean
data3$ndvi_sw=impute(data3$ndvi_sw, mean)  # replace with mean
data3$station_max_temp_c=impute(data3$station_max_temp_c, mean)  # replace with mean
data3$station_min_temp_c=impute(data3$station_min_temp_c, mean)  # replace with mean
data3$station_precip_mm=impute(data3$station_precip_mm, mean)  # replace with mean
data3$station_diur_temp_rng_c=impute(data3$station_diur_temp_rng_c, mean)  # replace with mean
data3$station_avg_temp_c=impute(data3$station_avg_temp_c, mean)  # replace with mean
data3$reanalysis_sat_precip_amt_mm=impute(data3$reanalysis_sat_precip_amt_mm, mean)  # replace with mean
# plot_missing(data3)
##OUTLIER final--------------------------------------------------------------------------------------
outlier <- lofactor(data3 %>% select(-city,-week_start_date), k = 90)
plot(density(outlier), main = "outlier score")
sort(outlier, decreasing = T)[1:50]
out <- order(outlier, decreasing = T)[1:2]
data3 <- data3[-out,] #2
data3<-data3[,-26]#miss deletion
nrow(data3)
##corr------------------------------------------------------------
library(Hmisc)
data.rcorr = cor(as.matrix(data3[,-c(1,4)]))
data.rcorr
# library(PerformanceAnalytics)
# chart.Correlation(data3[,-c(1,4)],
#                   method="pearson",
#                   histogram=TRUE,
#                  pch=16) 
data3=data3[,-c(10,17,12)]
###marginal plot-------------------------------------------------------------------------
#hist-density--------------------------------------------------------
# plot_histogram(data3)
plot_density(data3)
#log trans--------------------------------------------------------
data3$total_cases = log(data3$total_cases+1)
data3$station_precip_mm=log(data3$station_precip_mm+1)
data3$station_diur_temp_rng_c=log(data3$station_diur_temp_rng_c+1)
data3$reanalysis_tdtr_k=log(data3$reanalysis_tdtr_k+1)
data3$precipitation_amt_mm=log(data3$precipitation_amt_mm+1)
data3$reanalysis_precip_amt_kg_per_m2=log(data3$reanalysis_precip_amt_kg_per_m2+1)
#factor----------------------------------------------------------
data3$city=as.factor(data3$city)
data3$year=as.factor(data3$year)
data3 = data3[,-4]#date deletion
#year---------------------------------------------------------------------
ggplot(data3%>%filter(year==2003), aes(x=weekofyear, y=total_cases,col=city)) +
  geom_line()+theme_light()
ggplot(data3%>%filter(city=='sj'), aes(x=reanalysis_relative_humidity_percent, y=total_cases)) +
  geom_point()+ stat_smooth(method = "lm")+theme_light()
ggplot(data3%>%filter(city=='sj'), aes(x=station_avg_temp_c  , y=total_cases)) +
  geom_point()+ stat_smooth(method = "lm")+theme_light()
####train & test---------------------------------------------------
set.seed(1004) ; spl <- sample(1:2, nrow(data3), replace = T, prob = c(0.8, 0.2))
mytrain <- data3[(spl==1),] ; myvalid <- data3[(spl==2),]
# colnames(data2)
###1.linear regression--------------------------------------------------------
lm1=lm(total_cases~.,mytrain)
summary(lm1) #58%
pred <- predict(lm1, newdata = myvalid)
mean((pred-myvalid$total_cases)^2) #0.7606148
###2.lasso regression--------------------------------------------------------
library(glmnet)
x <- mytrain[,-21]%>% data.matrix()
y <- mytrain[,21]%>% data.matrix()
cv.lasso <- cv.glmnet(x, y, alpha=1)
plot(cv_fit)
lasso.coef = predict(cv.lasso, type = "coefficients", s=cv.lasso$lambda.min) #  0.004500906
lasso.prediction = predict(cv.lasso, s=cv.lasso$lambda.min, newx = myvalid[,-21]%>% data.matrix()) # coefficients
mean((lasso.prediction-myvalid$total_cases)^2) #] 0.9504377
###3.ridge regression--------------------------------------------------------
library(glmnet)
x <- mytrain[,-21]%>% data.matrix()
y <- mytrain[,21]%>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)
cv_ridge<- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
plot(cv_ridge)
opt_lambda <- cv_ridge$lambda.min
fit <- cv_fit$glmnet.fit
summary(fit)
ridge.pred <- predict(cv_ridge, s = opt_lambda, newx = myvalid[,-21]%>% data.matrix())
mean((ridge.pred-myvalid$total_cases)^2) #] 0.9496884
##densityplot--------------------------------------------------------------
data3 %>%
  count(total_cases) %>%
  hchart('area', hcaes(x = "total_cases", y = "n")) %>%
  hc_colors("#fb4901") %>%
  hc_add_theme(hc_theme_ffx()) %>%
  hc_title(text = "Distribution of total_cases")
###4. quantile regression------------------------------------------------------
x <- mytrain[,-21]%>% data.matrix()
for(i in 1:20) x[,i]=as.numeric(x[,i])
for(i in 1:20) x[,i]=scale(x[,i])
x=cbind(rep(1,nrow(x)), x)
x=data.matrix(x)
colnames(x)[1]="intercept"
head(x)
y=as.matrix(scale(mytrain[,21]))
lm.summary=lm(y~(x-1)) 
mu=lm.summary$coefficients 
Tau=solve(vcov(lm.summary))*0.01
Ones=rep(1,length(y))
initsList=list(beta=mu)
K=ncol(x)
nAdapt=1000; nUpdate=10000 ; nIter=30000; nChains=3
p.set=seq(0.1,0.9, length=9)
beta.hat=matrix(0, length(p.set),K)
beta.cl=beta.cu=matrix(0, length(p.set),K)
#### Model ####
modelString ="
model { 
for(i in 1:length(y)){
e[i]<- y[i,] - inprod( x[i,1:K] ,beta[1:K]) 
check[i]<- e[i]*( p-( 1-step(e[i]) ) )
fd[i] <- p*(1-p)*exp( -check[i] )

pi[i] <- fd[i]/10000
Ones[i] ~ dbern( pi[i] )
}
beta[1:K]~ dmnorm( mu[], Tau[,] )
}
"
writeLines(modelString, "model_QR.txt")
library(rjags)
library(runjags)
for(ip in 1:length(p.set)) {
  p=p.set[ip]
  
  #for model_QR.txt
  dataList=list(p=p, K=K, y=y, x=x, Ones=Ones, mu=mu, Tau=Tau)
  
  jagsModel=jags.model(file="model_QR.txt", data=dataList, 
                       inits=initsList, n.chains=3, n.adapt=nAdapt)
  update(jagsModel, n.iter=nUpdate)
  codaSamples=coda.samples(jagsModel, variable.names=c("beta"), 
                           thin=1, n.iter=nIter)
  #source(MyFunc.r)
  #convDiag(codaSamples)
  mcmcSamples=as.matrix(codaSamples)
  
  beta.hat[ip,1:K ]= apply( mcmcSamples[,1:K],2,quantile, 0.5)
  beta.cl[ip,1:K ]= apply( mcmcSamples[,1:K],2,quantile, 0.025)
  beta.cu[ip,1:K ]= apply( mcmcSamples[,1:K],2,quantile, 0.975)
}
par(mfrow=c(3,3))
for(k in 19:21) {
  plot(p.set, beta.hat[,k],type="l", ylim=c(min( beta.cl[,k]), max( beta.cu[,k])),
       xlab="p", ylab=colnames(x)[k] ) 
  lines(p.set, beta.cl[,k], col=3)
  lines(p.set, beta.cu[,k], col=3)
  abline(h=0)
}
coda::gelman.diag(codaSamples)
var=colnames(codaSamples[[1]])
par(mfrow=c(length(var),2))

convDiag.plot=function(codaSamples, var){
  ESS=effectiveSize(codaSamples[, var])
  par(mfrow=c(1,2))
  traceplot(codaSamples[, var], xlab=var)
  acf(as.matrix(codaSamples[,var]),main=paste0("ESS=", round(ESS,2))  )
}
convDiag=function(codaSamples){
  ESS=effectiveSize(codaSamples);cat("ESS = " , ESS, "\n")
  acceptRate=1-rejectionRate(codaSamples);cat("Accept rate = " , acceptRate, "\n")
  gelman=gelman.diag(codaSamples) #; cat("Gelman statistics  " ,"\n",  gelman, "\n")
  gelman
  gelman.1=as.matrix(gelman$psrf)
  if( max(gelman.1) > 1.1 ) cat ("Warning: Gelman Shrink Factor > 1.1", "\n")
  gelman.2=gelman$mpsrf
  if( gelman.2 > 1.1 ) cat ("Warning: Gelman Multivariate Shrink Factor > 1.1", "\n")
}
for(i in 1:length(var))convDiag.plot(codaSamples, var[i])
convDiag(codaSamples)
######gvs-----------------------------------------------------------------------
K=ncol(x)
p=0.5
library(quantreg)
rq.out=rq.fit.br(x,y,p, ci=T)
pseudo.mean.beta=rq.out$coefficients[1:K,1]
pseudo.sd.beta=(rq.out$coefficients[1:K,3]-rq.out$coefficients[1:K,2])/(2*1.96)
pseudo.var.beta=pseudo.sd.beta^2
z=rep(1,length(y))
dataList=list(p=p, K=K, y=y, x=x,z=z, pseudo.mean.beta=pseudo.mean.beta,
              pseudo.var.beta=pseudo.var.beta)
gammaInit=rep(0,K)
initsList=list(beta=pseudo.mean.beta, gamma=gammaInit) 
modelString = "
model { 
for (j in 1:K) { gbeta[j]<- gamma[j]*beta[j] }

for (i in 1:length(y)) {
fd[i] <- p*(1-p)*exp(- check[i] )
check[i] <- e[i]*(p-(1-step(e[i])))
e[i] <- y[i,] - inprod( x[i,1:K], gbeta[1:K]) 

z[i] ~ dbern( pi[i]) 
pi[i] <- fd[i]/10000
}

for (j in 1:K) { gamma[j] ~ dbern(0.5) }

for(j in 1:K) { 
beta[j] ~ dmnorm( mu[j], tau[j] )
mu[j] <- (1- gamma[j]) * pseudo.mean.beta[j]
tau[j] <- gamma[j]/100 + (1-gamma[j])/pseudo.var.beta[j]
}
}
"
writeLines(modelString, "model_GVS.txt")
require(rjags)
jagsModel=jags.model(file="model_GVS.txt", data=dataList, inits=initsList,n.chains=3, n.adapt=10000)
update(jagsModel, n.iter=10000)
codaSamples=coda.samples(jagsModel, variable.names=c("gamma"), thin=1, n.iter=15000)
mcmcSamples=as.matrix(codaSamples)
gamma.samples=mcmcSamples
gamma.hat=apply(gamma.samples,2,mean)
names(gamma.hat)=colnames(x)
gamma.hat
#install.packages("plyr”)
require(plyr)
freq=count(gamma.samples) 
colnames(freq)=c(colnames(x), "prob")
freq[,K+1]=freq[,K+1]/nrow(gamma.samples)
rankModel= as.integer(rank( -freq[,K+1]))
topModel=which(rankModel==1)
freq[topModel,]
freq[order(rankModel)[1:5], ]
library(MASS)
stepAIC(lm1, direction="both" )
###ml models--------------------------------------------------------------------------------------
# library(tree)
# tr1<-tree(total_cases~.,data=mytrain)
# plot(tr1); text(tr1)
#svm---------------------------------------------------------------------
library(e1071)
x <- mytrain[,-21]%>% data.matrix()
y<- mytrain[,21]%>% data.matrix()
# sv1<-svm(x,y,probability=T,kernel="polynomial")
# sv2<-svm(x,y,probability=T,kernel="linear")
# pred1 <- predict(sv1,myvalid[,-21]%>% data.matrix())
# pred2 <- predict(sv2,myvalid[,-21]%>% data.matrix())
actuals = myvalid$total_cases
mean((actuals-pred1)^2) #0.9680686
mean((actuals-pred2)^2) #0.937941
# tuned <- tune.svm(total_cases~.,
#                   data = mytrain,
#                   gamma = 10^(-1:-3), cost = 10^(1:3),  kernel="linear",
#                   tunecontrol = tune.control(cross = 10)) # 0.01  100
# summary(tuned)
svm_fit<-svm(x,y,probability=T,cost=10, gamma=0.01, kernel="linear")
pred1 <- predict(svm_fit,myvalid[,-21]%>% data.matrix())
mean((actuals-pred1)^2) #0.9374497
#xgboost---------------------------------------------------------------
require(xgboost) ;library(mlr)
labels <- mytrain$total_cases 
ts_label <- myvalid$total_cases
new_tr <- model.matrix(~.+0,data = mytrain[,-21]) 
new_ts <- model.matrix(~.+0,data = myvalid[,-21])

dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)

params <- list(booster = "gbtree",  objective = "reg:linear", eta=0.1, gamma=0, max_depth=7, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T,  maximize = F)
min(xgbcv$evaluation_log)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 100, watchlist = list(val=dtest,train=dtrain),  maximize = F , eval_metric = "rmse")
xgbpred <- predict (xgb1,dtest)
mean((xgbpred-actuals)^2) # 0.2834668
names <- dimnames(data.matrix(mytrain[,-21]))[[2]]
importance_matrix <- xgb.importance(model = xgb1)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

test = test[,-c(10,17,12)]
test = xgb.DMatrix(test)
xgbpred_final <- predict (xgb1,test)


##randomforest---------------------------------------------------
library(caret);library(randomForest)
# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(1004)
metric <- "Accuracy"
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry, .ntree=c(1000, 1500, 2000, 2500))
rf_default <- randomForest(total_cases~., data=mytrain, method="rf",  metric=metric, tuneLength=15, trControl=control)
print(rf_default)
y_pred <-predict(rf_default, myvalid[,-21])
mean((y_pred-actuals)^2) # 0.3707271
varImpPlot(rf_default) 
##bayesian network----------------------------------------------------------
library(bnlearn)
bn_df <- data.frame(mytrain)
res <- hc(bn_df)
# plot(res)
fittedbn <- bn.fit(res, data = mytrain)
pred=predict(fittedbn,"total_cases",method = "parents",data =myvalid)
mean((pred-actuals)^2) #0.5743067
##bayesian optimization-------------------------------------------------------
library(keras)
#one-hot encoding
labels <- data.matrix(mytrain$total_cases )
ts_label <- data.matrix(myvalid$total_cases)
new_tr <- model.matrix(~.+0,data = mytrain[,-21]) 
new_ts <- model.matrix(~.+0,data = myvalid[,-21])

X_train <-scale(new_tr)
X_val <- scale(new_ts)

y_train <-scale(labels)
y_val <- scale(ts_label)
#Parameters/config -------------------------------------------------------
n <- nrow(X_train)
n_epochs <- 100
output_dim <- 1
num_MC_samples <- 20
batch_size <- 200
l <- 1e-4
wd <- l ^ 2 / n
dd <- 2 / n

#R6 wrapper class, a subclass of KerasWrapper
ConcreteDropout <- R6::R6Class("ConcreteDropout",
                               
                               inherit = KerasWrapper,
                               
                               public = list(
                                 weight_regularizer = NULL,
                                 dropout_regularizer = NULL,
                                 init_min = NULL,
                                 init_max = NULL,
                                 is_mc_dropout = NULL,
                                 supports_masking = TRUE,
                                 p_logit = NULL,
                                 p = NULL,
                                 
                                 initialize = function(weight_regularizer,
                                                       dropout_regularizer,
                                                       init_min,
                                                       init_max,
                                                       is_mc_dropout) {
                                   self$weight_regularizer <- weight_regularizer
                                   self$dropout_regularizer <- dropout_regularizer
                                   self$is_mc_dropout <- is_mc_dropout
                                   self$init_min <- k_log(init_min) - k_log(1 - init_min)
                                   self$init_max <- k_log(init_max) - k_log(1 - init_max)
                                 },
                                 
                                 build = function(input_shape) {
                                   super$build(input_shape)
                                   
                                   self$p_logit <- super$add_weight(
                                     name = "p_logit",
                                     shape = shape(1),
                                     initializer = initializer_random_uniform(self$init_min, self$init_max),
                                     trainable = TRUE
                                   )
                                   
                                   self$p <- k_sigmoid(self$p_logit)
                                   
                                   input_dim <- input_shape[[2]]
                                   
                                   weight <- private$py_wrapper$layer$kernel
                                   
                                   kernel_regularizer <- self$weight_regularizer * 
                                     k_sum(k_square(weight)) / 
                                     (1 - self$p)
                                   
                                   dropout_regularizer <- self$p * k_log(self$p)
                                   dropout_regularizer <- dropout_regularizer +  
                                     (1 - self$p) * k_log(1 - self$p)
                                   dropout_regularizer <- dropout_regularizer * 
                                     self$dropout_regularizer * 
                                     k_cast(input_dim, k_floatx())
                                   
                                   regularizer <- k_sum(kernel_regularizer + dropout_regularizer)
                                   super$add_loss(regularizer)
                                 },
                                 
                                 concrete_dropout = function(x) {
                                   eps <- k_cast_to_floatx(k_epsilon())
                                   temp <- 0.1
                                   
                                   unif_noise <- k_random_uniform(shape = k_shape(x))
                                   
                                   drop_prob <- k_log(self$p + eps) - 
                                     k_log(1 - self$p + eps) + 
                                     k_log(unif_noise + eps) - 
                                     k_log(1 - unif_noise + eps)
                                   drop_prob <- k_sigmoid(drop_prob / temp)
                                   
                                   random_tensor <- 1 - drop_prob
                                   
                                   retain_prob <- 1 - self$p
                                   x <- x * random_tensor
                                   x <- x / retain_prob
                                   x
                                 },
                                 
                                 call = function(x, mask = NULL, training = NULL) {
                                   if (self$is_mc_dropout) {
                                     super$call(self$concrete_dropout(x))
                                   } else {
                                     k_in_train_phase(
                                       function()
                                         super$call(self$concrete_dropout(x)),
                                       super$call(x),
                                       training = training
                                     )
                                   }
                                 }
                               )
)

# function for instantiating custom wrapper
layer_concrete_dropout <- function(object, 
                                   layer,
                                   weight_regularizer = 1e-6,
                                   dropout_regularizer = 1e-5,
                                   init_min = 0.1,
                                   init_max = 0.1,
                                   is_mc_dropout = TRUE,
                                   name = NULL,
                                   trainable = TRUE) {
  create_wrapper(ConcreteDropout, object, list(
    layer = layer,
    weight_regularizer = weight_regularizer,
    dropout_regularizer = dropout_regularizer,
    init_min = init_min,
    init_max = init_max,
    is_mc_dropout = is_mc_dropout,
    name = name,
    trainable = trainable
  ))
}

# Model -------------------------------------------------------------------
get_model <- function(input_dim, hidden_dim) {
  input <- layer_input(shape = input_dim)
  output <-
    input %>% layer_concrete_dropout(
      layer = layer_dense(units = hidden_dim, activation = "softmax"),
      weight_regularizer = wd,
      dropout_regularizer = dd
    ) %>% layer_concrete_dropout(
      layer = layer_dense(units = hidden_dim, activation = "softmax"),
      weight_regularizer = wd,
      dropout_regularizer = dd
    ) %>% layer_concrete_dropout(
      layer = layer_dense(units = hidden_dim, activation = "softmax"),
      weight_regularizer = wd,
      dropout_regularizer = dd
    )
  mean <-
    output %>% layer_concrete_dropout(
      layer = layer_dense(units = output_dim),
      weight_regularizer = wd,
      dropout_regularizer = dd
    )
  
  log_var <-
    output %>% layer_concrete_dropout(
      layer_dense(units = output_dim),
      weight_regularizer = wd,
      dropout_regularizer = dd
    )
  
  output <- layer_concatenate(list(mean, log_var))
  
  model <- keras_model(input, output)
  
  heteroscedastic_loss <- function(y_true, y_pred) {
    mean <- y_pred[, 1:output_dim]
    log_var <- y_pred[, (output_dim + 1):(2 * output_dim)]
    precision <- k_exp(-log_var)
    k_sum(precision * (y_true - mean) ^ 2 + log_var, axis = 2)
  }
  
  model %>% compile(optimizer =optimizer_rmsprop(lr = 0.001, rho = 0.9),
                    loss = heteroscedastic_loss,
                    metrics = c("mse"))
}

model <- get_model(40, 10)
hist <-
  model %>% fit(
    #X_train[, 1],
    #X_train[, 2],
    #X_train[, 3],
    #X_train[, 4],
    X_train,
    y_train,
    #validation_data = list(X_val[, 1], y_val),
    #validation_data = list(X_val[, 2], y_val),
    #validation_data = list(X_val[, 3], y_val),
    #validation_data = list(X_val[, 4], y_val),
    validation_data = list(X_val, y_val),
    epochs = n_epochs,
    batch_size = batch_size,
    verbose = 1
  )

MC_samples <-
  array(0, dim = c(num_MC_samples, nrow(X_val), 2 * output_dim))
for (k in 1:num_MC_samples) {
  #MC_samples[k, , ] <- (model %>% predict(X_val[, 1]))
  #MC_samples[k, , ] <- (model %>% predict(X_val[, 2]))
  #MC_samples[k, , ] <- (model %>% predict(X_val[, 3]))
  MC_samples[k, , ] <- (model %>% predict(X_val))
}

means <- MC_samples[, , 1:output_dim]

predictive_mean <- apply(means, 2, mean)

epistemic_uncertainty <- apply(means, 2, var)

logvar <- MC_samples[, , (output_dim + 1):dim(MC_samples)[3]]
aleatoric_uncertainty <- exp(colMeans(logvar))
df <- data.frame(
  x = X_val[,40],
  y_pred = predictive_mean,
  e_u_lower = predictive_mean - sqrt(epistemic_uncertainty),
  e_u_upper = predictive_mean + sqrt(epistemic_uncertainty),
  a_u_lower = predictive_mean - sqrt(aleatoric_uncertainty),
  a_u_upper = predictive_mean + sqrt(aleatoric_uncertainty),
  u_overall_lower = predictive_mean - 
    sqrt(epistemic_uncertainty) - 
    sqrt(aleatoric_uncertainty),
  u_overall_upper = predictive_mean + 
    sqrt(epistemic_uncertainty) + 
    sqrt(aleatoric_uncertainty)
)

results <- model %>% evaluate(X_val, y_val, verbose = 0)
results$mean_squared_error #0.8945103

# preds %>% glimpse()
ggplot(df, aes(x, y_pred)) + 
  geom_point() + 
  geom_ribbon(aes(ymin = e_u_lower, ymax = e_u_upper), alpha = 0.3)

pca <- X_val %>% prcomp(center = FALSE)
plot(pca)
# 
# ###BAS-----------------------------------------------------------
# library(BAS)
# 
# mytrain2 = mytrain
# mytrain2 = scale(mytrain2)
# 
# myvalid2 = myvalid
# myvalid2 = scale(myvalid2)
# bas.lm <- bas.lm(total_cases ~ .,
#                  data = mytrain2,
#                  prior = "ZS-null",
#                  modelprior = uniform(),  method = "MCMC",initprobs = "eplogp",
#                  force.heredity = FALSE, pivot = TRUE
# )
# plot(bas.lm, which=1, ask=FALSE)
# plot(bas.lm, which=4, ask=FALSE, cex.lab=0.5)
# par(mfrow=c(4,4))
# plot(coefficients(bas.lm), subset=c(1:16), ask=FALSE)
# 
# pre =predict(bas.lm, newdata=myvalid2[,-21], estimator="BMA", se.fit=TRUE, interval="predict")
# actuals = myvalid2[,21]
# mean((pre-actuals)^2)
