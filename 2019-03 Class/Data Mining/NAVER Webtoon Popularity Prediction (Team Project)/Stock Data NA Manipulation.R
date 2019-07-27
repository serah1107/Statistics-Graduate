library(readr) ; library(tidyverse)
setwd("C:/Users/user/Desktop/날짜별 합치기/주가 데이터-20190515T084055Z-001/주가 데이터")
kakao <-read_excel("KAKAO1.xlsx") ; naver <- read_excel("NAVER1.xlsx")
colnames(kakao)<-c("date","start","end") ; colnames(naver)<-c("date","start","end")
date=data.frame(seq(as.Date("2006-05-02"), as.Date("2019-04-30"), by=1));colnames(date)<-"date"

kakao.new=merge(kakao,date,all=TRUE, by = "date") ; kakao.new=kakao.new[!duplicated(kakao.new[,1]),]
naver.new=merge(naver,date,all=TRUE, by = "date") ; naver.new=naver.new[!duplicated(naver.new[,1]),] 

kakao.new=kakao.new%>%mutate("stock" = (start+end)/2)%>%select(-start, -end)
naver.new=naver.new%>%mutate("stock" = (start+end)/2)%>%select(-start, -end)

complete.k1 = kakao.new[complete.cases(kakao.new),] ; miss.k= kakao.new[!complete.cases(kakao.new),]
complete.n1= naver.new[complete.cases(naver.new),];miss.n = naver.new[!complete.cases(naver.new),]

#miss 만들기 
set.seed(1234)
complete.k=complete.k1; complete.n=complete.n1
random1 <- sample(1:nrow(complete.k), as.integer(nrow(complete.k)*nrow(miss.k)/(nrow(complete.k)+nrow(miss.k)))) 
random2 <- sample(1:nrow(complete.n), as.integer(nrow(complete.n)*nrow(miss.n)/(nrow(complete.n)+nrow(miss.n))))
for (i in 1:length(random1)) complete.k[random1[i],2] <- NA
for (i in 1:length(random2)) complete.n[random2[i],2] <- NA

complete.k=complete.k[complete.cases(complete.k$date),] ; 
complete.n=complete.n[complete.cases(complete.n$date),] ; 

#mean, median 
library(Hmisc)
kakao.new2 = complete.k ; naver.new2=complete.n
kakao.new2$stock=impute(kakao.new2$stock, mean)  # replace with mean
naver.new2$stock=impute(naver.new2$stock, mean)  # replace with mean

library(DMwR)
actuals <- complete.k1$stock
predicteds <- rep(mean(kakao.new2$stock, na.rm=T), length(actuals))
regr.eval(actuals, predicteds) #1.016436e+09

actuals2 <- complete.n1$stock
predicteds2 <- rep(mean(naver.new2$stock, na.rm=T), length(actuals2))
regr.eval(actuals2, predicteds2) # 7.335316e+10

kakao.new2 = complete.k ; naver.new2=complete.n
kakao.new2$stock=impute(kakao.new2$stock, median)  # replace with median
naver.new2$stock=impute(naver.new2$stock, median)  # replace with median

library(DMwR)
actuals <- complete.k1$stock
predicteds <- rep(median(kakao.new2$stock, na.rm=T), length(actuals))
regr.eval(actuals, predicteds)# 1.016338e+09

actuals2 <- complete.n1$stock
predicteds2 <- rep(median(naver.new2$stock, na.rm=T), length(actuals2))
regr.eval(actuals2, predicteds2) # 9.593761e+10
#median보다는 mean이 나음. 


#이전 값으로 대체 
kakao.new.11<-complete.k ;naver.new.11<-complete.n
kakao.new.11[1,"stock"]<-47350
for(i in 1:nrow(kakao.new.11)){
  if(is.na(kakao.new.11[i,"stock"])){
    kakao.new.11[i,"stock"]<-kakao.new.11[i-1,"stock"]
  }
}
actuals <- complete.k1$stock
regr.eval(actuals, kakao.new.11$stock) # 3.492993e+06

naver.new.11[5,"stock"]<-339850
for(i in 1:nrow(naver.new.11)){
  if(is.na(naver.new.11[i,"stock"])){
    naver.new.11[i,"stock"]<-naver.new.11[i-1,"stock"]
  }
}
actuals <- complete.n1$stock
regr.eval(actuals, naver.new.11$stock) # 4.797415e+07


#rpart
library(rpart)
kakao.new4 = complete.k ; naver.new4=complete.n
anova_mod1 <- rpart(stock~., data=kakao.new4[!is.na(kakao.new4$stock), ], method="anova", na.action=na.omit)  # since ptratio is numeric.
start_pred1 <- predict(anova_mod1, kakao.new4[is.na(kakao.new4$stock), ])
names(start_pred1) <- 1:1113

actuals <- complete.k1$stock[is.na(complete.k$stock)]
regr.eval(actuals, start_pred1) #9.173032e+07

anova_mod2 <- rpart( stock~., data=naver.new4[!is.na(naver.new4$stock), ], method="anova", na.action=na.omit)  # since ptratio is numeric.
start_pred2 <- predict(anova_mod2, naver.new4[is.na(naver.new4$stock), ])
names(start_pred2) <- 1:1038

actuals2 <- complete.n1$stock[is.na(complete.n$stock)]
regr.eval(actuals2, start_pred2) # 3.930275e+09 

#knn
library(caret)
kakao.new5 = complete.k ; naver.new5=complete.n
myK = sum(apply(kakao.new5, 1, function(r) all(!is.na(r))))
a<-data.frame(kakao.new5[, -1]) ; colnames(a)<-"stock"
knnOutput.kakao <- preProcess(a, method = c("knnImpute"), k = 10)
anyNA(knnOutput.kakao)
a2<-data.frame(kakao.new5[complete.cases(kakao.new5),-1 ]) ; colnames(a2)<-"stock"
t_imp <- predict(knnOutput.kakao, a2)
regr.eval(actuals2, t_imp) # 3.425488e+11

a<-data.frame(naver.new5[, -1]) ; colnames(a)<-"stock"
myK = sum(apply(naver.new5, 1, function(r) all(!is.na(r))))
knnOutput.naver <- preProcess(a, method = c("knnImpute"), k = myK)
anyNA(knnOutput.naver)
a2<-data.frame(naver.new5[complete.cases(naver.new5),-1 ]) ; colnames(a2)<-"stock"
t_imp2 <- predict(knnOutput.naver, a2)
regr.eval(actuals2, t_imp2) #4.579648e+11

#MICE
library(mice)
kakao.new6 = complete.k ; naver.new6=complete.n
kakao.new6[,"dummy"]<-1 ; naver.new6[,"dummy"]<-1
miceMod <- mice(data.frame(kakao.new6[, !names(kakao.new6) %in% "date"]), method="rf")  # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)

actuals <- complete.k1$stock[is.na(complete.k$stock)]
predicteds <- miceOutput[is.na(kakao.new6$stock), "stock"]
regr.eval(actuals, predicteds) #2.096798e+09

miceMod <- mice(data.frame(naver.new6[, !names(naver.new6) %in% "date"]), method="rf")  # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)

actuals <- complete.n1$stock[is.na(complete.n$stock)]
predicteds <- miceOutput[is.na(naver.new6$stock), "stock"]
regr.eval(actuals, predicteds) #1.525380e+11

##Rpart가 가장 좋기 때문에 rpart로 대체 


#rpart final-------------------------------------------------------------------------------------
library(rpart)
anova_mod1 <- rpart(stock~., data=kakao.new[!is.na(kakao.new$stock), ], method="anova", na.action=na.omit)  # since ptratio is numeric.
pred<- predict(anova_mod1, miss.k)
names(pred) <- 1:1784
final_kakao=kakao.new
final_kakao[is.na(final_kakao$stock),"stock"]<-pred
nrow(final_kakao)

anova_mod2 <- rpart( stock~., data=naver.new[!is.na(naver.new$stock), ], method="anova", na.action=na.omit)  # since ptratio is numeric.
pred <- predict(anova_mod2, miss.n)
names(pred) <- 1:1535

final_naver = naver.new
final_naver[is.na(final_naver$stock),"stock"]<-pred
nrow(final_naver)

# write.csv(as.matrix(final_kakao),"kakao_final.csv")
# write.csv(as.matrix(final_naver),"naver_final.csv")
# 


kakao.new_1<-kakao.new
for(i in 1:nrow(kakao.new_1)){
  if(is.na(kakao.new_1[i,"stock"])){
    kakao.new_1[i,"stock"]<-kakao.new_1[i-1,"stock"]
  }
}
naver.new_1<-naver.new
for(i in 1:nrow(naver.new_1)){
  if(is.na(naver.new_1[i,"stock"])){
    naver.new_1[i,"stock"]<-naver.new_1[i-1,"stock"]
  }
}
write.csv(as.matrix(kakao.new_1),"kakao_FINAL2.csv")
write.csv(as.matrix(naver.new_1),"naver_FINAL2.csv")

