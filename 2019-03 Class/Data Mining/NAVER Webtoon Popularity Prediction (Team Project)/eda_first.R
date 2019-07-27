setwd("C:/Users/user/Desktop/데마 팀플")
library(tidyverse) ; library(caret) ; library(readr) ; library(dplyr) ;library(readxl)
library(lubridate);library(plotly);library(ggthemes) ; library(ggplot2) ;library(data.table) ;library(gridExtra)
memory.limit(size=56000) #memory increased  to 7GB
#data-----------------------------------------------------------------------------------------
total<-read_excel("final_dm_eng.xlsx",sheet = 1, col_names = TRUE)
##format manipulation-----------------------------------------------------------------------------
total$upload_date=as.Date(total$upload_date);total$upload_next_date=as.Date(total$upload_next_date)
#total$story_debut=as.Date(total$story_debut);total$picture_debut=as.Date(total$story_debut)
total$upload_time=format(as.POSIXct(total$upload_time), "%H:%M:%S")
###factor: new, writer_talk, music, cut_toon, age, month, year, daily~sports, episode~story,day
total$name = as.factor(total$name) ; 

total$new = as.factor(total$new);total$writer_talk = as.factor(total$writer_talk);total$music = as.factor(total$music);total$cut_toon = as.factor(total$cut_toon);
total$age = as.factor(total$age);total$month = as.factor(total$month);total$year = as.factor(total$year);

total$daily= as.factor(total$daily);total$thriller= as.factor(total$thriller);total$historical= as.factor(total$historical);total$sports= as.factor(total$sports);
total$comic= as.factor(total$comic);total$action= as.factor(total$action);total$episode= as.factor(total$episode);total$drama= as.factor(total$drama);
total$fantasy= as.factor(total$fantasy);total$drama= as.factor(total$drama);total$genuine= as.factor(total$genuine);total$omnibus= as.factor(total$omnibus);
total$story= as.factor(total$story);total$emotion= as.factor(total$emotion);total$day= as.factor(total$day);

glimpse(total)

#1.eda------------------------------------------------------------------------------
##첫회 댓글수 
time_series = read.csv("time_series_data.csv")
total_name = unique(time_series$name) ; total_length = vector() ; each<-list() #each webtoon
for(i in 1:length(total_name)){
      total_length[i]<-nrow(total[which(total$name==total_name[i]),])
      each[[i]]<-total[which(total$name==total_name[i]),]
}
name_length <-cbind(as.character(total_name),total_length)

##24시간 프롤로그 댓글수  & total에 추가 
first_reply<-vector()
for(i in 1:length(total_name)){
  first_reply[i]<-each[[i]][total_length[i],"comment_24hour"]  
  total[which(total$name==total_name[i]),"prolog_comment"]<-first_reply[i]
}
name_length<-cbind(name_length,first_reply)
write.csv(as.matrix(name_length),"name_length.csv")

##prolog 기간 , 데뷔 기간, 스토리 기간 & total에 추가 
prolog_date<-vector()
for(i in 1: length(each)){
  for(j in 1: nrow(each[[i]])){
    each[[i]][j,'prolog_date']<-as.numeric(each[[i]][j,'upload_date']-each[[i]][total_length[i],'upload_date'])
  }
  total[which(total$name==total_name[i]),"prolog_interval"]<-each[[i]][,'prolog_date']
}

for(i in 1: length(each)){
  for(j in 1: nrow(each[[i]])){
    each[[i]][j,'story_debut2']<-as.numeric(total[1,5]-each[[i]][j,'story_debut'])
    each[[i]][j,'picture_debut2']<-as.numeric(total[1,5]-each[[i]][j,'picture_debut'])
  }
  total[which(total$name==total_name[i]),"story_debut2"]<-each[[i]][,'story_debut2']
  total[which(total$name==total_name[i]),"picture_debut2"]<-each[[i]][,'picture_debut2']
}


total<-total[,-c(44:49)]
colnames(total)[46] <-"story_debut" ;colnames(total)[47] <-"picture_debut"

write.csv(as.matrix(total),"final_total.csv")

##total data------------------------------------------------------
setwd("C:/Users/user/Desktop/데마 팀플")
library(tidyverse) ; library(caret) ; library(readr) ; library(dplyr) ;library(readxl)
library(lubridate);library(plotly);library(ggthemes) ; library(ggplot2) ;library(data.table) ;library(gridExtra)
memory.limit(size=56000) #memory increased  to 7GB
total <-read.csv("total_final_v3.csv") ; total<-total[,-1]
total$upload_date=as.Date(total$upload_date);total$upload_next_date=as.Date(total$upload_next_date)
#total$upload_time=format(as.POSIXct(total$upload_time), "%H:%M:%S")
###factor: new, writer_talk, music, cut_toon, age, month, year, daily~sports, episode~story,day
total$name = as.factor(total$name)
total$new = as.factor(total$new);total$writer_talk = as.factor(total$writer_talk);total$music = as.factor(total$music);total$cut_toon = as.factor(total$cut_toon);
total$age = as.factor(total$age);total$month = as.factor(total$month);total$year = as.factor(total$year);
total$daily= as.factor(total$daily);total$thriller= as.factor(total$thriller);total$historical= as.factor(total$historical);total$sports= as.factor(total$sports);
total$comic= as.factor(total$comic);total$action= as.factor(total$action);total$episode= as.factor(total$episode);total$drama= as.factor(total$drama);
total$fantasy= as.factor(total$fantasy);total$drama= as.factor(total$drama);total$genuine= as.factor(total$genuine);total$omnibus= as.factor(total$omnibus);
total$story= as.factor(total$story);total$emotion= as.factor(total$emotion);total$day= as.factor(total$day);
##each webtoon------------------------------------------------
total_name = unique(total$name) ; total_length = vector() ; each<-list() #each webtoon
for(i in 1:length(total_name)){
  total_length[i]<-nrow(total[which(total$name==total_name[i]),])
  each[[i]]<-total[which(total$name==total_name[i]),]
}
name_length <-cbind(as.character(total_name),total_length)


# for(i in 1: length(each)){
#   dir.create(file.path(paste0("webtoon_timeseries/",name_length[i,1])), recursive = TRUE)
#   write.csv(as.matrix(each[[i]]),paste0("webtoon_timeseries/",name_length[i,1],"/timeseries.csv"))
#   
# }

#15 : 신의 탑 
View(each)
max(total$total_comment)

sum(total_length[1:14])
#1739+326
total<-total[-2065,] #신의 탑 outlier 제거 

#na발견 값들 
total[which(is.na(total$age)),"age"]<-0
total[which(is.na(total$music)),"music"]<-0

write.csv(as.matrix(total),"total_final_v2.csv")
#plot---------------------------------------------------------------------------------------------
#par(mfrow=c(1,1)) 


total$comment_24hour<-log(total$comment_24hour+1)
total$cut_number<-log(total$cut_number+1)
total$naver_stock<-log(total$naver_stock)
total$kakao_stock<-log(total$kakao_stock)
total$prolog_interval<-log(total$prolog_interval+1)
total$story_debut<-log(total$story_debut)

dfplot2 <- function(data.frame,y,z,dir)
{
  df <- data.frame
  ln <- length(colnames(data.frame))
  for(i in 1:ln){
    mname <- substitute(df[,i])
    if(is.factor(df[,i])){
      ggplot(total,aes_string(x=names(df)[i],y=y,fill=names(df)[i]))+
        guides(fill=FALSE) +
        geom_boxplot(width=0.3,alpha=0.3,outlier.size = 2)+
       scale_y_continuous(limits = quantile(total$comment_24hour, c(0.1, 0.97)))+ # 수정 
        ggtitle(paste(names(df)[i]," vs ","log",y))+theme_light()      
      ggsave(filename=paste0(dir,names(df)[i],".jpg"))
    }
    else{
      ggplot(df,aes_string(x=names(df)[i],y=y,col=z))+geom_point(size = 2,show.legend=F)+
        ggtitle(paste(names(df)[i]," vs ","log",y))+theme_light()
      ggsave(filename=paste0(dir,names(df)[i],".jpg"))
      }
    
  }
}

dfplot2(total[,-c(2,4,5)],"total_comment","name","plot_totalcomment/")
dev.off()

dfplot2(total[,-c(2,4,5)],"comment_24hour","name","plot2_24hour/")
dev.off()

dfplot2(total[,-c(2,4,5)],"grade","name","plot3_star/")
dev.off()

dfplot2(total[,-c(2,4,5)],"grade_parti","name","plot4_starperson/")
dev.off()
# ggplot(total,aes_string(x=names(total)[8],y="total_comment",fill=names(total)[8]))+
#   guides(fill=FALSE) +
#   geom_boxplot(width=0.3,alpha=0.3,outlier.size = 2)+
#   scale_y_continuous(limits = quantile(total$total_comment, c(0.1, 0.97)))+
#   ggtitle(paste(names(total)[8]," vs ",y="total_comment"))+theme_light()


dfplot3<-function(df,y,dir){
  for(i in 1: length(df)){
    ggplot(df[[i]])+geom_line(aes_string(x="upload_date",y=y),color="black")+
      theme_light()+ggtitle(df[[i]]$name[1])
    ggsave(filename=paste0(dir,df[[i]]$name[1],".jpg"))
  }
}
dfplot3(each,"total_comment","webtoon_plot/plot_totalcomment/")
dev.off()

dfplot3(each,"comment_24hour","webtoon_plot/plot2_24hour/")
dev.off()

dfplot3(each,"grade","name","webtoon_plot/plot3_star/")
dev.off()

dfplot3(each,"grade_parti","webtoon_plot/plot4_starperson/")
dev.off()
# 

total$writer_talk = as.numeric(total$writer_talk);total$cut_toon = as.numeric(total$cut_toon)
total2$music = as.numeric(total2$music);total2$age = as.numeric(total2$age)

total2 =aggregate(total[,c(1,3,7:18,21:42,44:47)], list(total$name), mean)
total2=total2[,-2];total2 = total2[,-6];total2 = total2[,-c(6,7)];total2 = total2[,-37]
lm1=lm(comment_24hour~.,data=total[,-c(36:42)])
plot(lm1)
summary(lm1)

for(i in 1:ncol(total))total[,i]<-as.numeric(total[,i])
data.rcorr = cor(as.matrix(total[,-c(36:42)]))
heatmap(total[,-c(36:42)]%>%as.matrix())
corrplot::corrplot(data.rcorr)
library(PerformanceAnalytics)
pairs(total,
                  method="pearson",
                  histogram=TRUE,
                 pch=16)

plot_density(total)
