library(readr)
setwd("C:/Users/user/Desktop/python/datamining/음악체크유/금요웹툰")
#웹툰 이름	24시간 댓글수	총 댓글 수 	별점	별점 참여자수 	회차 수
#dir<-list.dirs()
#컷툰 작가 불러오기 
src_dir = "C:/Users/user/Desktop/python/datamining/컷툰작가"
wr_dir<- list.files(src_dir) ; wr_dir

dataset<-list()
for(i in 1:length(wr_dir))  { 
    dataset[[i]] <-read.csv(paste0(src_dir,"/",wr_dir[i],"/","cut_writer.csv"),header = FALSE)
    colnames(dataset[[i]])<-c("title","num","date","time","cut_num","wr_talk")
}
# for(i in 1:length(wr_dir))  { 
# for(j in 1:nrow(dataset[[i]])){
#     dataset[[i]][j,"ttime"]=data.frame(as.POSIXlt(paste(as.Date(dataset[[i]][j,"date"], format = "%Y.%m.%d"),dataset[[i]][j,"time"])),origin='1970-01-01')
#   }
# }
# for(i in 1:length(wr_dir))  { 
#   write.csv(dataset[[i]][,"ttime"],paste0(src_dir,"/",wr_dir[i],"/","ttime.csv"))
# }
f1 <- list.files() ; f1;length(f1) #title 
f2<-list() #file 각각 데이터 
for (i in 1: length(f1)){
f2[[i]] = list.files(f1[i],pattern="*.csv");
}
# f_cnt<-vector() #회차수 
# for(i in 1: length(f1)){
# f_cnt[i] <- length(f2[[i]])
# }
#cut_title : dataset[[1]][1,"title"]
#title : strsplit(f1[1],"_")[[1]][2]
#날짜 : dataset2[c(4:nrow(dataset2)-1),1] , 시간 : dataset2[c(4:nrow(dataset2)-1),2]
#total comments : dataset2[nrow(dataset2),2]
#star : dataset2[2,1] starperson : dataset2[2,2]
# dataset2 <-read.csv(paste0(f1[1],"/", f2[[1]][1]),header=FALSE)
# difftime(as.POSIXct(as.numeric(as.POSIXct(dataset[[1]][1,"ttime"],origin='1970-01-01')),origin='1970-01-01'),as.POSIXlt(paste(dataset2[4,1],dataset2[4,2]),origin='1970-01-01'),units="hour")


for(j in 1: length(dataset)){
  dataset[[j]]=na.omit(dataset[[j]])
}

for(j in 1:length(dataset)){
  for(i in 1: length(f1)){
      if(dataset[[j]][1,"title"]==strsplit(f1[i],"_")[[1]][2]){
        print(dataset[[j]][1,"title"])
        for(k in 1: length(f2[[i]])){
          dataset2 <-read.csv(paste0(f1[i],"/", f2[[i]][k]),header=FALSE)
          for( q in 1: nrow(dataset[[j]])){
           if(as.character(dataset[[j]][q,"date"])==as.character(dataset2[1,2])){
             cnt =0 
             for( h in 4: (nrow(dataset2)-1)){
               cnt=cnt+ifelse(as.integer(difftime(as.POSIXct(paste(as.Date(dataset[[j]][q,"date"], format = "%Y.%m.%d"),dataset[[j]][q,"time"]),origin='1970-01-01'),as.POSIXlt(paste(dataset2[h,1],dataset2[h,2]),origin='1970-01-01'),units="mins"))<=0
                        & as.integer(difftime(as.POSIXct(paste(as.Date(dataset[[j]][q,"date"], format = "%Y.%m.%d"),dataset[[j]][q,"time"]),origin='1970-01-01'),as.POSIXlt(paste(dataset2[h,1],dataset2[h,2]),origin='1970-01-01'),units="mins"))>=-1440,1,0)
             }
             print(cnt)
             dataset[[j]][q,"reply"] = cnt 
           }
          }
        }
        rm(dataset2)
      }
    write.csv(dataset[[j]],paste0(src_dir,"/",wr_dir[j],"/","twentyfour.csv"))
}
}




