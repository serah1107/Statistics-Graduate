library(lattice)
library(mvtnorm)
##function------------------------------------------------------
get.dist<-function(N,p){
  x <- rmvnorm(N, mean = rep(0,p), sigma = diag(p))
  x <- t(apply(x, 1, function(z){runif(1)^(1/p)*z/sqrt(sum(z^2))}))
  res <- apply(x,1,function(z){sqrt(sum(z^2))})
  ifelse(p==1, min(x^2),min(res))
}
#generate p-dim r.v from normal distribution
#generate N nearest distances 
#N is sample size and p is dimension size.

##simulation---------------------------------------------------
med_table<-list() #sim.median list
error_table<-list() #error table list(squared bias, var, mse)
plot_list<-list() #error plot list

sim.n=100
sim.med<-matrix(0,nrow=5,ncol=1);rownames(sim.med) <- c("N=20","N=50", "N=100","N=500","N=1000")
sim.error<-matrix(0,nrow=5,ncol=3);rownames(sim.error) <- c("N=20","N=50", "N=100","N=500","N=1000")
colnames(sim.error) <- c("squared.bias","variance", "mse")
#sim.med : matrix for calculating median distances
#sim.error : matrix for calculating error

pp=1
for (p in c(2,3,5,10,20,30,50,100)){
  nn=1
  temp<-matrix(0,nrow=1,ncol=sim.n) 
  for (N in c(20,50,100,500,1000)){
      for ( i in 1:sim.n){
        dist1<-get.dist(N,p) #generate r.v's
        temp[1,i]<-dist1 #generate distances
      }
    sim.med[nn,1]<-median(temp[1,]) # median distance
    sim.error[nn,1]<-mean((temp[1,]-1)^2) # sq.bias
    sim.error[nn,2]<-var(temp[1,]) # variance
    sim.error[nn,3]<-sim.error[nn,1]+sim.error[nn,2] # variance
    
    med_table[[pp]]<-sim.med
    error_table[[pp]]<-sim.error
    nn<-nn+1 ; #parameter N
  }
  pp<-pp+1 #parameter p
}

##true distance-----------------------------------------------------------
pp=1
true_table<-list() #true value list
true<-matrix(0,nrow=5,ncol=1);rownames(true) <- c("N=20","N=50", "N=100","N=500","N=1000")
for (p in c(2,3,5,10,20,30,50,100)){
  nn=1
  for (N in c(20,50,100,500,1000)){
      true[nn,1]<-(1-0.5^(1/N))^(1/p)
      true_table[[pp]] <- true
      nn<-nn+1
  }
  pp<-pp+1
}

##result-------------------------------------------------------------------
View(med_table)
View(true_table)

##error graph-------------------------------------------------------
table_n1<-data.frame(0,nrow=8,ncol=3) ;colnames(table_n1)<-c("squared bias","variance","mse")
table_n2<-data.frame(0,nrow=8,ncol=3);colnames(table_n2)<-c("squared bias","variance","mse")
table_n3<-data.frame(0,nrow=8,ncol=3);colnames(table_n3)<-c("squared bias","variance","mse")
table_n4<-data.frame(0,nrow=8,ncol=3);colnames(table_n4)<-c("squared bias","variance","mse")
table_n5<-data.frame(0,nrow=8,ncol=3);colnames(table_n5)<-c("squared bias","variance","mse")

for (i in 1:8){
  table_n1[i,]<-error_table[[i]][1,] #N=20
  table_n2[i,]<-error_table[[i]][2,] #N=50
  table_n3[i,]<-error_table[[i]][3,] #N=100
  table_n4[i,]<-error_table[[i]][4,] #N=500
  table_n5[i,]<-error_table[[i]][5,] #N=1000
}
p=c(2,3,5,10,20,30,50,100)

###N=20 graph
plot(p,table_n1$`squared bias`,type="l",ylab = "error",main="Error comparison (N=20,sim.n=100)")
lines(p,table_n1$variance,col="red")
lines(p,table_n1$mse,col="blue")

###N=50 graph
plot(p,table_n2$`squared bias`,type="l",ylab = "error",main="Error comparison (N=50,sim.n=100)")
lines(p,table_n2$variance,col="red")
lines(p,table_n2$mse,col="blue")

###N=100 graph
plot(p,table_n3$`squared bias`,type="l",ylab = "error",main="Error comparison (N=100,sim.n=100)")
lines(p,table_n3$variance,col="red")
lines(p,table_n3$mse,col="blue")

###N=500 graph
plot(p,table_n4$`squared bias`,type="l",ylab = "error",main="Error comparison (N=500,sim.n=100)")
lines(p,table_n4$variance,col="red")
lines(p,table_n4$mse,col="blue")

###N=1000 graph
plot(p,table_n5$`squared bias`,type="l",ylab = "error",main="Error comparison (N=1000,sim.n=100)")
lines(p,table_n5$variance,col="red")
lines(p,table_n5$mse,col="blue")
