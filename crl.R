library(acepack)

maxCor<-function(x,y,method){
  fxy<-ace(x,y)
  r<-cor(fxy$tx,fxy$ty,method=method)
  return(r)
}

n<-100
x<-rnbinom(n,5,0.7)
y<-sin(x)+rpois(n,10)
plot(x,y)

simFunc<-function(k,n,alpha,distribution,method){
  v<-v1<-c()
  for(i in 1:k){
    if(distribution=='normal'){
      v[i]<-maxCor(rnorm(n,0,1),rnorm(n,0,1),method)
      x<-rnorm(n,0,0.5)
      y<-sin(x)+rnorm(n,0,0.5)
      v1[i]<-maxCor(x,y,method)
    }
    if(distribution=='uniform'){
      v[i]<-maxCor(runif(n,0,1),runif(n,0,1),method)
      x<-runif(n,0,1)
      y<-sin(x)+rnorm(n,0,0.5)
      v1[i]<-maxCor(x,y,method)
    }
    if(distribution=='exponential'){
      v[i]<-maxCor(rexp(n,1.5),rexp(n,1.5),method)
      x<-rexp(n,1.5)
      y<-sin(x)+rnorm(n,0,0.5)
      v1[i]<-maxCor(x,y,method)
    }
    if(distribution=='gamma'){
      v[i]<-maxCor(rgamma(n,0.5,1.5),rgamma(n,0.5,1.5),method)
      x<-rgamma(n,0.5,1.5)
      y<-sin(x)+rnorm(n,0,0.5)
      v1[i]<-maxCor(x,y,method)
    }
    if(distribution=='poisson'){
      v[i]<-maxCor(rpois(n,1),rpois(n,1),method)
      while(is.na(v[i])){
        v[i]<-maxCor(rpois(n,1),rpois(n,1),method)
      }
      x<-rpois(n,1)
      y<-sin(x)+rpois(n,10)
      v1[i]<-maxCor(x,y,method)
      while(is.na(v1[i])){
        x<-rpois(n,1)
        y<-sin(x)+rpois(n,10)
        v1[i]<-maxCor(x,y,method)
      }
    }
    if(distribution=='hyper'){
      v[i]<-maxCor(rhyper(n,10,10,15),rhyper(n,10,10,15),method)
      while(is.na(v[i])){
        v[i]<-maxCor(rhyper(n,10,10,15),rhyper(n,10,10,15),method)
      }
      x<-rhyper(n,10,10,15)
      y<-sin(x)+rpois(n,10)
      v1[i]<-maxCor(x,y,method)
      while(is.na(v1[i])){
        x<-rhyper(n,10,10,15)
        y<-sin(x)+rpois(n,10)
        v1[i]<-maxCor(x,y,method)
      }
    }
    if(distribution=='nbinom'){
      v[i]<-maxCor(rnbinom(n,3,0.7),rnbinom(n,3,0.7),method)
      while(is.na(v[i])){
        v[i]<-maxCor(rnbinom(n,3,0.7),rnbinom(n,3,0.7),method)
      }
      x<-rnbinom(n,3,0.7)
      y<-sin(x)+rpois(n,10)
      v1[i]<-maxCor(x,y,method)
      while(is.na(v1[i])){
        x<-rnbinom(n,3,0.7)
        y<-sin(x)+rpois(n,10)
        v1[i]<-maxCor(x,y,method)
      }
    }
  }
  critV<-as.numeric(quantile(v,probs=1-alpha))
  power<-sum(v1>=critV)/k
  return(power)
}
k<-1000
alpha<-0.01
df<-as.data.frame(matrix(NA,10,8))
for(i in 1:10){
  n<-10*i
  df[i,1]<-simFunc(k,n,alpha,'normal','pearson')
  df[i,2]<-simFunc(k,n,alpha,'normal','spearman')
  df[i,3]<-simFunc(k,n,alpha,'uniform','pearson')
  df[i,4]<-simFunc(k,n,alpha,'uniform','spearman')
  df[i,5]<-simFunc(k,n,alpha,'exponential','pearson')
  df[i,6]<-simFunc(k,n,alpha,'exponential','spearman')
  df[i,7]<-simFunc(k,n,alpha,'gamma','pearson')
  df[i,8]<-simFunc(k,n,alpha,'gamma','spearman')
  print(i)
}
print(df)
write.csv(df,file='df0.csv')

df<-read.csv(file='df0.csv')[1:10,2:9]
ltypes<-c(1,4,1,4,1,4,1,4)
ptypes<-c(1,8,1,8,1,8,1,8)
cols<-c('black','gray','blue','steelblue')
plot(seq(10,100,by=10),df[,1],type='l',lty=ltypes[1],
     xlim=c(10,100),ylim=c(0,1),col=cols[1],
     xlab='',ylab='')
points(seq(10,100,by=10),df[,1],pch=ptypes[1],col=cols[1])
for(i in 2:8){
  col=cols[c(1,1,2,2,3,3,4,4)[i]]
  points(seq(10,100,by=10),df[,i],type='l',lty=ltypes[i],col=col)
  points(seq(10,100,by=10),df[,i],pch=ptypes[i],col=col)
}
legend('topleft',pch=c(1,8,NA,NA,NA,NA),lty=c(1,4,1,1,1,1),
       col=c('black','black','black','gray','blue','steelblue'),
       legend=c('Pearson','Spearman','normal','uniform',
                'exponential','gamma'))

df<-as.data.frame(matrix(NA,10,6))
for(i in 1:10){
  n<-10*i
  df[i,1]<-simFunc(k,n,alpha,'poisson','pearson')
  df[i,2]<-simFunc(k,n,alpha,'poisson','spearman')
  df[i,3]<-simFunc(k,n,alpha,'hyper','pearson')
  df[i,4]<-simFunc(k,n,alpha,'hyper','spearman')
  df[i,5]<-simFunc(k,n,alpha,'nbinom','pearson')
  df[i,6]<-simFunc(k,n,alpha,'nbinom','spearman')
  print(i)
}
print(df)
write.csv(df,file='df1.csv')

df<-read.csv(file='df1.csv')[1:10,2:7]
ltypes<-c(1,4,1,4,1,4)
ptypes<-c(1,8,1,8,1,8)
cols<-c('black','gray','blue')
plot(seq(10,100,by=10),df[,1],type='l',lty=ltypes[1],
     xlim=c(10,100),ylim=c(0,1),col=cols[1],
     xlab='',ylab='')
points(seq(10,100,by=10),df[,1],pch=ptypes[1],col=cols[1])
for(i in 2:6){
  col=cols[c(1,1,2,2,3,3)[i]]
  points(seq(10,100,by=10),df[,i],type='l',lty=ltypes[i],col=col)
  points(seq(10,100,by=10),df[,i],pch=ptypes[i],col=col)
}
legend('topleft',pch=c(1,8,NA,NA,NA),lty=c(1,4,1,1,1),
       col=c('black','black','black','gray','blue'),
       legend=c('Pearson','Spearman','Poisson','hypergeo.',
                'neg. binom.'))

df<-CT_PET_Clin_Survival_v1_Octavio_event_types
df
colnames(df)
dim(df)

replaceZeros<-function(v){
  for(i in 1:length(v)){
    if(v[i]==0){
      v[i]<-NA
    }
  }
  return(v)
}

f<-function(x,y,k,alpha,method){
  x<-as.numeric(x)
  y<-as.numeric(y)
  dxy<-cbind(x,y)
  dxy<-as.data.frame(dxy)
  dxy<-na.omit(dxy)
  c<-maxCor(dxy$x,dxy$y,method)
  v<-c()
  s<-0
  for(i in 1:k){
    y1<-sample(dxy$y,length(dxy$y),replace=FALSE)
    v[i]<-maxCor(dxy$x,y1,method)
    if(v[i]<c){
      s=s+1/k
    }
  }
  critV<-as.numeric(quantile(v,probs=1-alpha))
  return(c(c,critV,1-s))
}

y<-df$STR_1
k<-10000
alpha<-0.01

f(replaceZeros(df$LADC_Athero),y,k,alpha,'pearson')
f(replaceZeros(df$LADC_Athero),y,k,alpha,'spearman')
f(replaceZeros(df$LADC_.Steno),y,k,alpha,'pearson')
f(replaceZeros(df$LADC_.Steno),y,k,alpha,'spearman')
f(replaceZeros(df$LADC_Calc),y,k,alpha,'pearson')
f(replaceZeros(df$LADC_Calc),y,k,alpha,'spearman')

f(replaceZeros(df$LCXB_Athero),y,k,alpha,'pearson')
f(replaceZeros(df$LCXB_Athero),y,k,alpha,'spearman')
f(replaceZeros(df$LCXB_.Steno),y,k,alpha,'pearson')
f(replaceZeros(df$LCXB_.Steno),y,k,alpha,'spearman')
f(replaceZeros(df$LCXB_Calc),y,k,alpha,'pearson')
f(replaceZeros(df$LCXB_Calc),y,k,alpha,'spearman')

f(replaceZeros(df$LPL_Athero),y,k,alpha,'pearson')
f(replaceZeros(df$LPL_Athero),y,k,alpha,'spearman')
f(replaceZeros(df$LPL_.Steno),y,k,alpha,'pearson')
f(replaceZeros(df$LPL_.Steno),y,k,alpha,'spearman')
f(replaceZeros(df$LPL_Calc),y,k,alpha,'pearson')
f(replaceZeros(df$LPL_Calc),y,k,alpha,'spearman')

f(replaceZeros(df$RPL_Athero),y,k,alpha,'pearson')
f(replaceZeros(df$RPL_Athero),y,k,alpha,'spearman')
f(replaceZeros(df$RPL_.Steno),y,k,alpha,'pearson')
f(replaceZeros(df$RPL_.Steno),y,k,alpha,'spearman')
f(replaceZeros(df$RPL_Calc),y,k,alpha,'pearson')
f(replaceZeros(df$RPL_Calc),y,k,alpha,'spearman')
