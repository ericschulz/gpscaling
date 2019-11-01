#GP Scaling: Ishita Dasgupta & Eric Schulz

#RBF kernel
rbf <- function(X1,X2,l=1) {
  Sigma <- matrix(rep(0, length(X1)*length(X2)), nrow=length(X1))
  for (i in 1:nrow(Sigma)) {
    for (j in 1:ncol(Sigma)) {
      Sigma[i,j] <- exp(-0.5*(abs(X1[i]-X2[j])/l)^2)
    }
  }
  return(Sigma)
}

#MASS for multivariate normal
library(MASS)

#input space
x.star <- seq(-5,5,len=101)
#sigma
sigma.n <- 0.1

#all combinations of lambdas and N
dd<-expand.grid(n=seq(10,30,2), ltrue=seq(0.5, 5, 0.5), lwrong=seq(0.5, 5, 0.5))
#exclude equal lambdas
dd<-subset(dd, ltrue!=lwrong)
#loop through
for (i in 1:nrow(dd)){
  #true lambda
  ltrue<-dd$ltrue[i]
  #wrong lambda
  lwrong<-dd$lwrong[i]
  #sample size n
  n<-dd$n[i]
  #sort x
  x<-sort(sample(x.star, n))
  #generate true function
  yobs<- mvrnorm(1, rep(0, length(x.star)), rbf(x.star,x.star,l=ltrue))
  #get observations for x
  yobs<-yobs[x.star %in% x]
  #add noise N(0,0.25)
  yobs<-yobs+rnorm(length(yobs), 0, sigma.n)
  #calculate required matrices for true lambda
  k.xx <- rbf(x,x, ltrue)
  k.xsx <- rbf(x.star,x, ltrue)
  #get predition for true lambda
  ftrue <- k.xsx%*%solve(k.xx + sigma.n^2*diag(1, ncol(k.xx)))%*%yobs
  #calculate required lambdas for wrong lambda
  k.xx <- rbf(x,x, lwrong)
  k.xsx <- rbf(x.star,x, lwrong)
  #get prediction for wrong lambda
  fwrong <- k.xsx%*%solve(k.xx + sigma.n^2*diag(1, ncol(k.xx)))%*%yobs
  #min and max for plotting
  ma<-max(c(yobs,ftrue,fwrong))
  mi<-min(c(yobs,ftrue,fwrong))
  #true prediction
  png(paste0("pngs/true", dd$ltrue[i]*10,"lw", dd$lwrong[i]*10, "n", dd$n[i], ".png"),
      width=300, height=300)
  plot(x, yobs, pch=19,  xaxt='n',
       ann=FALSE, yaxt='n', ylim=c(mi,ma))
  lines(x.star, ftrue, col="green", lwd=2)
  dev.off()
  #wrong prediction
  png(paste0("pngs/wrong", dd$ltrue[i]*10,"lw", dd$lwrong[i]*10, "n", dd$n[i], ".png"),
      width=300, height=300)
  plot(x, yobs, pch=19,  xaxt='n',
       ann=FALSE, yaxt='n',ylim=c(mi,ma))
  lines(x.star, fwrong, col="green", lwd=2)
  dev.off()
  
  #collect data
  if(i==1){
    dat<-data.frame(x, yobs,
                    lt=rep(dd$ltrue[i], length(x)),
                    lw=rep(dd$lwrong[i], length(x)),
                    n=rep(dd$n[i], length(x)))
  } else{
    dat<-rbind(dat, data.frame(x, yobs,
                               lt=rep(dd$ltrue[i], length(x)),
                               lw=rep(dd$lwrong[i], length(x)),
                               n=rep(dd$n[i], length(x))))
  }
}
#save data
write.csv(dat, "exp1functions.csv")


l1<-0.5
l2<-3.5
l3<-5
n<-20
x<-sort(sample(x.star, n))
#generate true function
yobs<- mvrnorm(1, rep(0, length(x.star)), rbf(x.star,x.star,l=l2))
#get observations for x
yobs<-yobs[x.star %in% x]
#add noise N(0,0.25)
yobs<-yobs+rnorm(length(yobs), 0, sigma.n)
#calculate required matrices for true lambda
k.xx <- rbf(x,x, l1)
k.xsx <- rbf(x.star,x, l1)
#get predition for true lambda
f1 <- k.xsx%*%solve(k.xx + sigma.n^2*diag(1, ncol(k.xx)))%*%yobs
#calculate required lambdas for wrong lambda
k.xx <- rbf(x,x, l2)
k.xsx <- rbf(x.star,x, l2)
#get prediction for wrong lambda
f2<- k.xsx%*%solve(k.xx + sigma.n^2*diag(1, ncol(k.xx)))%*%yobs
k.xx <- rbf(x,x, l3)
k.xsx <- rbf(x.star,x, l3)
#get prediction for wrong lambda
f3<- k.xsx%*%solve(k.xx + sigma.n^2*diag(1, ncol(k.xx)))%*%yobs

#min and max for plotting
ma<-max(c(yobs,f1,f2,f3))
mi<-min(c(yobs,f1,f2,f3))
png("example3.png", width=900, height=300)
par(mfrow=c(1,3))
plot(x, yobs, pch=19,  xaxt='n',xlab="", ylab="",
     yaxt='n',ylim=c(mi,ma), main="Too simple", cex.main=2)
lines(x.star, f3, col="green", lwd=2)
plot(x, yobs, pch=19,  xaxt='n',xlab="", ylab="",
     yaxt='n',ylim=c(mi,ma), main="Correct", cex.main=2)
lines(x.star, f2, col="green", lwd=2)
plot(x, yobs, pch=19,  xaxt='n',xlab="", ylab="",
     yaxt='n',ylim=c(mi,ma), main="Too complex", cex.main=2)
lines(x.star, f1, col="green", lwd=2)
dev.off()
