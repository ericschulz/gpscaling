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
x.star <- seq(-5,5,len=500)
#sigma
sigma.n <- 0.1

n<-seq(10,50,10)
near<-c(0.7, 0.3)
lambda<-c(2,5)
sim<-1:20

dd<-expand.grid(n=n,near=near,lambda=lambda,sim=sim)


for (i in 1:nrow(dd)){
  near<-dd$near[i]
  far<-1-near
  n<-dd$n[i]
  lambda<-dd$lambda[i]
  target<-sample(x.star, 1)
  yobs<-rep(-5,500)
  while(sum(abs(yobs)<1)!=500 | (max(yobs)-min(yobs))<1.2){
  yobs<- mvrnorm(1, rep(0, length(x.star)), rbf(x.star,x.star,l=lambda))
  }
  x.near<-x.star[abs(x.star-target)<=1]
  x.far<-x.star[abs(x.star-target)>1]
  x.near<-sample(x.near, near*n)
  x.far<-sample(x.far, far*n)
  
  x<-sort(c(x.near, x.far))
  yplot<-yobs[x.star %in% x]
  
  #add noise N(0,0.25)
  yplot<-yplot+rnorm(length(yplot), 0, sigma.n)
  
  ftrue<-yobs[x.star %in% target]
  fwrong<-yobs[x.star %in% target]+sample(c(-1.96,1.96), 1)*0.1
  
  ma<-max(c(yobs,ftrue,fwrong, yplot))
  mi<-min(c(yobs,ftrue,fwrong, yplot))
  
  png(paste0("pngs2/func", dd$sim[i],"l", dd$lambda[i], "n", dd$n[i],"near", dd$near[i]*100, ".png"),
      width=300, height=300)
  par(mar = c(0, 0, 0, 0))
  plot(x, yplot, pch=1,  xaxt='n', ann=FALSE, yaxt='n',ylim=c(-1.1,1.1), xlim=c(-5,5), col="black")
  abline(v = target, col="black", lwd=2)
  dev.off()
  
  png(paste0("pngs2/funcred", dd$sim[i],"l", dd$lambda[i], "n", dd$n[i],"near", dd$near[i]*100, ".png"),
      width=300, height=300)
  par(mar = c(0, 0, 0, 0))
  plot(x, yplot, pch=1,  xaxt='n', ann=FALSE, yaxt='n',ylim=c(-1.1,1.1), xlim=c(-5,5), col="black")
  abline(v = target, col="black", lwd=2)
  points(target, ftrue, pch=19, col="red", cex=1)
  points(target, fwrong,  pch=19, col="darkgreen", cex=1)
  dev.off()
  
  
  png(paste0("pngs2/funcblue", dd$sim[i],"l", dd$lambda[i], "n", dd$n[i],"near", dd$near[i]*100, ".png"),
      width=300, height=300)
  par(mar = c(0, 0, 0, 0))
  plot(x, yplot, pch=1,  xaxt='n', ann=FALSE, yaxt='n',ylim=c(-1.1,1.1), xlim=c(-5,5), col="grey30")
  abline(v = target, col="black", lwd=2)
  points(target, ftrue, pch=19, col="darkgreen", cex=1)
  points(target, fwrong, pch=19, col="red", cex=1)
  dev.off()
  write.csv(data.frame(x, yplot, target, ftrue, fwrong), 
            paste0("data2/func", dd$sim[i],"l", dd$lambda[i], "n", dd$n[i],"near", dd$near[i]*100, "csv"))
  print(i)
}


png("pngs2/correct.png", width=300, height=300)
par(mar = c(0, 0, 0, 0))
plot(x, yplot, pch="",  xaxt='n', ann=FALSE, yaxt='n',ylim=c(-1.1,1.1), xlim=c(-5,5), col="grey30")
text(0,0, "Correct", cex=4)
dev.off()

png("pngs2/incorrect.png", width=300, height=300)
par(mar = c(0, 0, 0, 0))
plot(x, yplot, pch="",  xaxt='n', ann=FALSE, yaxt='n',ylim=c(-1.1,1.1), xlim=c(-5,5), col="grey30")
text(0,0, "Incorrect", cex=3)
dev.off()

png("pngs2/init.png", width=300, height=300)
par(mar = c(0, 0, 0, 0))
plot(x, yplot, pch="",  xaxt='n', ann=FALSE, yaxt='n',ylim=c(-1.1,1.1), xlim=c(-5,5), col="grey30")
text(0,0, "+", cex=3)
dev.off()

png("pngs2/begin.png", width=300, height=300)
par(mar = c(0, 0, 0, 0))
plot(x, yplot, pch="",  xaxt='n', ann=FALSE, yaxt='n',ylim=c(-1.1,1.1), xlim=c(-5,5), col="grey30")
text(0,0, "Press the space\n bar to begin.", cex=3)
dev.off()
