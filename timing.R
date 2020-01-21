setwd("/home/hanshalbe/Desktop/gpscaling/data2")

library(microbenchmark)
library(tgp)

lf<-list.files()
mutime<-numeric()
for (i in 1:400){
  d<-read.csv(lf[i])
  X<-matrix(d$x)
  Z<-matrix(d$yplot)
  XX<-matrix(d$target)
  tt<-microbenchmark(bgp(X, Z, XX))
  mutime<-c(mutime, mean(tt$time))
}