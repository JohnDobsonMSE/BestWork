rm(list=ls())
d <- read.csv("crimerate.csv")
d$highcrime=as.integer(d$crimerate>0.003)

result <- glm(highcrime ~ emp + un + ymale + white , family=binomial(link="probit"),data=d)
summary(result)


x <- as.matrix(d[,5:8])
x <- cbind(integer(length(d$highcrime))+1,x)  #constant
y <- as.vector(d[,12])
b <- numeric(5)


ll <- function(b,x,y){
  p <- pnorm(x %*% b,log.p=TRUE)
  p2 <- pnorm(- x %*% b,log.p=TRUE)
  return(sum(y*p+(1-y)*p2))
}



gr <- function(b,x,y){
  p <- pnorm(x %*% b)
  d <- dnorm(x %*% b)
  return(as.vector(t(x) %*% (y*d/p + (1-y)*(-d)/(1-p))))
}

h <- function(b,x,y) {
  p <- pnorm(x %*% b)
  d <- dnorm(x %*% b)
  e <- matrix(numeric(25),nrow=5)
  for(i in 1:length(y)){
    if(y[i]==1){
      e <- e+ as.numeric(d[i]/p[i]*(d[i]/p[i]+x[i,] %*% b))*(x[i,]%*%t(x[i,]))
    } else{
      f <- -d[i]/(1-p[i])
      g <- x[i,] %*% b
      g<- f*(f+g)
      e <- e+as.numeric(g)*(x[i,]%*%t(x[i,]))
    }
  }
  return (-e)
}




library(maxLik)
system.time(result <- maxLik(ll,start=numeric(5),method="NR",grad=gr,hess=h,x=x,y=y))
summary(result)

bmatrix=NULL
for(i in 1:10){
  ind = sample(1:3328,replace=TRUE)
  x2 = x[ind,]
  y2 = y[ind]
  bresults =  maxLik(ll,start=numeric(5),method="NR",grad=gr,hess=h,x=x2,y=y2)
  bmatrix=rbind(bmatrix,bresults$estimate)
}
sderror=vector()
for(i in 1:5){
  sderror[i] = sd(bmatrix[,i])
}




#Comp

d <- read.csv("crimerate.csv")
d$highcrime=as.integer(d$crimerate>0.003)

result <- glm(highcrime ~ emp + un + ymale + white , family=binomial(link="probit"),data=d)
summary(result)


x <- as.matrix(d[,5:8])
x <- cbind(integer(length(d$highcrime))+1,x)  #constant
y <- as.vector(d[,12])
b <- numeric(5)


ll <- function(b){
  p <- pnorm(x %*% b,log.p=TRUE)
  p2 <- pnorm(- x %*% b,log.p=TRUE)
  return(sum(y*p+(1-y)*p2))
}


llmin <- function(b){
  p <- pnorm(x %*% b,log.p=TRUE)
  p2 <- pnorm(- x %*% b,log.p=TRUE)
  r <- -sum(y*p+(1-y)*p2)
  return(r)
}

gr <- function(b){
  p <- pnorm(x %*% b)
  d <- dnorm(x %*% b)
  return(as.vector(t(x) %*% (y*d/p + (1-y)*(-d)/(1-p))))
}

h <- function(b) {
  p <- pnorm(x %*% b)
  d <- dnorm(x %*% b)
  e <- matrix(numeric(25),nrow=5)
  for(i in 1:length(y)){
    if(y[i]==1){
      e <- e+ as.numeric(d[i]/p[i]*(d[i]/p[i]+x[i,] %*% b))*(x[i,]%*%t(x[i,]))
    } else{
      f <- -d[i]/(1-p[i])
      g <- x[i,] %*% b
      g<- f*(f+g)
      e <- e+as.numeric(g)*(x[i,]%*%t(x[i,]))
    }
  }
  return (-e)
}




library(maxLik)
system.time(result <- maxLik(ll,start=numeric(5),method="NR",grad=gr,hess=h))
summary(result)

system.time(result <- maxLik(ll,start=numeric(5),method="NR"))
summary(result)

(result <- nlm(llmin,b,hessian=TRUE))
sqrt(diag(solve(result$hessian)))


control=list(10000)
names(control)=c("maxit")
(result <- optim(b,llmin,hessian=TRUE))
sqrt(diag(solve(result$hessian)))


(result <- nlminb(b,llmin,hessian=TRUE,lower=c(-5,-5,-5,-5,-Inf),upper=c(5,5,5,5,Inf)))


#Newton Raphson

nr <-function(b){
  i <- 0
  while(sqrt(sum(gr(b)^2))>1e-7){
    i <-i+1
    b <- b-(solve(h(b))%*%gr(b))
  }
  r <- list()
  r[[1]]<-b
  r[[2]]<-sqrt(diag(solve(-h(b))))
  r[[3]]<-i
  return(r)
}


#grid search
result <- glm(highcrime ~ emp  , family=binomial(link="probit"),data=d)
summary(result)

gridsearch2 <- function(ll,minp,maxp,step){
  iter=(maxp-minp)/step
  maxl=-Inf
  for(i in 1:iter){
    for (j in 1:iter){
      b1=minp+i*step
      b2=minp+j*step
      l=ll(c(b1,b2))
      if(l>maxl){
        maxl=l
        maxb1=b1
        maxb2=b2
      }
    }
  }
  return(c(maxb1,maxb2,maxl))
}

x <- as.matrix(d[,5])
x <- cbind(integer(length(d$highcrime))+1,x)  #constant
y <- as.vector(d[,12])
gridsearch2(ll,-10,10,0.1)



a<-0
for (i in 1:1000) {
  a<-a+(1/1000)
}