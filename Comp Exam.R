selsort<- function(x){
  for (i in 1:length(x)) {
    mini<-i
  for (j in i:length(x)) {
    if (x[j] < x[mini]){
  mini<- j
  }
  }
    temp = x[i]
    x[i] = x[mini]
    x[mini] = temp
  }
  return(x)
}

x = 10:1
v = c(10,4,7,2,8,45,67,24)
selsort(x)
selsort(v)

#John Dobson Problem Set 4
#CDF is Function sqrt(x/10), PDF is 1/(20)(sqrt(x/10))
# U = X+Y+Z, from 0-30
rm(list=ls())
omega<-numeric()
x<-numeric()
y<-numeric()
z<-numeric()
#pdf for uniform 1/b-a, a=0, so 1/upper bound
ucdf<-function(u){
  for (i in 1:100000) {
    z<-runif(1,0,min(10,u))
    y<-runif(1,0,min(10,u-z))
    x<-runif(1,0,min(10,u-z-y))
    fx<-1/(20*sqrt(x/10))
    fy<-1/(20*sqrt(y/10))
    fz<-1/(20*sqrt(z/10))
    hx<- 1/min(10,u-z-y)
    hy<- 1/min(10,u-z)
    hz<- 1/min(10,u)
    omega[i]<-(fx*fy*fz)/(hx*hy*hz)
  }
  mean(omega)
  sd(omega)
}


x = runif(100000,0,1)
fx = 1/(sqrt(1-x^2))
hx = 1/1
mean(fx/hx)
sd(fx/hx)/sqrt(1000000)
