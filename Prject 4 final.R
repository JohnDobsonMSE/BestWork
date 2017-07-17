 #Project 4, John Anthony Dobson


rm(list=ls())
df<-read.csv("pddata.csv",sep=",",head=TRUE)   
df2<-df
set.seed(100)

#Creating new dataframe for bootstrap
Aa_<-vector()
Aa_[1:100]<-0
Ab_<-vector()
Ab_[1:100]<-0
proa<-0
prob<-0
df$Aa_<-0
df$Ab_<-0

h<-1 #for function
d<-1
df1<-df
J<-df$id
df$id2<-df$id
df2<-df #for bootstrap


#Function ll_rl
parameters<-vector()
ll_rl<-function(parameters){
  d<-parameters[1]
  h<-parameters[2]
  L<-c()
  listofdf<-list()
  for (i in 1:20) {
    listofdf[[i]]<-df[df$id2==i,]
  }
  for (j in 1:20) {
    for (i in 2:100) {
      id<-j
      Aa_[i]<-0
      Ab_[i]<-0
      listofdf[[j]]$Aa_[1]<-0
      listofdf[[j]]$Ab_[1]<-0
      listofdf[[j]]$Aa_[i]<-d*listofdf[[j]]$Aa_[i-1]+((1-listofdf[[j]]$choice[i-1])*(listofdf[[j]]$payoff[i-1]))
      listofdf[[j]]$Ab_[i]<-d*listofdf[[j]]$Ab_[i-1]+((listofdf[[j]]$choice[i-1])*listofdf[[j]]$payoff[i-1])
      Aa_[i]<-d*Aa_[i-1]+(1-df$choice[df$id2==id][i-1])*df$epayoff[df$id2==id][i-1]
      Ab_[i]<-d*Ab_[i-1]+(df$choice[df$id2==id][i-1])*df$epayoff[df$id2==id][i-1]
      listofdf[[j]]$propa2[i]<-exp(Aa_[i])/ (exp(Aa_[i])+exp(Ab_[i]))
      listofdf[[j]]$propb2[i]<-exp(Ab_[i])/ (exp(Aa_[i])+exp(Ab_[i]))
    }
  }
  df1<-listofdf[[1]]
  for (i in 2:20) {
    x<-listofdf[[i]]
    df1<-rbind(df1,x)
  }
  for (i in 1:2000) {
    L[i]<-((((1-df1$choice[i])*df1$Aa_[i])*h)+(((df1$choice[i])*df1$Ab_[i])*h)- (log(exp(h*df1$Aa_[i]) + exp(h*df1$Ab_[i]))) )
  }
  LL<-sum(L)
  negLL<- -LL
  return(negLL)
}

#Running function, can take a minute on my computer
parameters<-c(0,0)
results1<-nlminb(parameters,ll_rl,lower = c(0,0),upper = c(1, Inf ))
result<-results1$par
llresult<-results1$objective

#Bootstrap
df3<-data.frame()
df4<-data.frame()
df5<-data.frame()
bparameters<-c() #bootstrap parameters


#Bootstrap will take a while to run considering my function takes a while
XXZ<-matrix(0,nrow = 10,ncol = 2)
for (i in 1:10) {
  parameters<-c(0,0)
  ind = sample(1:20, replace = TRUE)
  df<-data.frame()
  for (j in ind) {
    x<-which(df2$id==j)
    df3<-df2[x,]
    df<-rbind(df,df3)
  }
  df$id2<-J 
  bp<-nlminb(parameters,ll_rl,lower = c(0,0),upper = c(1, Inf ))
  
  XXZ[i,]<-bp$par
  
}
matrix1<-XXZ

sd(matrix1[1,])
sd(matrix1[1,])
serror<-c(sd(matrix1[,1]),sd(matrix1[,2]))

df<-read.csv("pddata.csv",sep=",",head=TRUE)   

df$id2<-J

parameters<-vector()
ll_rl2<-function(parameters){
  d<-parameters[1]
  h<-parameters[2]
  L<-c()
  listofdf<-list()
  for (i in 1:20) {
    listofdf[[i]]<-df[df$id2==i,]
  }
  for (j in 1:20) {
    for (i in 2:100) {
      id<-j
      Aa_[i]<-0
      Ab_[i]<-0
      listofdf[[j]]$Aa_[1]<-0
      listofdf[[j]]$Ab_[1]<-0
      listofdf[[j]]$Aa_[i]<-d*listofdf[[j]]$Aa_[i-1]+((1-listofdf[[j]]$choice[i-1])*(listofdf[[j]]$epayoff[i-1]))
      listofdf[[j]]$Ab_[i]<-d*listofdf[[j]]$Ab_[i-1]+((listofdf[[j]]$choice[i-1])*listofdf[[j]]$epayoff[i-1])
    }
  }
  df1<-listofdf[[1]]
  for (i in 2:20) {
    x<-listofdf[[i]]
    df1<-rbind(df1,x)
  }
  for (i in 1:2000) {
    L[i]<-((((1-df1$choice[i])*df1$Aa_[i])*h)+(((df1$choice[i])*df1$Ab_[i])*h)- (log(exp(h*df1$Aa_[i]) + exp(h*df1$Ab_[i]))) )
  }
  LL<-sum(L)
  negLL<- -LL
  return(negLL)
}



parameters<-c(0,0)
results2<-nlminb(parameters,ll_rl2,lower = c(0,0),upper = c(1, Inf ))

result2<-results2$par
llresult2<-results2$objective

df3<-data.frame()
df4<-data.frame()
df5<-data.frame()
bparameters<-c() #bootstrap parameters


XXZ<-matrix(0,nrow = 10,ncol = 2)
for (i in 1:10) {
  parameters<-c(0,0)
  ind = sample(1:20, replace = TRUE)
  df<-data.frame()
  for (j in ind) {
    x<-which(df2$id==j)
    df3<-df2[x,]
    df<-rbind(df,df3)
  }
  df$id2<-J 
  bp<-nlminb(parameters,ll_rl2,lower = c(0,0),upper = c(1, Inf ))
  
  XXZ[i,]<-bp$par
  
}
matrix2<-XXZ

serror2<-c(sd(matrix2[,1]),sd(matrix2[,2]))


result
llresult
serror

result2
llresult2
serror2


