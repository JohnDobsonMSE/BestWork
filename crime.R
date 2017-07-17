rm(list=ls())
a<-readLines("crime.csv")


X<-X[-c(state)]

#creating collumn header
cname<-strsplit(a[6],",")
cname[[1]][21]<-"State"

#by state
state<-grep("Estimated crime",a)
statenames<-strsplit(a[state],"Estimated crime in")

X<-read.csv("crime.csv", header = FALSE)
names(X)<-cname[[1]]
df<-X[-c(1:3)]
X<-X[-c(1:4,state,state+1)]

#starting after with alabama
row<-a[7:2904]
cells<-strsplit(a[7:2904],",")
row1<-row[[1]]
row1<-strsplit(row[[1]],",")
row1[21]<-"Alabama"
row1<-unlist(row1)
mat1<-matrix(row1)
mat1<-t(mat1)

#creating dataframe with row 1, and naming them collumns
d<-data.frame(mat1)
names(d)<-cname[[1]]
names(X)<-cname[[1]]

#attempting just alabama
rowsa<-row[1:53]
rowsa[1]<-strsplit(rows[[1]],",")
cells<-strsplit(rowsa,",")
data
cells<-as.numeric(strsplit(rowsa[[1]][1],",")[[1]])

for (i in 1:53){
  cells<-(strsplit(rowsa[[1]][i],","))
  d<-rbind(d,cells)
}

for (i in 1:53){
  rows[[i]]<-strsplit(row[[i]],",")
}

unlisted<-unlist(rows)
d<-cbind(d,unlisted)
for (i in 1:53){
  for (j in 1:20){
  row1<-as.numeric(strsplit(row[[1]][i],"&")[[1]])
  d<-rbind(d,row)
  }
}

mat2<-t(matrix(row[1:53]))

d$state<-statenames[1]
for (i in 1:length(row)) {
  row[[i]]<-
mat2<-matrix(row[[i]])
}






#failed ideas
#states
stateid<-list(1:50)
for (i in 2:50) {
  stateid[[1]]<-a[4]
  stateid[i]<-a[4+(57*i)]
}

for (i in 1:51){
  for (j in 1:53){
    state[i]<-row[i:(53+i)]
  }
}

alabama<-rows[1:53]
alabama<-strsplit(alabama,",")
alabama1<-alabama[1]
alabama1<-matrix(alabama1)
t(matrix(alabama))
alabamaframe<-data.frame(matrix(alabama1))
a<-data.frame(NA)

x<-read.table(row, header = FALSE)
read.table