rm(list=ls())
load("~/Desktop/R Files/R Directory/ECONtraining.RData")

#Question seems to be very influential
mean(datatraining$correct[datatraining$question=="CT3eCH13MC26"]) #.333
mean(datatraining$correct[datatraining$question=="CT3eCH14MC178"]) #.325
mean(datatraining$correct[datatraining$question=="CT3eCH03MC73"]) #.928

df1<-dataholdoutBLIND
df2<-datatraining

Q<- list(datatraining$question)
Q<-unlist(Q)
Q<-sort(Q)
Qs<-unique(unlist(Q))


MeanQ<-numeric()
for (i in 1:190) {
  MeanQ[i]<-mean(datatraining$correct[datatraining$question==Qs[i]])
}       

which(MeanQ>.9)
easyQ<-which(MeanQ>.9)
reallyeasy<-which(MeanQ>.95)
hardQ<-which(MeanQ<.6)
normalQ<-Qs[-c(hardQ,easyQ)]
reallyhard<-which(MeanQ<.3)

EASY<-Qs[easyQ]
HARD<-Qs[hardQ]
EQ<-which(df2$question==EASY)
HARD<-Qs[hardQ]


#choosing outlier students
A<- c(datatraining$ID)
A<-unique(A)
MeanA<-numeric()
for (i in 10:71) {
  MeanA[i]<-mean(datatraining$correct[datatraining$ID==i])
}       

which(MeanA>.9) #11 14 29 30 33 56 67
which(MeanA<.6) #21 #38 #40 #44


topstudents<-which(MeanA>.87)
aboveaverage<-which(MeanA<.75)

bottomstudents<-which(MeanA<.65)
middlestudents<-A[-c(topstudents,bottomstudents,11)]

#Easy model
modelT<- lm(formula = datatraining$correct~datatraining$ID+datatraining$question+datatraining$exam)
summary(modelT)

#Average correct responses, 75% in training data
length(which(datatraining$correct==1))/length(datatraining$correct)

#Exam Type doesn't seem to effect grades
#update: this is wrong, does effect, should have considered
mean(datatraining$correct[datatraining$exam==1]) #.7621
mean(datatraining$correct[datatraining$exam==2]) #.747
mean(datatraining$correct[datatraining$exam==3]) #.745
mean(datatraining$correct[datatraining$exam==4]) #.77


#organizing data to include 1's for each variable

topstudentstraining<-c(which(datatraining$ID==11),which(datatraining$ID==14),which(datatraining$ID==15),which(datatraining$ID==29),which(datatraining$ID==30),which(datatraining$ID==33),which(datatraining$ID==42),which(datatraining$ID==49),which(datatraining$ID==56),which(datatraining$ID==67),which(datatraining$ID==67))
bottomstudentstraining<-c(which(datatraining$ID==17), which(datatraining$ID==21),which(datatraining$ID==38),which(datatraining$ID==40),which(datatraining$ID==44),which(datatraining$ID==46),which(datatraining$ID==51),which(datatraining$ID==59))

ReallyHARDQ<-c(which(datatraining$question=="CT3eCH09MC58"),which(datatraining$question=="CT3eCH05MC129"),which(datatraining$question=="CT3eCH06MC94"),which(datatraining$question=="CT3eCH06MC138"))

EASYQtraining<-c(which(datatraining$question=="CT3eCH03MC73"),which(datatraining$question=="CT3eCH02MC111"),which(datatraining$question=="CT3eCH04TF2"),which(datatraining$question=="CT3eCH04MC74"),which(datatraining$question=="CT3eCH03MC41"),which(datatraining$question=="CT3eCH05MC76"),which(datatraining$question=="CT3eCH04MC74"),which(datatraining$question=="CT3eCH03MC41"),which(datatraining$question=="CT3eCH05MC76"),which(datatraining$question=="CT3eCH03MC123"),which(datatraining$question=="CT3eCH03TF15"),which(datatraining$question=="CT3eCH04MC138"),which(datatraining$question=="CT3eCH03MC78"),which(datatraining$question=="CT3eCH04MC127"),
which(datatraining$question=="CT3eCH19TF20"),which(datatraining$question=="CT3eCH11MC133"),which(datatraining$question=="CT3eCH11MC64"),which(datatraining$question=="CT3eCH10MC158"),which(datatraining$question=="CT3eCH11MC77"),which(datatraining$question=="CT3eCH19MC45"),which(datatraining$question=="CT3eCH19TF8"),which(datatraining$question=="CT3eCH09MC48"),which(datatraining$question=="CT3eCH08MC6"),which(datatraining$question=="CT3eCH08MC41"),which(datatraining$question=="CT3eCH11MC123"),which(datatraining$question=="CT3eCH03MC82"),which(datatraining$question=="CT3eCH14MC19"),which(datatraining$question=="CT3eCH19MC6"),which(datatraining$question=="CT3eCH14MC71"),
which(datatraining$question=="CT3eCH01MC107"),which(datatraining$question=="CT3eCH10MC72"),which(datatraining$question=="CT3eCH11MC46"),which(datatraining$question=="CT3eCH02MC132"),which(datatraining$question=="CT3eCH04MC196"),which(datatraining$question=="CT3eCH08MC166"),which(datatraining$question=="CT3eCH14MC163"),which(datatraining$question=="CT3eCH03MC63"),which(datatraining$question=="CT3eCH05MC93"),which(datatraining$question=="CT3eCH02MC34"),which(datatraining$question=="CT3eCH03MC67"),which(datatraining$question=="CT3eCH04MC113"),which(datatraining$question=="CT3eCH02MC122"),which(datatraining$question=="CT3eCH08MC87"),which(datatraining$question=="CT3eCH06MC3"),
which(datatraining$question=="CT3eCH13MC58"),which(datatraining$question=="CT3eCH04MC137"),which(datatraining$question=="CT3eCH09MC10"),which(datatraining$question=="CT3eCH10MC26"),which(datatraining$question=="CT3eCH08MC131"),which(datatraining$question=="CT3eCH13MC29"),which(datatraining$question=="CT3eCH04MC84"))

HARDQtraining<- c(which(datatraining$question=="CT3eCH05MC83"),which(datatraining$question=="CT3eCH06MC129"),which(datatraining$question=="CT3eCH05TF32"),which(datatraining$question=="CT3eCH06MC162"),which(datatraining$question=="CT3eCH06MC6"),which(datatraining$question=="CT3eCH04MC174"),which(datatraining$question=="CT3eCH08MC107"),which(datatraining$question=="CT3eCH08MC85"),which(datatraining$question=="CT3eCH19MC54"),which(datatraining$question=="CT3eCH09MC55"),which(datatraining$question=="CT3eCH09MC58"),which(datatraining$question=="CT3eCH08MC130"),which(datatraining$question=="CT3eCH19MC114"),which(datatraining$question=="CT3eCH11MC135"),which(datatraining$question=="CT3eCH05MC129"),
which(datatraining$question=="CT3eCH14MC95"),which(datatraining$question=="CT3eCH06MC94"),which(datatraining$question=="CT3eCH14MC178"),which(datatraining$question=="CT3eCH03MC186"),which(datatraining$question=="CT3eCH09MC60"),which(datatraining$question=="CT3eCH06MC163"),which(datatraining$question=="CT3eCH06MC138"),which(datatraining$question=="CT3eCH08TF13"),which(datatraining$question=="CT3eCH09MC100"),which(datatraining$question=="CT3eCH01MC70"),which(datatraining$question=="CT3eCH14MC114"),which(datatraining$question=="CT3eCH10MC137"),which(datatraining$question=="CT3eCH13MC26"),which(datatraining$question=="CT3eCH09MC63"))

datatraining$exam1<-0
datatraining$exam1[datatraining$exam==1]<-1
datatraining$exam2<-0
datatraining$exam1[datatraining$exam==2]<-1
datatraining$exam3<-0
datatraining$exam1[datatraining$exam==3]<-1

datatraining$Reallyhard<-0
datatraining$Reallyhard[ReallyHARDQ]<-1
datatraining$easyq<-0
datatraining$easyq[EASYQtraining]<-1
datatraining$hardq<-0
datatraining$hardq[HARDQtraining]<-1
datatraining$top<-0
datatraining$bottom<-0
datatraining$top[topstudentstraining]<-1
datatraining$bottom[bottomstudentstraining]<-1


#Trying Training Model

lm2<-lm(formula = datatraining$correct~datatraining$easyq+datatraining$top+datatraining$bottom + datatraining$hardq+datatraining$Reallyhard)
summary(lm2)


lm3<-lm(formula = datatraining$correct~datatraining$easyq+datatraining$top+datatraining$bottom + datatraining$hardq+datatraining$Reallyhard +datatraining$exam1 + datatraining$exam2 + datatraining$exam3)
summary(lm3)

X<-as.matrix(datatraining$easyq,datatraining$top,datatraining$bottom,datatraining$hardq,datatraining$ID,datatraining$question)
B<-coef(lm3)

datatraining$scoreprediction<- 0.755228 + datatraining$top*0.160869 + datatraining$bottom*-.187281 + datatraining$easyq* .186930 + datatraining$hardq*-.263802 +datatraining$Reallyhard*-.248025
ones<- which((datatraining$scoreprediction)>1)
zeros<- which((datatraining$scoreprediction)<.10)
datatraining$scoreprediction[zeros]<-0
datatraining$scoreprediction[ones]<-1

SSE<- sum((datatraining$correct - datatraining$scoreprediction)^2)
SST<- sum((datatraining$correct - mean(datatraining$correct))^2)
R2<- 1 - (SSE/SST)
lm4<- lm(formula = datatraining$correct ~ datatraining$scoreprediction)
summary(lm4)

X<-as.character(datatraining$ID)
datatraining$IDA<-X

linmod<-lm(formula = datatraining$correct~datatraining$IDA + datatraining$question)
summary(linmod)
linmod2<-lm(formula = datatraining$correct~datatraining$IDA + datatraining$question + datatraining$easyq + datatraining$hardq + datatraining$top + datatraining$bottom)
summary(linmod2)

prediction<-predict(linmod)
datatraining$prediction<-prediction

ones<- which((datatraining$prediction)>1)
zeros<- which((datatraining$prediction)<.10)
datatraining$prediction[zeros]<-0
datatraining$prediction[ones]<-1
SSE<- sum((datatraining$correct - datatraining$prediction)^2)
SST<- sum((datatraining$correct - mean(datatraining$correct))^2)
R2<- 1 - (SSE/SST)

SSE<-sum((datatraining$correct - .75)^2)

B<-coefficients(linmod2)
X<-as.matrix(cbind(rep(1,8239),datatraining$IDA,datatraining$question))
yhat<-t(X)%*%B

#alternative model based on conditional means
mean(datatraining$correct[datatraining$bottom==1]) #.501
mean(datatraining$correct[datatraining$top==1]) #.924
mean(datatraining$correct[datatraining$easyq==1]) #.944
mean(datatraining$correct[datatraining$Reallyhard==1]) #.2465
#Translating to Holdout Data

topstudentsholdout<-c(which(dataholdoutBLIND$ID==11),which(dataholdoutBLIND$ID==14),which(dataholdoutBLIND$ID==15),which(dataholdoutBLIND$ID==29),which(dataholdoutBLIND$ID==30),which(dataholdoutBLIND$ID==33),which(dataholdoutBLIND$ID==42),which(dataholdoutBLIND$ID==49),which(dataholdoutBLIND$ID==56),which(dataholdoutBLIND$ID==65),which(dataholdoutBLIND$ID==67))
bottomstudentsholdout<-c(which(dataholdoutBLIND$ID==17),which(dataholdoutBLIND$ID==21),which(dataholdoutBLIND$ID==38),which(dataholdoutBLIND$ID==40),which(dataholdoutBLIND$ID==44),which(dataholdoutBLIND$ID==46,which(dataholdoutBLIND$ID==51),which(dataholdoutBLIND$ID==59)))



EASYQholdout<-c(which(dataholdoutBLIND$question=="CT3eCH03MC73"),which(dataholdoutBLIND$question=="CT3eCH02MC111"),which(dataholdoutBLIND$question=="CT3eCH04TF2"),which(dataholdoutBLIND$question=="CT3eCH04MC74"),which(dataholdoutBLIND$question=="CT3eCH03MC41"),which(dataholdoutBLIND$question=="CT3eCH05MC76"),which(dataholdoutBLIND$question=="CT3eCH04MC74"),which(dataholdoutBLIND$question=="CT3eCH03MC41"),which(dataholdoutBLIND$question=="CT3eCH05MC76"),which(dataholdoutBLIND$question=="CT3eCH03MC123"),which(dataholdoutBLIND$question=="CT3eCH03TF15"),which(dataholdoutBLIND$question=="CT3eCH04MC138"),which(dataholdoutBLIND$question=="CT3eCH03MC78"),which(dataholdoutBLIND$question=="CT3eCH04MC127"),
                 which(dataholdoutBLIND$question=="CT3eCH19TF20"),which(dataholdoutBLIND$question=="CT3eCH11MC133"),which(dataholdoutBLIND$question=="CT3eCH11MC64"),which(dataholdoutBLIND$question=="CT3eCH10MC158"),which(dataholdoutBLIND$question=="CT3eCH11MC77"),which(dataholdoutBLIND$question=="CT3eCH19MC45"),which(dataholdoutBLIND$question=="CT3eCH19TF8"),which(dataholdoutBLIND$question=="CT3eCH09MC48"),which(dataholdoutBLIND$question=="CT3eCH08MC6"),which(dataholdoutBLIND$question=="CT3eCH08MC41"),which(dataholdoutBLIND$question=="CT3eCH11MC123"),which(dataholdoutBLIND$question=="CT3eCH03MC82"),which(dataholdoutBLIND$question=="CT3eCH14MC19"),which(dataholdoutBLIND$question=="CT3eCH19MC6"),which(dataholdoutBLIND$question=="CT3eCH14MC71"),
                 which(dataholdoutBLIND$question=="CT3eCH01MC107"),which(dataholdoutBLIND$question=="CT3eCH10MC72"),which(dataholdoutBLIND$question=="CT3eCH11MC46"),which(dataholdoutBLIND$question=="CT3eCH02MC132"),which(dataholdoutBLIND$question=="CT3eCH04MC196"),which(dataholdoutBLIND$question=="CT3eCH08MC166"),which(dataholdoutBLIND$question=="CT3eCH14MC163"),which(dataholdoutBLIND$question=="CT3eCH03MC63"),which(dataholdoutBLIND$question=="CT3eCH05MC93"),which(dataholdoutBLIND$question=="CT3eCH02MC34"),which(dataholdoutBLIND$question=="CT3eCH03MC67"),which(dataholdoutBLIND$question=="CT3eCH04MC113"),which(dataholdoutBLIND$question=="CT3eCH02MC122"),which(dataholdoutBLIND$question=="CT3eCH08MC87"),which(dataholdoutBLIND$question=="CT3eCH06MC3"),
                 which(dataholdoutBLIND$question=="CT3eCH13MC58"),which(dataholdoutBLIND$question=="CT3eCH04MC137"),which(dataholdoutBLIND$question=="CT3eCH09MC10"),which(dataholdoutBLIND$question=="CT3eCH10MC26"),which(dataholdoutBLIND$question=="CT3eCH08MC131"),which(dataholdoutBLIND$question=="CT3eCH13MC29"),which(dataholdoutBLIND$question=="CT3eCH04MC84"))

HARDQholdout<- c(which(dataholdoutBLIND$question=="CT3eCH05MC83"),which(dataholdoutBLIND$question=="CT3eCH06MC129"),which(dataholdoutBLIND$question=="CT3eCH05TF32"),which(dataholdoutBLIND$question=="CT3eCH06MC162"),which(dataholdoutBLIND$question=="CT3eCH06MC6"),which(dataholdoutBLIND$question=="CT3eCH04MC174"),which(dataholdoutBLIND$question=="CT3eCH08MC107"),which(dataholdoutBLIND$question=="CT3eCH08MC85"),which(dataholdoutBLIND$question=="CT3eCH19MC54"),which(dataholdoutBLIND$question=="CT3eCH09MC55"),which(dataholdoutBLIND$question=="CT3eCH09MC58"),which(dataholdoutBLIND$question=="CT3eCH08MC130"),which(dataholdoutBLIND$question=="CT3eCH19MC114"),which(dataholdoutBLIND$question=="CT3eCH11MC135"),which(dataholdoutBLIND$question=="CT3eCH05MC129"),
                  which(dataholdoutBLIND$question=="CT3eCH14MC95"),which(dataholdoutBLIND$question=="CT3eCH06MC94"),which(dataholdoutBLIND$question=="CT3eCH14MC178"),which(dataholdoutBLIND$question=="CT3eCH03MC186"),which(dataholdoutBLIND$question=="CT3eCH09MC60"),which(dataholdoutBLIND$question=="CT3eCH06MC163"),which(dataholdoutBLIND$question=="CT3eCH06MC138"),which(dataholdoutBLIND$question=="CT3eCH08TF13"),which(dataholdoutBLIND$question=="CT3eCH09MC100"),which(dataholdoutBLIND$question=="CT3eCH01MC70"),which(dataholdoutBLIND$question=="CT3eCH14MC114"),which(dataholdoutBLIND$question=="CT3eCH10MC137"),which(dataholdoutBLIND$question=="CT3eCH13MC26"),which(dataholdoutBLIND$question=="CT3eCH09MC63"))

ReallyHARDQholdout<-c(which(dataholdoutBLIND$question=="CT3eCH09MC58"),which(dataholdoutBLIND$question=="CT3eCH05MC129"),which(dataholdoutBLIND$question=="CT3eCH06MC94"),which(dataholdoutBLIND$question=="CT3eCH06MC138"))


dataholdoutBLIND$topstudents<-0
dataholdoutBLIND$topstudents[topstudentsholdout]<-1
dataholdoutBLIND$bottomstudents<-0
dataholdoutBLIND$bottomstudents[bottomstudentsholdout]<-1
dataholdoutBLIND$easyq<-0
dataholdoutBLIND$easyq[EASYQholdout]<-1
dataholdoutBLIND$hardq<-0
dataholdoutBLIND$hardq[HARDQholdout]<-1
dataholdoutBLIND$reallyhard<-0
dataholdoutBLIND$reallyhard[ReallyHARDQholdout]<-1


#My Holdout Model based on training data

dataholdoutBLIND$scoreprediction<-0.755228 + dataholdoutBLIND$topstudents*.160869 + dataholdoutBLIND$bottomstudents*-.187281 + dataholdoutBLIND$easyq* .186930 + dataholdoutBLIND$hardq*-.263802 + dataholdoutBLIND$reallyhard*-.248025
datatraining$scoreprediction<- 0.755228 + datatraining$top*0.160869 + datatraining$bottom*-.187281 + datatraining$easyq* .186930 + datatraining$hardq*-.263802 +datatraining$Reallyhard*-.248025

ones<- which((dataholdoutBLIND$scoreprediction)>1)
zeros<- which((dataholdoutBLIND$scoreprediction)<.10)
dataholdoutBLIND$scoreprediction[zeros]<-0
dataholdoutBLIND$scoreprediction[ones]<-1

submission<-data.frame(dataholdoutBLIND$scoreprediction)
submit<- write.csv(dataholdoutBLIND$scoreprediction)

#Best R-Squared I got was .22, but that was using the lm, and predict functions and having the correct data.
read.csv("Problem Set 6 John Dobson.csv")

read.csv("Problem Set 6 John Dobson")
