#Terra Data Code 2
rm(list=ls())
#dfa = read.csv('accounts with years.csv')
dfa = read.csv('AccountsWithNoNAsInGivingYears.csv')
#dfc = load('LeanContacts.RData')
#dfc2 = read.csv('ContactHasLeftForProbit-3.csv', stringsAsFactors = FALSE)
#dfc = x
#dfo = load('OpportunitiesnoTF.RData')
#dfo = x
#dfa = dfa[1:233273,]

#dfa5 = read.csv('accounts with years.csv')

str(dfc2)
which(dfa$rC_Bios__Active__c == FALSE | dfa$rC_Bios__Active__c == NA)
length(which(dfa$rC_Bios__Active__c != FALSE | dfa$rC_Bios__Active__c != NA))

unique(dfa$Industry)


meanday = mean(dfa$rC_Bios__Age_for_Preference_Rule_Engine__c)

x = which(dfa$rC_Bios__Age_for_Preference_Rule_Engine__c > 1126)

dfa2 = dfa[x,]
#mod = lm(dfa2$Total_Giving_2016__c ~ dfa2$BillingCountry + dfa2$Account_Type__c)
summary(mod)

#mod2 = lm(dfa2$Total_Giving_2016__c ~ dfa2$BillingCountry + dfa2$Account_Type__c + dfa2$BillingState + dfa2$First_Inkind_Hard_Credit_Amount__c)
summary(mod2)

AccountNumber = dfa$Account_Number__c

#merging dataframes

dfa3 = dfa
dfa3$AccountId = dfa$Id
dfao = merge(dfa3,dfo)
dfac = merge(dfa3,dfc2)
dfoc = merge(dfo,dfc2)
dfaoc = merge(dfoc, dfc2)

library('lubridate')
dfao$CreatedDate2 =  ymd_hms(dfao$CreatedDate)
dfao$LastActivityDate2 =  ymd(dfao$LastActivityDate)
#dfao$timesince = difftime(dfao$CreatedDate2,dfao$LastActivityDate2, units = "days")
#dfao$timesince =  difftime(dfao$CreatedDate, dfao$LastActivityDate)
as.numeric(dfao$timesince)
dfao$timesince=as.numeric(dfao$timesince)


dfa$hosted2007 = 0
dfa$hosted2007[dfa$Num_of_Events_2007__c > 0] = 1
dfa$hosted2008 = 0
dfa$hosted2008[dfa$Num_of_Events_2008__c > 0] = 1
dfa$hosted2009 = 0
dfa$hosted2009[dfa$Num_of_Events_2009__c > 0] = 1
dfa$hosted2010 = 0
dfa$hosted2010[dfa$Num_of_Events_2010__c > 0] = 1
dfa$hosted2011 = 0
dfa$hosted2011[dfa$Num_of_Events_2011__c > 0] = 1
dfa$hosted2012 = 0
dfa$hosted2012[dfa$Num_of_Events_2012__c > 0] = 1
dfa$hosted2013 = 0
dfa$hosted2013[dfa$Num_of_Events_2013__c > 0] = 1
dfa$hosted2014 = 0
dfa$hosted2014[dfa$Num_of_Events_2014__c > 0] = 1
dfa$hosted2015 = 0
dfa$hosted2015[dfa$Num_of_Events_2015__c > 0] = 1
dfa$hosted2016 = 0
dfa$hosted2016[dfa$Num_of_Events_2016__c > 0] = 1


#Looking at giving over time

dfa$givingfraction10 = dfa$Total_Giving_2010__c/dfa$Total_Giving__c
dfa$givingfraction11 = dfa$Total_Giving_2011__c/dfa$Total_Giving__c
dfa$givingfraction12 = dfa$Total_Giving_2012__c/dfa$Total_Giving__c
dfa$givingfraction13 = dfa$Total_Giving_2013__c/dfa$Total_Giving__c
dfa$givingfraction14 = dfa$Total_Giving_2014__c/dfa$Total_Giving__c
dfa$givingfraction15 = dfa$Total_Giving_2015__c/dfa$Total_Giving__c
dfa$givingfraction16 = dfa$Total_Giving_2016__c/dfa$Total_Giving__c

which(dfa$givingfraction10 + dfa$givingfraction11 + dfa$givingfraction12 + dfa$givingfraction13 < .25)

dfa$givingfraction16 - (dfa$givingfraction10 + dfa$givingfraction11 + dfa$givingfraction12 + dfa$givingfraction13)

which(dfa$givingfraction16[dfa$givingfraction16 < 1] - (dfa$givingfraction10 + dfa$givingfraction11 + dfa$givingfraction12 + dfa$givingfraction13) > 0)

j = (dfa$Total_Giving_2016__c[dfa$givingfraction16 < 1] - dfa$Total_Giving_2015__c[dfa$givingfraction16 < 1]) + (dfa$Total_Giving_2015__c[dfa$givingfraction16 < 1] - dfa$Total_Giving_2014__c[dfa$givingfraction16 < 1]) + (dfa$Total_Giving_2014__c[dfa$givingfraction16 < 1] - dfa$Total_Giving_2013__c[dfa$givingfraction16 < 1])
jj = which(j>5000)

dfa$increasing = 0 
dfa$increasing[jj] = 1

View(dfa[,100:166])
#Accounts

dfa$hostcount = dfa$Num_of_Events_2012__c+dfa$Num_of_Events_2013__c + dfa$Num_of_Events_2014__c + dfa$Num_of_Events_2015__c + dfa$Num_of_Events_2016__c
recurring = which(dfa$hostcount > 2)
dfa$recurring = 0
dfa$recurring[which(dfa$hostcount > 2)] = 1

which(dfa$Num_of_Events_2007__c >0 & dfa$Num_of_Events_2008__c > 0 & dfa$Num_of_Events_2009__c > 0 & dfa$Num_of_Events_2010__c > 0 & dfa$Num_of_Events_2011__c > 0 & dfa$Num_of_Events_2012__c >0 & dfa$Num_of_Events_2013__c >0 & dfa$Num_of_Events_2014__c >0 & dfa$Num_of_Events_2015__c >0 & dfa$Num_of_Events_2016__c > 0)

lm(dfa$recurring ~ dfa$Account_Sub_Type__c + dfa$Annual_Report_Subscriber__c)


dfa$percentc11 = 0
dfa$percentc12 = 0
dfa$percentc13 = 0
dfa$percentc14 = 0
dfa$percentc15 = 0
dfa$percentc16 = 0

dfa2 = dfa
dfa = dfa2
dfa[244,] = NA

b = which(is.na(dfa$Total_Giving_2010__c))
dfa$Total_Giving_2010__c[b] = 0
b = which(is.na(dfa$Total_Giving_2011__c))
dfa$Total_Giving_2011__c[b] = 0
b = which(is.na(dfa$Total_Giving_2012__c))
dfa$Total_Giving_2012__c[b] = 0
b = which(is.na(dfa$Total_Giving_2013__c))
dfa$Total_Giving_2013__c[b] = 0
b = which(is.na(dfa$Total_Giving_2014__c))
dfa$Total_Giving_2014__c[b] = 0
b = which(is.na(dfa$Total_Giving_2015__c))
dfa$Total_Giving_2015__c[b] = 0
b = which(is.na(dfa$Total_Giving_2016__c))
dfa$Total_Giving_2016__c[b] = 0

#10
dfa$percentc1110 = (dfa$Total_Giving_2011__c - dfa$Total_Giving_2010__c)/(dfa$Total_Giving_2010__c )
dfa$percentc1210 = (dfa$Total_Giving_2012__c - dfa$Total_Giving_2010__c)/(dfa$Total_Giving_2010__c )
dfa$percentc1310 = (dfa$Total_Giving_2013__c - dfa$Total_Giving_2010__c)/(dfa$Total_Giving_2010__c )
dfa$percentc1410 = (dfa$Total_Giving_2014__c - dfa$Total_Giving_2010__c)/(dfa$Total_Giving_2010__c )
dfa$percentc1510 = (dfa$Total_Giving_2015__c - dfa$Total_Giving_2010__c)/(dfa$Total_Giving_2010__c )
dfa$percentc1610 = (dfa$Total_Giving_2016__c - dfa$Total_Giving_2010__c)/(dfa$Total_Giving_2010__c )
#11
dfa$percentc1011 = (dfa$Total_Giving_2010__c - dfa$Total_Giving_2011__c)/(dfa$Total_Giving_2011__c )
dfa$percentc1211 = (dfa$Total_Giving_2012__c - dfa$Total_Giving_2011__c)/(dfa$Total_Giving_2011__c )
dfa$percentc1311 = (dfa$Total_Giving_2013__c - dfa$Total_Giving_2011__c)/(dfa$Total_Giving_2011__c )
dfa$percentc1411 = (dfa$Total_Giving_2014__c - dfa$Total_Giving_2011__c)/(dfa$Total_Giving_2011__c )
dfa$percentc1511 = (dfa$Total_Giving_2015__c - dfa$Total_Giving_2011__c)/(dfa$Total_Giving_2011__c )
dfa$percentc1611 = (dfa$Total_Giving_2016__c - dfa$Total_Giving_2011__c)/(dfa$Total_Giving_2011__c )
#12
dfa$percentc1012 = (dfa$Total_Giving_2010__c - dfa$Total_Giving_2012__c)/(dfa$Total_Giving_2012__c)
dfa$percentc1112 = (dfa$Total_Giving_2011__c - dfa$Total_Giving_2012__c)/(dfa$Total_Giving_2012__c)
dfa$percentc1312 = (dfa$Total_Giving_2013__c - dfa$Total_Giving_2012__c)/(dfa$Total_Giving_2012__c)
dfa$percentc1412 = (dfa$Total_Giving_2014__c - dfa$Total_Giving_2012__c)/(dfa$Total_Giving_2012__c)
dfa$percentc1512 = (dfa$Total_Giving_2015__c - dfa$Total_Giving_2012__c)/(dfa$Total_Giving_2012__c)
dfa$percentc1612 = (dfa$Total_Giving_2016__c - dfa$Total_Giving_2012__c)/(dfa$Total_Giving_2012__c)
#13
dfa$percentc1013 = (dfa$Total_Giving_2010__c - dfa$Total_Giving_2013__c)/(dfa$Total_Giving_2013__c)
dfa$percentc1113 = (dfa$Total_Giving_2011__c - dfa$Total_Giving_2013__c)/(dfa$Total_Giving_2013__c)
dfa$percentc1213 = (dfa$Total_Giving_2012__c - dfa$Total_Giving_2013__c)/(dfa$Total_Giving_2013__c)
dfa$percentc1413 = (dfa$Total_Giving_2014__c - dfa$Total_Giving_2013__c)/(dfa$Total_Giving_2013__c)
dfa$percentc1513 = (dfa$Total_Giving_2015__c - dfa$Total_Giving_2013__c)/(dfa$Total_Giving_2013__c)
dfa$percentc1613 = (dfa$Total_Giving_2016__c - dfa$Total_Giving_2013__c)/(dfa$Total_Giving_2013__c)
#14
dfa$percentc1014 = (dfa$Total_Giving_2010__c - dfa$Total_Giving_2014__c)/(dfa$Total_Giving_2014__c )
dfa$percentc1114 = (dfa$Total_Giving_2011__c - dfa$Total_Giving_2014__c)/(dfa$Total_Giving_2014__c )
dfa$percentc1214 = (dfa$Total_Giving_2012__c - dfa$Total_Giving_2014__c)/(dfa$Total_Giving_2014__c )
dfa$percentc1314 = (dfa$Total_Giving_2013__c - dfa$Total_Giving_2014__c)/(dfa$Total_Giving_2014__c )
dfa$percentc1514 = (dfa$Total_Giving_2015__c - dfa$Total_Giving_2014__c)/(dfa$Total_Giving_2014__c )
dfa$percentc1614 = (dfa$Total_Giving_2016__c - dfa$Total_Giving_2014__c)/(dfa$Total_Giving_2014__c )
#15
dfa$percentc1015 = (dfa$Total_Giving_2010__c - dfa$Total_Giving_2015__c)/(dfa$Total_Giving_2015__c )
dfa$percentc1115 = (dfa$Total_Giving_2011__c - dfa$Total_Giving_2015__c)/(dfa$Total_Giving_2015__c )
dfa$percentc1215 = (dfa$Total_Giving_2012__c - dfa$Total_Giving_2015__c)/(dfa$Total_Giving_2015__c )
dfa$percentc1315 = (dfa$Total_Giving_2013__c - dfa$Total_Giving_2015__c)/(dfa$Total_Giving_2015__c )
dfa$percentc1415 = (dfa$Total_Giving_2014__c - dfa$Total_Giving_2015__c)/(dfa$Total_Giving_2015__c )
dfa$percentc1615 = (dfa$Total_Giving_2016__c - dfa$Total_Giving_2015__c)/(dfa$Total_Giving_2015__c )
#16
dfa$percentc1016 = (dfa$Total_Giving_2010__c - dfa$Total_Giving_2016__c)/(dfa$Total_Giving_2016__c )
dfa$percentc1116 = (dfa$Total_Giving_2011__c - dfa$Total_Giving_2016__c)/(dfa$Total_Giving_2016__c )
dfa$percentc1216 = (dfa$Total_Giving_2012__c - dfa$Total_Giving_2016__c)/(dfa$Total_Giving_2016__c )
dfa$percentc1316 = (dfa$Total_Giving_2013__c - dfa$Total_Giving_2016__c)/(dfa$Total_Giving_2016__c )
dfa$percentc1416 = (dfa$Total_Giving_2014__c - dfa$Total_Giving_2016__c)/(dfa$Total_Giving_2016__c )
dfa$percentc1516 = (dfa$Total_Giving_2015__c - dfa$Total_Giving_2016__c)/(dfa$Total_Giving_2016__c )

#NA 10
b = which(dfa$percentc1110 == "Inf" |dfa$percentc1110 ==  "NaN")
dfa$percentc1110[b] = NA
b = which(dfa$percentc1110 == -Inf)
dfa$percentc1610[b] = NA

b = which(dfa$percentc1210 == "Inf"|dfa$percentc1210 ==  "NaN")
dfa$percentc1210[b] = NA
b = which(dfa$percentc1310 == "Inf"|dfa$percentc1310 ==  "NaN")
dfa$percentc1310[b] = NA
b = which(dfa$percentc1410 == "Inf"|dfa$percentc1410 ==  "NaN")
dfa$percentc1410[b] = NA
b = which(dfa$percentc1510 == "Inf"|dfa$percentc1510 ==  "NaN")
dfa$percentc1510[b] = NA
b = which(dfa$percentc1610 == "Inf"|dfa$percentc1610 ==  "NaN")
dfa$percentc1610[b] = NA
b = which(dfa$percentc1610 == -Inf)
dfa$percentc1610[b] = NA

#NA 11
b = which(dfa$percentc1011 == "Inf"|dfa$percentc1011 ==  "NaN")
dfa$percentc1011[b] = NA
b = which(dfa$percentc1211 == "Inf"|dfa$percentc1211 ==  "NaN")
dfa$percentc1211[b] = NA
b = which(dfa$percentc1311 == "Inf"|dfa$percentc1311 ==  "NaN")
dfa$percentc1311[b] = NA
b = which(dfa$percentc1411 == "Inf"|dfa$percentc1411 ==  "NaN")
dfa$percentc1411[b] = NA
b = which(dfa$percentc1511 == "Inf"|dfa$percentc1511 ==  "NaN")
dfa$percentc1511[b] = NA
b = which(dfa$percentc1611 == "Inf"|dfa$percentc1611 ==  "NaN")
dfa$percentc1611[b] = NA
b = which(dfa$percentc1611 == -Inf)
dfa$percentc1611[b] = NA


#NA 12
b = which(dfa$percentc1012 == "Inf"|dfa$percentc1012 ==  "NaN")
dfa$percentc1012[b] = NA
b = which(dfa$percentc1112 == "Inf"|dfa$percentc1112 ==  "NaN")
dfa$percentc1112[b] = NA
b = which(dfa$percentc1112 == -Inf)
dfa$percentc1112[b] = NA
b = which(dfa$percentc1312 == "Inf"|dfa$percentc1312 ==  "NaN")
dfa$percentc1312[b] = NA
b = which(dfa$percentc1412 == "Inf"|dfa$percentc1412 ==  "NaN")
dfa$percentc1412[b] = NA
b = which(dfa$percentc1512 == "Inf"|dfa$percentc1512 ==  "NaN")
dfa$percentc1512[b] = NA
b = which(dfa$percentc1612 == "Inf"|dfa$percentc1612 ==  "NaN")
dfa$percentc1612[b] = NA
b = which(dfa$percentc1612 == -Inf)
dfa$percentc1612[b] = NA

#NA 13
b = which(dfa$percentc1013 == "Inf"|dfa$percentc1013 ==  "NaN")
dfa$percentc1013[b] = NA
b = which(dfa$percentc1113 == "Inf"|dfa$percentc1113 ==  "NaN")
dfa$percentc1113[b] = NA
b = which(dfa$percentc1113 == -Inf)
dfa$percentc1113[b] = NA
b = which(dfa$percentc1213 == "Inf"|dfa$percentc1213 ==  "NaN")
dfa$percentc1213[b] = NA
b = which(dfa$percentc1413 == "Inf"|dfa$percentc1413 ==  "NaN")
dfa$percentc1413[b] = NA
b = which(dfa$percentc1513 == "Inf"|dfa$percentc1513 ==  "NaN")
dfa$percentc1513[b] = NA
b = which(dfa$percentc1613 == "Inf"|dfa$percentc1613 ==  "NaN")
dfa$percentc1613[b] = NA
b = which(dfa$percentc1613 == -Inf)
dfa$percentc1613[b] = NA

#NA 14
b = which(dfa$percentc1014 == "Inf"|dfa$percentc1014 ==  "NaN")
dfa$percentc1014[b] = NA
b = which(dfa$percentc1114 == "Inf"|dfa$percentc1114 ==  "NaN")
dfa$percentc1114[b] = NA
b = which(dfa$percentc1114 == -Inf)
dfa$percentc1114[b] = NA
b = which(dfa$percentc1214 == "Inf"|dfa$percentc1214 ==  "NaN")
dfa$percentc1214[b] = NA
b = which(dfa$percentc1314 == "Inf"|dfa$percentc1314 ==  "NaN")
dfa$percentc1314[b] = NA
b = which(dfa$percentc1514 == "Inf"|dfa$percentc1514 ==  "NaN")
dfa$percentc1514[b] = NA
b = which(dfa$percentc1614 == "Inf"|dfa$percentc1614 ==  "NaN")
dfa$percentc1614[b] = NA
b = which(dfa$percentc1614 == -Inf)
dfa$percentc1614[b] = NA

#NA 15
b = which(dfa$percentc1015 == "Inf"|dfa$percentc1015 ==  "NaN")
dfa$percentc1015[b] = NA
b = which(dfa$percentc1115 == "Inf"|dfa$percentc1115 ==  "NaN")
dfa$percentc1115[b] = NA
b = which(dfa$percentc1115 == -Inf)
dfa$percentc1115[b] = NA
b = which(dfa$percentc1215 == "Inf"|dfa$percentc1215 ==  "NaN")
dfa$percentc1215[b] = NA
b = which(dfa$percentc1315 == "Inf"|dfa$percentc1315 ==  "NaN")
dfa$percentc1315[b] = NA
b = which(dfa$percentc1415 == "Inf"|dfa$percentc1415 ==  "NaN")
dfa$percentc1415[b] = NA
b = which(dfa$percentc1615 == "Inf"|dfa$percentc1615 ==  "NaN")
dfa$percentc1615[b] = NA
b = which(dfa$percentc1615 == -Inf)
dfa$percentc1615[b] = NA

#NA 16
b = which(dfa$percentc1016 == "Inf"|dfa$percentc1016 ==  "NaN")
dfa$percentc1016[b] = NA
b = which(dfa$percentc1116 == "Inf"|dfa$percentc1116 ==  "NaN")
dfa$percentc1116[b] = NA
b = which(dfa$percentc1116 == -Inf)
dfa$percentc1116[b] = NA
b = which(dfa$percentc1216 == "Inf"|dfa$percentc1216 ==  "NaN")
dfa$percentc1216[b] = NA
b = which(dfa$percentc1316 == "Inf"|dfa$percentc1316 ==  "NaN")
dfa$percentc1316[b] = NA
b = which(dfa$percentc1416 == "Inf"|dfa$percentc1416 ==  "NaN")
dfa$percentc1416[b] = NA
b = which(dfa$percentc1516 == "Inf"|dfa$percentc1516 ==  "NaN")
dfa$percentc1516[b] = NA


#10
mean(dfa$percentc1110[is.na(dfa$percentc1110) == FALSE])
mean(dfa$percentc1210[is.na(dfa$percentc1210) == FALSE])
mean(dfa$percentc1310[is.na(dfa$percentc1310) == FALSE])
mean(dfa$percentc1410[is.na(dfa$percentc1410) == FALSE])
mean(dfa$percentc1510[is.na(dfa$percentc1510) == FALSE])
mean(dfa$percentc1610[is.na(dfa$percentc1610) == FALSE])
#11
mean(dfa$percentc1011[is.na(dfa$percentc1011) == FALSE])
mean(dfa$percentc1211[is.na(dfa$percentc1211) == FALSE])
mean(dfa$percentc1311[is.na(dfa$percentc1311) == FALSE])
mean(dfa$percentc1411[is.na(dfa$percentc1411) == FALSE])
mean(dfa$percentc1511[is.na(dfa$percentc1511) == FALSE])
mean(dfa$percentc1611[is.na(dfa$percentc1611) == FALSE])
#12
mean(dfa$percentc1012[is.na(dfa$percentc1012) == FALSE])
mean(dfa$percentc1112[is.na(dfa$percentc1112) == FALSE])
mean(dfa$percentc1312[is.na(dfa$percentc1312) == FALSE])
mean(dfa$percentc1412[is.na(dfa$percentc1412) == FALSE])
mean(dfa$percentc1512[is.na(dfa$percentc1512) == FALSE])
mean(dfa$percentc1612[is.na(dfa$percentc1612) == FALSE])
#13
mean(dfa$percentc1013[is.na(dfa$percentc1013) == FALSE])
mean(dfa$percentc1113[is.na(dfa$percentc1113) == FALSE])
mean(dfa$percentc1213[is.na(dfa$percentc1213) == FALSE])
mean(dfa$percentc1413[is.na(dfa$percentc1413) == FALSE])
mean(dfa$percentc1513[is.na(dfa$percentc1513) == FALSE])
mean(dfa$percentc1613[is.na(dfa$percentc1613) == FALSE])
#14
mean(dfa$percentc1014[is.na(dfa$percentc1014) == FALSE])
mean(dfa$percentc1114[is.na(dfa$percentc1114) == FALSE])
mean(dfa$percentc1214[is.na(dfa$percentc1214) == FALSE])
mean(dfa$percentc1314[is.na(dfa$percentc1314) == FALSE])
mean(dfa$percentc1514[is.na(dfa$percentc1514) == FALSE])
mean(dfa$percentc1614[is.na(dfa$percentc1614) == FALSE])
#15
mean(dfa$percentc1015[is.na(dfa$percentc1015) == FALSE])
mean(dfa$percentc1115[is.na(dfa$percentc1115) == FALSE])
mean(dfa$percentc1215[is.na(dfa$percentc1215) == FALSE])
mean(dfa$percentc1315[is.na(dfa$percentc1315) == FALSE])
mean(dfa$percentc1415[is.na(dfa$percentc1415) == FALSE])
mean(dfa$percentc1615[is.na(dfa$percentc1615) == FALSE])
#16
mean(dfa$percentc1016[is.na(dfa$percentc1016) == FALSE])
mean(dfa$percentc1116[is.na(dfa$percentc1116) == FALSE])
mean(dfa$percentc1216[is.na(dfa$percentc1216) == FALSE])
mean(dfa$percentc1316[is.na(dfa$percentc1316) == FALSE])
mean(dfa$percentc1416[is.na(dfa$percentc1416) == FALSE])
mean(dfa$percentc1516[is.na(dfa$percentc1516) == FALSE])




colnames(dfa)
count = 1
d = vector()
for (i in 174:215){
  d[count] = mean(dfa[which(!is.na(dfa[,i]) & dfa[,i] != "NaN" & dfa[,i] != -Inf),i])
  count = count + 1
}
d = c(0,d)
d = c(d[1:8],0,d[9:length(d)])
d = c(d[1:16],0,d[17:length(d)])
d = c(d[1:24],0,d[25:length(d)])
d = c(d[1:32],0,d[33:length(d)])
d = c(d[1:40],0,d[41:length(d)])
d = c(d[1:48],0,d[49:length(d)])
d = d[1:49]

#d = c(0,-0.5304782,-0.3164338,-0.2611929, 0.07412919,0.1273901,-0.09636934,-0.405149,0,2.486172,8.378351, -0.4163933, -0.2501038,-0.2569831, -0.4804073,-0.6555196,0,-0.4285957,-0.5075605,-0.3585653,-0.5645015,-0.8222199,-0.8494116,-0.6005713,0,-0.5464216,-0.3206925, -0.2971308,-0.8195312,-0.6708261,-0.5695291, -0.3049303, 0 , -0.121404,0.9706727,-0.8123537, -0.8308333 ,-0.8174345,-0.7286622, -0.5620708,0,-0.2073679,-0.9074092, -0.9040589 ,-0.8399473, -0.6780766, -0.6812513,-0.4483383,0)            




c(0,mean(dfa$percentc1110[is.na(dfa$percentc1110) == FALSE]),mean(dfa$percentc1210[is.na(dfa$percentc1210) == FALSE]),mean(dfa$percentc1310[is.na(dfa$percentc1310) == FALSE]),mean(dfa$percentc1410[is.na(dfa$percentc1410) == FALSE]),mean(dfa$percentc1510[is.na(dfa$percentc1510) == FALSE]),mean(dfa$percentc1610[is.na(dfa$percentc1610) == FALSE]),mean(dfa$percentc1011[is.na(dfa$percentc1011) == FALSE]),mean(dfa$percentc1211[is.na(dfa$percentc1211) == FALSE]),mean(dfa$percentc1311[is.na(dfa$percentc1311) == FALSE]),mean(dfa$percentc1411[is.na(dfa$percentc1411) == FALSE]),mean(dfa$percentc1511[is.na(dfa$percentc1511) == FALSE]),mean(dfa$percentc1611[is.na(dfa$percentc1611) == FALSE]),mean(dfa$percentc1012[is.na(dfa$percentc1012) == FALSE]),mean(dfa$percentc1112[is.na(dfa$percentc1112) == FALSE]),mean(dfa$percentc1312[is.na(dfa$percentc1312) == FALSE]),mean(dfa$percentc1412[is.na(dfa$percentc1412) == FALSE]),mean(dfa$percentc1512[is.na(dfa$percentc1512) == FALSE]),mean(dfa$percentc1612[is.na(dfa$percentc1612) == FALSE]),mean(dfa$percentc1013[is.na(dfa$percentc1013) == FALSE]),mean(dfa$percentc1113[is.na(dfa$percentc1113) == FALSE]),mean(dfa$percentc1213[is.na(dfa$percentc1213) == FALSE]),mean(dfa$percentc1413[is.na(dfa$percentc1413) == FALSE]),mean(dfa$percentc1513[is.na(dfa$percentc1513) == FALSE])mean(dfa$percentc1613[is.na(dfa$percentc1613) == FALSE]),mean(dfa$percentc1014[is.na(dfa$percentc1014) == FALSE])],mean(dfa$percentc1114[is.na(dfa$percentc1114) == FALSE]),mean(dfa$percentc1214[is.na(dfa$percentc1214) == FALSE]),mean(dfa$percentc1314[is.na(dfa$percentc1314) == FALSE]),mean(dfa$percentc1514[is.na(dfa$percentc1514) == FALSE]),mean(dfa$percentc1614[is.na(dfa$percentc1614) == FALSE]),mean(dfa$percentc1015[is.na(dfa$percentc1015) == FALSE]),mean(dfa$percentc1115[is.na(dfa$percentc1115) == FALSE]),mean(dfa$percentc1215[is.na(dfa$percentc1215) == FALSE]),mean(dfa$percentc1315[is.na(dfa$percentc1315) == FALSE]),mean(dfa$percentc1415[is.na(dfa$percentc1415) == FALSE]),mean(dfa$percentc1615[is.na(dfa$percentc1615) == FALSE]),mean(dfa$percentc1016[is.na(dfa$percentc1016) == FALSE]),mean(dfa$percentc1116[is.na(dfa$percentc1116) == FALSE]),mean(dfa$percentc1216[is.na(dfa$percentc1216) == FALSE]),mean(dfa$percentc1316[is.na(dfa$percentc1316) == FALSE]),mean(dfa$percentc1416[is.na(dfa$percentc1416) == FALSE])mean(dfa$percentc1516[is.na(dfa$percentc1516) == FALSE])







View(dfa[,100:169])
View(dfa2[,100:169])


dfa$averagegrowth = (dfa$percentc11 + dfa$percentc12 + dfa$percentc13 + dfa$percentc14 + dfa$percentc15 + dfa$percentc16)/6

#could create an indicator of whether individual donated the previous year then multiply that indicator by current year to specify whether indivdiual donated
#dfa$averagegrowth = (dfa$percentc11 + dfa$percentc12 + dfa$percentc13 + dfa$percentc14 + dfa$percentc15 + dfa$percentc16)/6

View(dfa[,100:170])

x = which(is.na(dfa$Total_Giving_2010__c))
dfa$Total_Giving_2010__c[x] = 0
x = which(is.na(dfa$Total_Giving_2011__c))
dfa$Total_Giving_2010__c[x] = 0
x = which(is.na(dfa$Total_Giving_2012__c))
dfa$Total_Giving_2010__c[x] = 0
x = which(is.na(dfa$Total_Giving_2013__c))
dfa$Total_Giving_2010__c[x] = 0
x = which(is.na(dfa$Total_Giving_2014__c))
dfa$Total_Giving_2010__c[x] = 0
x = which(is.na(dfa$Total_Giving_2015__c))
dfa$Total_Giving_2010__c[x] = 0
x = which(is.na(dfa$Total_Giving_2016__c))
dfa$Total_Giving_2010__c[x] = 0


sumdfa$tota

# % change 2010  
(sum(dfa$Total_Giving_2011__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2010__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2010__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2012__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2010__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2010__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2013__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2010__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2010__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2014__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2010__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2010__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2015__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2010__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2010__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2016__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2010__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2010__c, na.rm = TRUE)

# % change 2011
(sum(dfa$Total_Giving_2010__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2011__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2011__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2012__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2011__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2011__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2013__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2011__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2011__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2014__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2011__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2011__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2015__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2011__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2011__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2016__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2011__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2011__c, na.rm = TRUE)



# % change 2012 
(sum(dfa$Total_Giving_2010__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2012__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2012__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2011__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2012__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2012__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2013__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2012__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2012__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2014__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2012__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2012__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2015__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2012__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2012__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2016__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2012__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2012__c, na.rm = TRUE)


# % change 2013
(sum(dfa$Total_Giving_2010__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2013__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2013__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2011__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2013__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2013__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2012__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2013__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2013__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2014__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2013__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2013__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2015__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2013__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2013__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2016__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2013__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2013__c, na.rm = TRUE)



# % change 2014
(sum(dfa$Total_Giving_2010__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2014__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2014__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2011__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2014__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2014__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2012__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2014__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2014__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2013__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2014__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2014__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2015__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2014__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2014__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2016__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2014__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2014__c, na.rm = TRUE)


# % change 2015
(sum(dfa$Total_Giving_2010__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2015__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2015__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2011__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2015__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2015__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2012__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2015__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2015__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2013__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2015__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2015__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2014__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2015__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2015__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2016__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2015__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2015__c, na.rm = TRUE)

# % change 2016
(sum(dfa$Total_Giving_2010__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2016__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2016__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2011__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2016__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2016__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2012__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2016__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2016__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2013__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2016__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2016__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2014__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2016__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2016__c, na.rm = TRUE)

(sum(dfa$Total_Giving_2015__c, na.rm = TRUE) -  sum(dfa$Total_Giving_2016__c, na.rm = TRUE)) /  sum(dfa$Total_Giving_2016__c, na.rm = TRUE)


#GDP 2010 % change 2.53142
#GDP 2011 % change 1.60175
#GDP 2012 % change 1.67767
#GDP 2013 % change 2.37058
#GDP 2014 % change 2.596
#GDP 2015 % change 1.61552

#10 11 12 13 14 15 16


#Percent Change Year by Year totals
percentagechangeyears = matrix(c(0,0.08896774,0.5921487, 1.502729, 2.019665, 3.150771, 3.564955,-0.08169915,0,0.4620715,1.298258,1.772961,2.811656,3.192002,-0.371918,-0.3160389,0,0.5719191,0.8965975,1.607025,1.867166,-0.6004361,-0.5648879,-0.3638349,0,0.206549,0.658498,0.8239908,-0.6688375, -0.6393747,-0.47274,-0.1711899,0,0.3745799,0.511742,-0.7590809,-0.7376469,-0.616421, -0.3970448,-0.272505,0,0.09978477, -0.8588657,-0.8374316,-0.7162057, -0.4968296,-0.3722898,-0.09978477,0 ),7,7)
a = c("10","11","12","13","14","15","16")
row.names(percentagechangeyears) = a
colnames(percentagechangeyears) = a
percentagechangeyears

hist(percentagechangeyears, breaks = 100, include.lowest = TRUE)

#Average Persons % change year by year
averagepecercentchange = matrix(d,7,7)
row.names(averagepecercentchange) = a
colnames(averagepecercentchange) = a

dfa$nodonate10 = 0
dfa$nodonate10[dfa$Total_Giving_2010__c == 0] = 1
dfa$nodonate10[dfa$Total_Giving_2010__c == NA] = 1
dfa$nodonate11 = 0
dfa$nodonate11[dfa$Total_Giving_2011__c == 0] = 1
dfa$nodonate11[dfa$Total_Giving_2011__c == NA] = 1
dfa$nodonate12 = 0
dfa$nodonate12[dfa$Total_Giving_2012__c == 0] = 1
dfa$nodonate12[dfa$Total_Giving_2012__c == NA] = 1
dfa$nodonate13 = 0
dfa$nodonate13[dfa$Total_Giving_2013__c == 0] = 1
dfa$nodonate13[dfa$Total_Giving_2013__c == NA] = 1
dfa$nodonate14 = 0
dfa$nodonate14[dfa$Total_Giving_2014__c == 0] = 1
dfa$nodonate14[dfa$Total_Giving_2014__c == NA] = 1
dfa$nodonate15 = 0
dfa$nodonate15[dfa$Total_Giving_2015__c == 0] = 1
dfa$nodonate15[dfa$Total_Giving_2015__c == NA] = 1
dfa$nodonate16 = 0
dfa$nodonate16[dfa$Total_Giving_2016__c == 0] = 1
dfa$nodonate16[dfa$Total_Giving_2016__c == NA] = 1


#previous 0 and NA sums
#first time donor 11
a0 = sum(dfa$Total_Giving_2010__c * dfa$nodonate10)
a1 = sum(dfa$Total_Giving_2011__c * dfa$nodonate10)
a2 = sum(dfa$Total_Giving_2012__c * dfa$nodonate10)
a3 = sum(dfa$Total_Giving_2013__c * dfa$nodonate10)
a4 =sum(dfa$Total_Giving_2014__c * dfa$nodonate10)
a5 =sum(dfa$Total_Giving_2015__c * dfa$nodonate10)
a6 =sum(dfa$Total_Giving_2016__c * dfa$nodonate10)

#first time donor 12
a7 = sum(dfa$Total_Giving_2010__c * dfa$nodonate11 * dfa$nodonate10)
a8 = sum(dfa$Total_Giving_2011__c * dfa$nodonate11 * dfa$nodonate10)
a9 = sum(dfa$Total_Giving_2012__c * dfa$nodonate11 * dfa$nodonate10)
a10 =sum(dfa$Total_Giving_2013__c * dfa$nodonate11 * dfa$nodonate10)
a11= sum(dfa$Total_Giving_2014__c * dfa$nodonate11* dfa$nodonate10)
a12 =sum(dfa$Total_Giving_2015__c * dfa$nodonate11* dfa$nodonate10)
a13 = sum(dfa$Total_Giving_2016__c * dfa$nodonate11* dfa$nodonate10)
#first time donor 13
a14 =sum(dfa$Total_Giving_2010__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a15 = sum(dfa$Total_Giving_2011__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a16 = sum(dfa$Total_Giving_2012__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a17 = sum(dfa$Total_Giving_2013__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a18 = sum(dfa$Total_Giving_2014__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a19 = sum(dfa$Total_Giving_2015__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a20 = sum(dfa$Total_Giving_2016__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
#first time donor 14
a21 = sum(dfa$Total_Giving_2010__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a22 = sum(dfa$Total_Giving_2011__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a23 = sum(dfa$Total_Giving_2012__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a24 = sum(dfa$Total_Giving_2013__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a25 = sum(dfa$Total_Giving_2014__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a26 = sum(dfa$Total_Giving_2015__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a27 = sum(dfa$Total_Giving_2016__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
#first time donor 15
a28 = sum(dfa$Total_Giving_2010__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a29 = sum(dfa$Total_Giving_2011__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a30 = sum(dfa$Total_Giving_2012__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a31 = sum(dfa$Total_Giving_2013__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a32 = sum(dfa$Total_Giving_2014__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a33 = sum(dfa$Total_Giving_2015__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a34 = sum(dfa$Total_Giving_2016__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
#first time donor 16
a35 = sum(dfa$Total_Giving_2010__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a36 = sum(dfa$Total_Giving_2011__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a37 = sum(dfa$Total_Giving_2012__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a38 = sum(dfa$Total_Giving_2013__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a39 = sum(dfa$Total_Giving_2014__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a40 = sum(dfa$Total_Giving_2015__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a41 = sum(dfa$Total_Giving_2016__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)

avector = c(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32,a33,a34,a35,a36,a37,a38,a39,a40,a41)
amatrix = matrix(avector,7,6)

#previous 0 and NA sums
#first time donor 11
a0 = mean(dfa$Total_Giving_2010__c * dfa$nodonate10)
a1 = mean(dfa$Total_Giving_2011__c * dfa$nodonate10)
a2 = mean(dfa$Total_Giving_2012__c * dfa$nodonate10)
a3 = mean(dfa$Total_Giving_2013__c * dfa$nodonate10)
a4 =mean(dfa$Total_Giving_2014__c * dfa$nodonate10)
a5 =mean(dfa$Total_Giving_2015__c * dfa$nodonate10)
a6 =mean(dfa$Total_Giving_2016__c * dfa$nodonate10)

#first time donor 12
a7 = mean(dfa$Total_Giving_2010__c * dfa$nodonate11 * dfa$nodonate10)
a8 = mean(dfa$Total_Giving_2011__c * dfa$nodonate11 * dfa$nodonate10)
a9 = mean(dfa$Total_Giving_2012__c * dfa$nodonate11 * dfa$nodonate10)
a10 =mean(dfa$Total_Giving_2013__c * dfa$nodonate11 * dfa$nodonate10)
a11= mean(dfa$Total_Giving_2014__c * dfa$nodonate11* dfa$nodonate10)
a12 =mean(dfa$Total_Giving_2015__c * dfa$nodonate11* dfa$nodonate10)
a13 = mean(dfa$Total_Giving_2016__c * dfa$nodonate11* dfa$nodonate10)
#first time donor 13
a14 =mean(dfa$Total_Giving_2010__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a15 = mean(dfa$Total_Giving_2011__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a16 = mean(dfa$Total_Giving_2012__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a17 = mean(dfa$Total_Giving_2013__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a18 = mean(dfa$Total_Giving_2014__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a19 = mean(dfa$Total_Giving_2015__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a20 = mean(dfa$Total_Giving_2016__c * dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
#first time donor 14
a21 = mean(dfa$Total_Giving_2010__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a22 = mean(dfa$Total_Giving_2011__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a23 = mean(dfa$Total_Giving_2012__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a24 = mean(dfa$Total_Giving_2013__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a25 = mean(dfa$Total_Giving_2014__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a26 = mean(dfa$Total_Giving_2015__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a27 = mean(dfa$Total_Giving_2016__c * dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
#first time donor 15
a28 = mean(dfa$Total_Giving_2010__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a29 = mean(dfa$Total_Giving_2011__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a30 = mean(dfa$Total_Giving_2012__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a31 = mean(dfa$Total_Giving_2013__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a32 = mean(dfa$Total_Giving_2014__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a33 = mean(dfa$Total_Giving_2015__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a34 = mean(dfa$Total_Giving_2016__c * dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
#first time donor 16
a35 = mean(dfa$Total_Giving_2010__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a36 = mean(dfa$Total_Giving_2011__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a37 = mean(dfa$Total_Giving_2012__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a38 = mean(dfa$Total_Giving_2013__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a39 = mean(dfa$Total_Giving_2014__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a40 = mean(dfa$Total_Giving_2015__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)
a41 = mean(dfa$Total_Giving_2016__c * dfa$nodonate15* dfa$nodonate14* dfa$nodonate13* dfa$nodonate12* dfa$nodonate11* dfa$nodonate10)

meanvector = c(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32,a33,a34,a35,a36,a37,a38,a39,a40,a41)
meanmatrix = matrix(meanvector,7,6)
names = c(11:16)
colnames(meanmatrix) = names
names=c(10:16)
row.names(meanmatrix) = names

john = c(0,sum(dfa$Total_Giving_2011__c * dfa$nodonate10),sum(dfa$Total_Giving_2012__c * dfa$nodonate10),sum(dfa$Total_Giving_2013__c * dfa$nodonate10),sum(dfa$Total_Giving_2014__c * dfa$nodonate10),sum(dfa$Total_Giving_2015__c * dfa$nodonate10),sum(dfa$Total_Giving_2016__c * dfa$nodonate10),sum(dfa$Total_Giving_2010__c * dfa$nodonate11),0,sum(dfa$Total_Giving_2012__c * dfa$nodonate11),sum(dfa$Total_Giving_2013__c * dfa$nodonate11),sum(dfa$Total_Giving_2014__c * dfa$nodonate11),sum(dfa$Total_Giving_2015__c * dfa$nodonate11),sum(dfa$Total_Giving_2016__c * dfa$nodonate11),sum(dfa$Total_Giving_2010__c * dfa$nodonate12),sum(dfa$Total_Giving_2011__c * dfa$nodonate12),0,sum(dfa$Total_Giving_2013__c * dfa$nodonate12),sum(dfa$Total_Giving_2014__c * dfa$nodonate12),sum(dfa$Total_Giving_2015__c * dfa$nodonate12),sum(dfa$Total_Giving_2016__c * dfa$nodonate12),sum(dfa$Total_Giving_2010__c * dfa$nodonate13),sum(dfa$Total_Giving_2011__c * dfa$nodonate13),sum(dfa$Total_Giving_2012__c * dfa$nodonate13),0,sum(dfa$Total_Giving_2014__c * dfa$nodonate13),sum(dfa$Total_Giving_2015__c * dfa$nodonate13),sum(dfa$Total_Giving_2016__c * dfa$nodonate13),sum(dfa$Total_Giving_2010__c * dfa$nodonate14),sum(dfa$Total_Giving_2011__c * dfa$nodonate14),sum(dfa$Total_Giving_2012__c * dfa$nodonate14),sum(dfa$Total_Giving_2013__c * dfa$nodonate14),0,sum(dfa$Total_Giving_2015__c * dfa$nodonate14),sum(dfa$Total_Giving_2016__c * dfa$nodonate14),sum(dfa$Total_Giving_2010__c * dfa$nodonate15),sum(dfa$Total_Giving_2011__c * dfa$nodonate15),sum(dfa$Total_Giving_2012__c * dfa$nodonate15),sum(dfa$Total_Giving_2013__c * dfa$nodonate15),sum(dfa$Total_Giving_2014__c * dfa$nodonate15),0,sum(dfa$Total_Giving_2016__c * dfa$nodonate15),sum(dfa$Total_Giving_2010__c * dfa$nodonate16),sum(dfa$Total_Giving_2011__c * dfa$nodonate16),sum(dfa$Total_Giving_2012__c * dfa$nodonate16),sum(dfa$Total_Giving_2013__c * dfa$nodonate16),sum(dfa$Total_Giving_2014__c * dfa$nodonate16),sum(dfa$Total_Giving_2015__c * dfa$nodonate16),sum(dfa$Total_Giving_2016__c * dfa$nodonate16))
john = matrix(john,7,7)
Dobson = c(10:16)
colnames(john) = Dobson
Dobson = c(10:16)
row.names(john) = Dobson


#sum of new donations in 2011
sum(dfa$Total_Giving_2011__c * dfa$nodonate10)
#sum of new donations in 2012
sum(dfa$Total_Giving_2012__c * dfa$nodonate10 * dfa$nodonate11)
#sum of new donations in 2013
sum(dfa$Total_Giving_2013__c * dfa$nodonate10 * dfa$nodonate11 * dfa$nodonate12)
#sum of new donations in 2014
sum(dfa$Total_Giving_2014__c * dfa$nodonate10 * dfa$nodonate11 * dfa$nodonate12 * dfa$nodonate13)
#sum of new donations in 2015
sum(dfa$Total_Giving_2015__c * dfa$nodonate10 * dfa$nodonate11 * dfa$nodonate12 * dfa$nodonate13 * dfa$nodonate14)
#sum of new donations in 2016
sum(dfa$Total_Giving_2016__c * dfa$nodonate10 * dfa$nodonate11 * dfa$nodonate12 * dfa$nodonate13 * dfa$nodonate14 * dfa$nodonate15)


#mod = lm(dfa$Total_Giving__c ~ dfa$nodonate10*dfa$nodonate11*dfa$nodonate12*dfa$nodonate13*dfa$nodonate14*dfa$nodonate15 + dfa$nodonate10*dfa$nodonate11*dfa$nodonate12*dfa$nodonate13*dfa$nodonate14*dfa$nodonate15*dfa$Account_Type__c)
#summary(mod)

View(john)

ATraining = sample(1:length(dfa$AccountNumber),size = floor(.8*length(dfa$AccountNumber)),replace = FALSE)
dfatraining = dfa[ATraining,]

dfaTest = (dfa[-(ATraining),])

#trainmod = lm(dfatraining$Total_Giving__c ~ dfatraining$nodonate10*dfatraining$nodonate11*dfatraining$nodonate12*dfatraining$nodonate13*dfatraining$nodonate14*dfatraining$nodonate15 + dfatraining$nodonate10*dfatraining$nodonate11*dfatraining$nodonate12*dfatraining$nodonate13*dfatraining$nodonate14*dfatraining$nodonate15*dfatraining$Account_Type__c, rm.na = T)
#summary(trainmod)
#trainmod2 = lm(dfatraining$Total_Giving__c~dfatraining$nodonate10*dfatraining$nodonate11*dfatraining$nodonate12*dfatraining$nodonate13*dfatraining$nodonate14*dfatraining$nodonate15 + dfatraining$Account_Sub_Type__c + dfatraining$Total_Events__c + dfatraining$Total_Meals__c + dfatraining$Total_Meals__c*dfatraining$Total_Events__c)
#summary(trainmod2)
#p = predict(trainmod2, dfaTest, interval = "predict", rm.na = T)
#p2 = as.data.frame(p)
#x = na.omit(dfaTest$RECURRINGDONOR - p2$fit)


ATraining = sample(1:length(dfa$AccountNumber),size = floor(.8*length(dfa$AccountNumber)),replace = FALSE)
dfatraining = dfa[ATraining,]

dfaTest = (dfa[-(ATraining),])

#trainmod = lm(dfatraining$RECURRINGDONOR ~ dfatraining$nodonate10*dfatraining$nodonate11*dfatraining$nodonate12*dfatraining$nodonate13*dfatraining$nodonate14*dfatraining$nodonate15 + dfatraining$nodonate10*dfatraining$nodonate11*dfatraining$nodonate12*dfatraining$nodonate13*dfatraining$nodonate14*dfatraining$nodonate15*dfatraining$Account_Type__c, rm.na = T)
#summary(trainmod)
#trainmod2 = lm(dfatraining$RECURRINGDONOR~dfatraining$nodonate10*dfatraining$nodonate11*dfatraining$nodonate12*dfatraining$nodonate13*dfatraining$nodonate14*dfatraining$nodonate15 + dfatraining$Account_Sub_Type__c + dfatraining$Total_Events__c + dfatraining$Total_Meals__c + dfatraining$Total_Meals__c*dfatraining$Total_Events__c)
#summary(trainmod2)
#p = predict(trainmod2, dfaTest, interval = "predict", rm.na = T)
#p2 = as.data.frame(p)
x = na.omit(dfatraining$RECURRINGDONOR - p2$fit)




p2 = na.omit(p2)
#averagepecercentchange2 = data.frame(averagepecercentchange)
#for(i in 2:ncol(averagepecercentchange2)){
 # for(j in 1:nrow(averagepecercentchange2)){
  #  if(j < i){
      #averagepecercentchange2[j,i] = NA
   # }
#  }
#}

plot(2010:2016, averagepecercentchange[,1], col = 1, type = "l", ylim = c(min(averagepecercentchange), max(averagepecercentchange)))
for(i in 2:ncol(averagepecercentchange)){
  lines((2009+i):2016, averagepecercentchange[i:nrow(averagepecercentchange), i], col = i)
}


plot(2010:2016, percentagechangeyears[,1], col = 1, type = "l", ylim = c(min(percentagechangeyears), max(percentagechangeyears)))
for(i in 2:ncol(percentagechangeyears)){
  lines((2009+i):2016, percentagechangeyears[i:nrow(percentagechangeyears), i], col = i)
}

library('rio')
Names = colnames(dfa)
Names2 = gsub("_","",Names)
Names3 = gsub("rCGiving","rcg",Names2)
Names4 = gsub("PriorCalendarYear","PCY",Names3)
Names5 = gsub("rcgL","RCGl",Names4)
Names6 = gsub("Create","c",Names5)
Names = gsub("EventsandOpportunitiesc","EventsandOppsc", Names)
#colnames(dfa) = Names4
#export(dfa,'dfa.dta')
#write.csv(dfa, "dfa.csv")


#mod = lm(dfa$Total_Giving__c ~ dfa$nodonate10*dfa$nodonate11*dfa$nodonate12*dfa$nodonate13*dfa$nodonate14*dfa$nodonate15*dfa$nodonate16 + dfa$Total_Meals__c + dfa$Total_Events__c + dfa$First_Inkind_Hard_Credit_Amount__c, dfa$Account_Type__c)
#summary(mod)


sum(dfa$Total_Giving_2010__c, rm.na = T)
sum(dfa$Total_Giving_2011__c, rm.na = T)
sum(dfa$Total_Giving_2012__c, rm.na = T)
sum(dfa$Total_Giving_2013__c, rm.na = T)
sum(dfa$Total_Giving_2014__c, rm.na = T)
sum(dfa$Total_Giving_2015__c, rm.na = T)
sum(dfa$Total_Giving_2016__c, rm.na = T)
