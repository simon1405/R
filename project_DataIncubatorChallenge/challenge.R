##The data has been deleted rows contained NA by excel.
library(readxl)
ccrb_data_cleaned <- read_excel("C:/Users/simon/Downloads/ccrb_data_cleaned.xlsx", 
                                sheet = "Complaints_Allegations")
ccrb<-as.data.frame(ccrb_data_cleaned)
library("dplyr", lib.loc="~/R/win-library/3.4")

##
options(digits=15)
dim(table(ccrb[,2]))
##68467
table(ccrb[,5])/sum(table(ccrb[,5]))
max(table(ccrb[,5])/sum(table(ccrb[,5])))
##Brooklyn, with proportion 0.3536736004
colnames(ccrb)[12]<-"IY"
ccrb1<-ccrb[which(ccrb[,12]==2016),]
ccrb1.1<-table(ccrb1[,5])*100000
ccrb1.1[1]/1471160
ccrb1.1[2]/2648771
ccrb1.1[3]/1664727
ccrb1.1[6]/2358582
ccrb1.1[7]/479458
##Bronx,with 173.4005818538 per capita
ccrb$time<-ccrb$`Close Year`-ccrb$`Received Year`
ccrb2<-summarise(group_by(ccrb,UniqueComplaintId),d=mean(`Close Year`),s=mean(`Received Year`))
mean(ccrb2$d-ccrb2$s)
##it needs an average of 0.4743745162 year to close
ccrb3.1<-filter(ccrb,`Allegation Description`=="Frisk")
ccrb3.2<-filter(ccrb,`Allegation Description`=="Stop")
ccrb3.0<-rbind(ccrb3.1,ccrb3.2)
ccrb3<-summarise(group_by(ccrb3.0,UniqueComplaintId),d=mean(`IY`))
max(table(ccrb3$d))
y<-matrix(c(1727,1691, 1399, 1360, 1164,973,777,697,528),ncol=1)
x<-matrix(seq(2008,2016),ncol=1)
lmmodel<-lm(y~1+as.vector(x))
point <- data.frame(x=2018)
predict(lmmodel,newdata = point)
##it predicts that 205 complaints including stop and frisk in 2018
ccrb$full<-0
ccrb$full[which(ccrb$`Is Full Investigation`=="TRUE")]<-1
ccrb$video<-0
ccrb$video[which(ccrb$`Complaint Has Video Evidence`=="TRUE")]<-1
ccrb4<-summarise(group_by(ccrb,UniqueComplaintId),videoevidence=mean(video),fullinvest=mean(full))
table(ccrb4[,2:3])
chit<-chisq.test(table(ccrb4[,2:3]))
chit$statistic
##The chi-square statistics is 1312.0319808203
ccrb5<-cbind(ccrb$UniqueComplaintId,ccrb$`Allegation FADO Type`)
ccrb5.0<-ccrb5[!duplicated(ccrb5[,1:2], fromLast=TRUE), ] 
ccrb5<-as.data.frame(ccrb5.0)
ccrb5$num<-1

ccrb5$aoa<-0
ccrb5$aoa[which(ccrb5$V2=="Abuse of Authority")]<-1
ccrb5$dis<-0
ccrb5$dis[which(ccrb5$V2=="Discourtesy")]<-1
ccrb5$forc<-0
ccrb5$forc[which(ccrb5$V2=="Force")]<-1
ccrb5$ol<-0
ccrb5$ol[which(ccrb5$V2=="Offensive Language")]<-1
ccrb5.1<-summarise(group_by(ccrb5,V1),resu=sum(num),abu=sum(aoa),disco=sum(dis),force=sum(forc),offlan=sum(ol))
lm2<-lm(resu~abu+disco+force+offlan,data=ccrb5.1)
summary(lm2)
##The maximum of coefficient is 1
precincts<-c(12,22,23,16,4)
ccrb6<-ccrb[which(ccrb[,12]==2016),]
ccrb6<-select(ccrb6,UniqueComplaintId,`Borough of Occurrence`)
ccrb6.1<-ccrb6[!duplicated(ccrb6[,1:2], fromLast=TRUE), ] 
table(ccrb6.1[,2])[-4]
officerpprec<-table(ccrb6.1[,2])[-4]/sum(table(ccrb6.1[,2])[-4])*36000/precincts
max(officerpprec)/min(officerpprec)
##the ratio of the highest number of officers per precinct to the lowest number of officers per precinct is 2.5362694301

#####
N<-64
T<-5
s.save<-c()
rope<-c(0,0,0)
for (k in 1:100000){
N.temp<-N-1
for (i in 1:T){
  if (N.temp>=4){
  cut<-sample(2:N.temp-1,2,replace=F)
  if (cut[1]>cut[2]){rope[1]=cut[2]-1;rope[2]=cut[1]-cut[2];rope[3]=N.temp+1-cut[1]}
  else {rope[1]=cut[1]-1;rope[2]=cut[2]-cut[1];rope[3]=N.temp+1-cut[2]}
  }
  N.temp<-max(rope)
}
S<-N.temp
s.save<-c(s.save,S)
}
mean(s.save)
sd(s.save)
sum(s.save>8)/sum(s.save>4)
##
N<-1024
T<-10
s.save<-c()
rope<-c(0,0,0)
for (k in 1:100000){
  N.temp<-N-1
  for (i in 1:T){
    if (N.temp>=4){
      cut<-sample(2:N.temp-1,2,replace=F)
      if (cut[1]>cut[2]){rope[1]=cut[2]-1;rope[2]=cut[1]-cut[2];rope[3]=N.temp+1-cut[1]}
      else {rope[1]=cut[1]-1;rope[2]=cut[2]-cut[1];rope[3]=N.temp+1-cut[2]}
    }
    N.temp<-max(rope)
  }
  S<-N.temp
  s.save<-c(s.save,S)
}
mean(s.save)
sd(s.save)
sum(s.save>12)/sum(s.save>6)

####
library(readr)
bank_full <- read_delim("C:/Users/simon/Downloads/bank-full.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
dim(bank_full)
ba<-bank_full[,-c(9:11)]
for (i in 1:45211){
  for (j in 1:14){
    if (ba[i,j]=="unknown"){ba[i,j]<-NA}
  }
}
ba<-ba[complete.cases(ba),]
dim(ba)
ba$mari_1<-0
ba$mari_1[which(ba$marital=="married")]<-1
ba$mari_2<-0
ba$mari_2[which(ba$marital=="divorced")]<-1


ba$edu_1<-0
ba$edu_1[which(ba$education=="secondary")]<-1
ba$edu_2<-0
ba$edu_2[which(ba$education=="tertiary")]<-1

ba$def<-0
ba$def[which(ba$default=="yes")]<-1

ba$house<-0
ba$house[which(ba$housing=="yes")]<-1

ba$loa<-0
ba$loa[which(ba$loan=="yes")]<-1

ba$pout<-1
ba$pout[which(ba$poutcome=="failure")]<-0

ba$subs<-0        
ba$subs[which(ba$y=="yes")]<-1

##
lo<-glm(formula = subs~age+edu_1+edu_2+def+mari_1+mari_2+balance+house+loa+duration+campaign+pdays+previous+pout, family = binomial(link = "logit"), 
    data = ba)
summary(lo)
lo1<-glm(formula = subs~edu_1+edu_2+def+house+loa+duration+campaign+pout, family = binomial(link = "logit"), 
        data = ba)
summary(lo1)

h4<-ba[,c(1,6,9:12,17:19,21:25)]
R<-cor(h4)
n<-nrow(h4)
p<-ncol(h4)
evec.4<-eigen(R)$vectors
eval.4<-eigen(R)$values
plot(1:14,eval.4, xlab="Eigenvalue Number", ylab = "Eigenvalue",
     main= "Scree Plot for eigenvalues of variables", pch=19); lines(1:14, eval.4)
percentage <- rep(0,14)
for (i in 1:14){
  percentage[i] <- sum(eval.4[1:i])/sum(eval.4)   
}
percentage
length(eval.4[eval.4>mean(eval.4)])

library(rpart)
library(rpart.plot)
mct <- rpart(subs~age+balance+duration+campaign+pdays+previous+loa+house+pout+mari_1+mari_2+edu_1+edu_2+def,data=ba, method="class")
rpart.plot(mct, main= "votes", type=0,extra=101)
pct <- predict(mct, h4, type="class")
(tab.ct <- table(Sample = ba$y, Predicted = pct))
1-sum(diag(tab.ct))/sum(tab.ct)
##apparent error rate is 0.1762995

mct1 <- rpart(subs~edu_1+edu_2+def+house+loa+duration+campaign+pout,data=ba, method="class")
rpart.plot(mct1, main= "votes", type=0,extra=101)
colnames(h4)
h5<-h4[,c(12,13,14,8,7,3,4,9)]
pct1 <- predict(mct1, h5, type="class")
(tab.ct1 <- table(Sample = ba$y, Predicted = pct1))
1-sum(diag(tab.ct1))/sum(tab.ct1)