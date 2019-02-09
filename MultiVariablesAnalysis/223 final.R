##Final Exam for STAT223
##Zihao Huang
##

#1.
library(readxl)
firms <- read_excel("C:/Users/simon/Desktop/STAT223/firms.xlsx")

firm.b<-subset(firms, Status=="B", select=c(x1,x2,x3,x4))
firm.s<-subset(firms, Status=="S", select=c(x1,x2,x3,x4))
n1<-nrow(firm.b)
n2<-nrow(firm.s)
s1<-var(firm.b)
s2<-var(firm.s)
meany1<- apply(firm.b,2,mean)
meany2<- apply(firm.s,2,mean)
meandiff<-(meany1-meany2)
p<-4
k<-2
sp<-((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
##A)
(T2<-(n1*n2/(n1+n2))*t(meandiff)%*%solve(sp)%*%(meandiff))
p*(n1+n2-2)/(n1+n2-p-1)*qf(.95,p, n1+n2-p-1)
##B)
(a<-solve(sp)%*%(meany1-meany2))
(a.star <- sqrt(diag(sp))*solve(sp)%*%meandiff)

###The ranking for variables is x2,x4,x3,x1
###
##C)
(zc<-0.5*t(a)%*%(meany1+meany2))
###The rule is -2.360438
z<-as.matrix(firms[,1:4])%*%a
class.1<-c(rep("B",n1+n2))
class.1[which(z>=-2.360438)]<-"S"

library(MASS)
m1 <- lda(Status~x1+x2+x3+x4, data=firms, prior=rep(1,k)/k)
predict(m1)
pred1 <- predict(m1)$class #Predicting each state
pre<-data.frame(firms$Status,pred1)
table(pre)
1-sum(diag(table(pre)))/sum(table(pre))

##D)


##E)
m.cv <- lda(Status~x1+x2+x3+x4, data=firms, prior=rep(1,k)/k,CV=T)
pred2<-m.cv$class
pre.cv<-data.frame(firms$Status,pred2)#Comparing our predictions and R predictions
table(pre.cv)
1-sum(diag(table(pre.cv)))/sum(table(pre.cv))


#2.
library(readxl)
cereal <- read_excel("C:/Users/simon/Desktop/STAT223/cereal.xlsx")
ce<-as.data.frame(cereal[,-1])
rownames(ce)<-cereal$Name
##A)
D <- (dist(ce,diag=T, upper=T))
cl.sin<-hclust(D,method="single")
plot(as.dendrogram(cl.sin),main="Dendrogram for Single Linkage")

cl.com<-hclust(D,method="complete")
plot(as.dendrogram(cl.com),main="Dendrogram for Complete Linkage")

cl.ave<-hclust(D,method="average")
plot(as.dendrogram(cl.ave),main="Dendrogram for Average Linkage")
##B,C
cl.com<-hclust(D,method="complete")
plot(as.dendrogram(cl.com),main="Dendrogram for Complete Linkage")
rect.hclust(cl.com,k=3,border="red")
###We will choose the clusters before large gaps are shown. Hence, it is suggested to use k=3.
##D
###The method that clustering the cereals are determined by the furthest distances
###from each cluster. Because Complete linkage tends to increase the distances 
###between clusters, to find compact clusters.
group1<-apply(ce[c(7,9),],2,mean)
group2<-ce[10,]
group3<-apply(ce[c(-7,-9,-10)],2,mean)
rbind(group1,group2,group3)
###Comparing to the group 1 (total and product) and puffed, they have different vitamin, crabs 
###protein, calories. group 1 and group 3 have different vitamin.
###pUffed and group 3 have different vitamin, carbs, calories and protein.
###

##E
###The next step is gathering puffed to group 3
n1<-1
n2<-9
c1<-ce[10,]
c2<-ce[c(-7,-9,-10),]
s22<-var(c2)
s21<-matrix(rep(0,25),nrow=5)
meany21<- apply(c1,2,mean)
meany22<- apply(c2,2,mean)
meandiff2<-(meany21-meany22)
p2<-5
k2<-2
sp2<-((n1-1)*s21+(n2-1)*s22)/(n1+n2-2)
(T2<-(n1*n2/(n1+n2))*t(meandiff2)%*%solve(sp2)%*%(meandiff2))
p2*(n1+n2-2)/(n1+n2-p2-1)*qf(.95,p2, n1+n2-p2-1)
###Given T2 is 208.65>62.561,we reject the null hypothesis that the group 3
###is significant different from puffed. We believe puffed should not be
##combined.

#3.
house1<- read.table("C:/Users/simon/Desktop/STAT223/housdat.txt",header=T)
house<-house1[,-14]
(R<-cor(house))
(S<-cov(house))
##A)
###choose R than S.Because covariance have larger variances rather than others. These would
###lead to the variable with high variance explains most of the data.

##B)
E <- eigen(R)$vectors
eval<-eigen(R)$values
Lambda <- diag(eigen(R)$values)
diag(Lambda)
plot(1:13,diag(Lambda), xlab="Eigenvalue Number", ylab = "Eigenvalue",
     main= "Scree Plot", pch=19); lines(1:13, diag(Lambda))
percentage <- rep(0,14)
for (i in 1:13){
  percentage[i] <- sum(diag(Lambda)[1:i])/sum(diag(Lambda))   
}
percentage
length(eval[eval>mean(eval)])
###There are 3 eigenvalues larger than mean of eigenvalues.
###The scree plot shows that a huge decrease before m=4, and the percentage of 
###egienvalues explained the total variance shows that m=5 explains 80.78%
###Hence, we choose m=5

##C)
###m=5, 5 eigenvalues explains 80.78%

##D)
h.sc <- scale(house, center=T, scale=apply(house,2,sd))
z<-h.sc%*%E[,1:2]
plot(z[,1]~z[,2],main="plot for first two PCs")
###The 1st Pcs seperated the observations better than the 2nd PCs.
###The 1st Pcs seperated 3 groups(at least). 

##E)
plot(house1$MED,z[,1],main="scatter plot for 1st PCs",xlab="1st PCs", ylab="Median house price")
###The scatter plot for 1st PCs shows the observations are not seperated well,
###it is hard to believed the 1st PCs is interpretable.
plot(house1$MED,z[,2],main="scatter plot for 2nd PCs",xlab="2nd PCs", ylab="Median house price")
###The scatter plot for 2nd PCs showsit doesn't seperated the observations
###well, the data gathered together, it should not be used for interpretation.

#4.
h4<-house[,-4]
R<-cor(h4)
n<-nrow(h4)
p<-ncol(h4)
evec.4<-eigen(R)$vectors
eval.4<-eigen(R)$values
plot(1:12,eval.4, xlab="Eigenvalue Number", ylab = "Eigenvalue",
     main= "Scree Plot", pch=19); lines(1:12, eval.4)
percentage <- rep(0,13)
for (i in 1:13){
  percentage[i] <- sum(eval.4[1:i])/sum(eval.4)   
}
percentage
length(eval.4[eval.4>mean(eval.4)])
##B)
m.FA5 <- factanal(x=h4,factors=5,rotation = "varimax")
m.FA4 <- factanal(x=h4,factors=4,rotation = "varimax")
m.FA3 <- factanal(x=h4,factors=3,rotation = "varimax")
L5 <- m.FA5$loadings
Psi5 <- diag(diag(R-(L5)%*%t(L5)))
L4 <- m.FA4$loadings
Psi4 <- diag(diag(R-L4%*%t(L4)))
L3 <- m.FA3$loadings
Psi3 <- diag(diag(R-L3%*%t(L3)))
u5 <- log(det(L5%*%t(L5)+Psi5)/det(R))
u.prime <- (n-1-1/6*(2*p+5)-2/3*5)*u5
df <- .5*((p-5)^2-p-5)
(p.val <- pchisq(u.prime,df,lower.tail = F))#P-value
u4 <- log(det(L4%*%t(L4)+Psi4)/det(R))
u.prime <- (n-1-1/6*(2*p+4)-2/3*4)*u4
df <- .5*((p-4)^2-4-5)
(p.val <- pchisq(u.prime,df,lower.tail = F))#P-value
u3 <- log(det(L3%*%t(L3)+Psi3)/det(R))
u.prime <- (n-1-1/6*(2*p+3)-2/3*3)*u3
df <- .5*((p-3)^2-p-3)
(p.val <- pchisq(u.prime,df,lower.tail = F))#P-value
ll5<- 2*n*(log(det(L5%*%t(L5)+Psi5))+sum(diag(solve(L5%*%t(L5)+Psi5)%*%R)))
k5<- p*(5+1)-5*(5-1)
AIC5<-ll5+2*k5
ll4<- 2*n*(log(det(L4%*%t(L4)+Psi4))+sum(diag(solve(L4%*%t(L4)+Psi4)%*%R)))
k4<- p*(4+1)-4*(4-1)
AIC4<-ll4+2*k4
ll3<- 2*n*(log(det(L3%*%t(L3)+Psi3))+sum(diag(solve(L3%*%t(L3)+Psi3)%*%R)))
k3<- p*(3+1)-3*(3-1)
AIC3<- -2*ll3+2*k3
cbind(AIC5,AIC4,AIC3)
###Since AIC5=3316.089 is smallest, we choose m=5
sum(eigen(R)$values[1:5])/sum(eigen(R)$values)
###It explains 84.62% of the total variance.
##C)
L5

###And the 4th factor and 5th factor are trivial factors. Because they only explain one variable.
