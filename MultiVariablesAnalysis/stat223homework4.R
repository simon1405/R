
#STAT223 Homework 4
#Zihao Huang
#4/27/2018


#1.
library(readxl)
track <- read_excel("C:/Users/simon/Desktop/STAT223/track.xlsx")
track
tra<-track[,2:9]
S<-var(tra)
R<-cor(tra)
n<-nrow(tra)
p<-ncol(tra)
##a.
S
(eval<-eigen(S)$values)
evec<-eigen(S)$vectors
##b.
percentage<-rep(0,8)
for (i in 1:8){
  percentage[i] <- sum(eval[1:i])/sum(diag(eval))   
}
percentage
length(eval[eval>mean(eval)])
###It recommends 1 PCs.
sum(eval[1])/sum(eval)
###The proportion of total variance is 98.01%.
##c.
z <- as.matrix(scale(tra,center=T, scale=F))%*%evec[,1:2]
rbind(colnames(track)[-1],t(evec[,1:2]))
###The first principal component explains the long run ability (Marathon). The second
###principal component explains the 400m running ability.
##d.
z1.1<-cbind(as.numeric(z[,1]),track[,1])
best5.1<-z1.1[order(z1.1[,1],decreasing = T),c(1,2)][1:5,]
worst5.1<-z1.1[order(z1.1[,1],decreasing = F),c(1,2)][1:5,]
###Best 5 are Usa,Australia,Japan,Portugal,Netherla.(The nations need the shortest time to finish running)
###Worst 5 are Cookis,Wsamoa,Singapor,Domrep,Malaysia.(The nations need the longest time to finish running)


#2.
##a.
R
(eval.2<-eigen(R)$values)
evec.2<-eigen(R)$vectors
##b.
percentage<-rep(0,8)
for (i in 1:8){
  percentage[i] <- sum(eval.2[1:i])/sum(diag(eval.2))   
}
percentage
length(eval.2[eval.2>mean(eval.2)])
###It recommends 1 PCs.
sum(eval.2[1])/sum(eval.2)
###The proportion of total variance is 82.78%.
##c.
#z.2<-as.matrix(scale(tra,center=T, scale=F))%*%evec.2[,1:2]
tra.st<-matrix(0,ncol=8,nrow=55)
tra<-as.matrix(tra)
for (i in 1:55){
  for (j in 1:8){
    tra.st[i,j]<-(tra[i,j]-colMeans(tra)[j])/sqrt(diag(S)[j])
  }
}
z.2<-tra.st%*%evec.2[,1]
evec.2[,1:2]
###The first principal component doesn't interpret well since all of the values are smaller than -0.40,
###The second principal interpret the short run ability (100m running)
##d.
z1.2<-cbind(as.numeric(z.2[,1]),track[,1])
best5.2<-z1.2[order(z1.2[,1],decreasing = T),c(1,2)][1:5,]
worst5.2<-z1.2[order(z1.2[,1],decreasing = F),c(1,2)][1:5,]
###Best 5 are Usa,gbni,italy,Ussr,gdr. (The nations need the shortest time to finish running)
###Worst 5 are Cookis,Wsamoa,Mauritiu,png,Singapor.(The nations need the longest time to finish running)
##e.
(cbind(best5.1,best5.2))
###From the result on the best 5 nations between 1 and 2, Usa is the top 2 nations.
(cbind(worst5.1,worst5.2))
###From the result on the worst 5 nations between 1 and 2, Cookis, Wsamoa, Singapor  are on the list.

#3.
##a
tra3<-cbind(tra[,1:3],60*tra[,4:8])
tramps<-cbind(100/tra3[,1],200/tra3[,2],400/tra3[,3],800/tra3[,4],1500/tra3[,5],
              5000/tra3[,6],10000/tra3[,7],42195/tra3[,8])
tramps<-as.matrix(tramps)
##b
(S3<-var(tramps))
(eval.3<-eigen(S3)$values)
evec.3<-eigen(S3)$vectors
##c.
length(eval.3[eval.3>mean(eval.3)])
###It recommends 1 PCs.
sum(eval.3[1])/sum(eval.3)
###The proportion of total variance is 81.95%.
##d.
evec.3[,1:2]
###The first component is not interpretable, the second one explains 100m running result.
tra3.st<-matrix(0,ncol=8,nrow=55)
for (i in 1:55){
  for (j in 1:8){
    tra3.st[i,j]<-(tramps[i,j]-colMeans(tramps)[j])/sqrt(diag(S3)[j])
  }
}
z.3 <- tra3.st%*%evec.3[,1:2]
#plot(z.3[,1]~z.3[,2],xlab="PC1", ylab="PC2")

##e.
z1.3<-cbind(as.numeric(z.3[,1]),track[,1])
best5.3<-z1.3[order(z1.3[,1],decreasing = T),c(1,2)][1:5,]
worst5.3<-z1.3[order(z1.3[,1],decreasing = F),c(1,2)][1:5,]
###Best 5 are Cookis,Wsamoa,mauritiu,png,Singapor.(The slowest 5)
###Worst 5 are Usa,gbni,italy,ussr,gdr.(The fastest 5)

##f.
(cbind(best5.1,best5.2,best5.3))
(cbind(worst5.1,worst5.2,worst5.3))
###Since problem 3 is using speed as a measuarement instead of time, the scores from problem 3 are
###opposite to previous problem 1 and 2.
###The top 5 in problem 2 are the same as the bottom 5 in problem 3, while
###The bottom 5 in problem 2 are the same as the top 5 in problem 3
###The least scores for 3 nations, Cookis, Wsamoa, Singapor are on the list for 3 problems.
###Especially, USA have highest scores problem 1,2 and lowest scores in problem 3.

#4.
##a.
#install.packages("MVN")
library(MVN)
mvn(tra)
###Based on all variables reject the null hypothesis of Shapiro-Wilk Test
###It doesn't meet with the assumption of mulitivariate normality.
##b.
###It suggests to use PC methods.
##c.
R4<-cor(tra)
E <- eigen(R4)$vectors 
Lambda <- diag(eigen(R4)$values)
diag(Lambda)
L <- E%*%sqrt(Lambda)
plot(1:8,diag(Lambda), xlab="Eigenvalue Number", ylab = "Eigenvalue",
     main= "Scree Plot", pch=19); lines(1:8, diag(Lambda))
percentage <- rep(0,p)
for (i in 1:8){
  percentage[i] <- sum(diag(Lambda)[1:i])/sum(diag(Lambda))   
}
percentage
###The reason choosing m=1: 1.The first eigenvalue explain 82.78%,
###2.Only 1 eigenvalue exceeds 1, 3.Scree plot shows m should be 1.


E1 <- E[,1]
Lambda1 <- Lambda[1,1]
L1 <- E1%*%t(sqrt(Lambda1))
C1 <- L1%*%t(L1)
Psi1 <- diag(diag(R4-C1)) 
FA.PC1.res <-round(R4-(C1+Psi1),2)

E2 <- E[,1:2]
Lambda2 <- Lambda[1:2,1:2]
L2 <- E2%*%sqrt(Lambda2)
C2 <- L2%*%t(L2)
Psi2 <- diag(diag(R4-C2)) 
FA.PC2.res <-round(R4-(C2+Psi2),2)

library(psych)
FA.PC1 <- principal(r=R4, nfactors=1, rotate="varimax")
diag(FA.PC1$residual)
FA.PC2 <- principal(r=R4, nfactors=2, rotate="varimax")
diag(FA.PC2$residual)
###By comparing m=1 and m=2, the residual matrix has one residua greater than 0.25,
###We suggest to choose m=2.
##d.
FA.PC1$loadings
###Pc1 interpret all the running perfomances.
FA.PC2$loadings
###In Pc2 the first pc interpret long run results(800m,1500m,5000m,10000m and Marathon)
###The second pc interpret short run results(100m,200m,400m,800m).
##e.
###Since the data are not normall distributed, we suggest to use Bartlett's method.
L1<-FA.PC2$loadings
D <- sqrt(solve(diag(diag(cov(tra)))))
ybar <- colMeans(tra)
(f <- solve(t(L1)%*%solve(Psi1)%*%L1)%*%t(L1)%*%solve(Psi1)%*%D%*%(tra[53,]-ybar))
###For Usa, the score is PC1: -0.1743, PC2: -1.7993
###This shows that UsA has factors scores below 0, shows the perfomance of USA is below
###the average, that is, Usa sportsmen needed shorter time to finish the running race. 

