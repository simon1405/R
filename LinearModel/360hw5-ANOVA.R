##360 hw5

x<-matrix(c(1,1,1,1,1,1,1,1,4,1,2,4,4,2,7,8,2,5,6,7,5,6,4,4,3,2,1,8,2,1,2,12),ncol=4)
y<-matrix(c(2,3,5,5,2,4,3,12),ncol=1)
#1
beta<-solve(t(x)%*%x)%*%t(x)%*%y
sigma<-t(y-x%*%beta)%*%(y-x%*%beta)
varb<-det(sigma)*solve(t(x)%*%x)
c1<-matrix(c(0,1,-1,0),nrow=1)
c2<-matrix(c(0,1,-0.5,-0.5),nrow=1)
c<-rbind(c1,c2)
t(y)%*%(y)
j<-1/8*matrix(rep(1,64),nrow=8)

sstotal<-t(y)%*%(y)-t(y)%*%j%*%(y)
sse<-t(y-x%*%beta)%*%(y-x%*%beta)
#2
ssh<-t(c%*%beta)%*%solve(c%*%solve(t(x)%*%x)%*%t(c))%*%(c%*%beta)  
F<-(ssh/2)/(sse/(24-4))
#3
rsquare<-1-sse/sstotal
#4
y.pre<-x%*%beta
resi<-y-y.pre
sum(resi)

t(y)%*%x%*%beta%*%solve(t(y)%*%y)
ssre<-t(y)%*%y-t(beta)%*%t(x)%*%y
ssre
sse

lm1<-lm(y~x[,2]+x[,3]+x[,4])
summary(lm1)

summary(aov(y~x[,2]+x[,3]+x[,4]))
t(y)%*%y
1-ssre/236

x[,2:4]
