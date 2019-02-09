stat360

a<-matrix(c(3,6,12,2,4,8,1,2,4),nrow=3,ncol=3)
det(a)

b<-matrix(c(1,-1,-1,-1,1,1,2,-2,-2),nrow=3,ncol=3)
det(b)          

c<-matrix(c(2,1,2,-1),nrow=2)
det(c)

a%*%b

library(Matrix)
rankMatrix(b)

#2

a<-matrix(c(2/3,0,sqrt(2)/3,0,1,0,sqrt(2)/3,0,1/3),nrow=3,ncol=3)
rankMatrix(a)

a%*%a
i<-matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
a%*%(i-a)
det(a)
det(i-a)
a%*%a
trace(a)

X<-matrix(rep(0,162),nrow=18,ncol=16)
X[,1]=1
X
write.csv(X,"matrix.csv")
X<-read.csv("MatrixX.csv",header=F)
c<-as.matrix(X)
t(c)%*%c
det(c%*%solve(t(c)%*%c)%*%t(c)-c%*%t(c))
det(c%*%t(c))
det(t(c%*%t(c)))
solve(t(c)%*%c)*18


c1<-as.matrix(X<-read.csv("MatrixXX.csv",header=T))
m<-c1
m
m%*%t(m)
t(m)%*%m
m%*%solve(t(m)%*%m)%*%t(m)
print(m%*%t(m),digits=0)
print(c%*%solve(t(c)%*%c)%*%t(c),digits=1)
round(c%*%solve(t(c)%*%c)%*%t(c),digits=2)
t(m[,2])%*%(m[,3])
t(m)%*%m

for (i in 1:8)
  {for (j in i+1:9)
  {print(t(c[,i])%*%(c[,j]))}
}

round(solve(t(c)%*%c),digits=4)
c

or<-as.matrix(read.csv("matrix.csv"))
or
(or)%*%t(or)
t(c)%*%c
