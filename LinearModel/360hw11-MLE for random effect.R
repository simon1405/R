pup<-read.table("C:/Users/simon/Desktop/STAT360/hw9.txt",header=T)
pup<-as.matrix(pup)
z<-matrix(c(rep(1,3),rep(0,12),rep(1,3),rep(0,12),rep(1,3),rep(0,12),rep(1,3)),byrow=F,ncol=4)
Z<-matrix(c(rep(1,3),rep(0,12),rep(1,3),rep(0,12),rep(1,3),rep(0,12),rep(1,3)),byrow=F,ncol=4)
x<-matrix(c(seq(1,3),rep(0,12),seq(1,3),rep(0,12),seq(1,3),rep(0,12),seq(1,3)),byrow=F,ncol=4)
zx<-cbind(z,x)
y<-pup[,3]
X<-cbind(rep(1,12),pup[,2])
Z<-zx

findt<-function(s0,s1,se){
  l.save<-9999
  s0.save=s0
  s1.save=s1
  se.save=se
      for (k in seq(s0-0.5,s0+0.5,by=0.1)){
        for (l in seq(s1-0.5,s1+0.5,by=0.1)){
          for (m in seq(se-0.5,se+0.5,by=0.1)){
            vt<-diag(rep(abs(m),12))+Z%*%diag(c(rep(abs(k),4),rep(abs(l),4)))%*%t(Z)
            betat<-solve(t(X)%*%solve(vt)%*%X)%*%t(X)%*%solve(vt)%*%y
            lt<-log(det(vt))+t(y-X%*%betat)%*%solve(vt)%*%(y-X%*%betat)+log(det(t(X)%*%solve(vt)%*%X))
            if (lt<l.save){
              l.save<-lt
              s0.save=abs(k)
              s1.save=abs(l)
              se.save=abs(m)
            }
          }
        }
      
      }  
  
  k<-cbind(s0.save,s1.save,se.save)
  return(k)
}

k<-c(5,5,5)

for (i in 1:20){
  k<-findt(k[1],k[2],k[3])
}
k
finddeci<-function(s0,s1,se,d){
  l.save<-9999
  s0.save=s0
  s1.save=s1
  se.save=se
  for (k in seq(s0-5*d,s0+5*d,by=d)){
    for (l in seq(s1-0.5*d,s1+0.5*d,by=d)){
      for (m in seq(se-0.5*d,se+0.5*d,by=d)){
        vt<-diag(rep(abs(m),12))+Z%*%diag(c(rep(abs(k),4),rep(abs(l),4)))%*%t(Z)
        betat<-solve(t(X)%*%solve(vt)%*%X)%*%t(X)%*%solve(vt)%*%y
        lt<-log(det(vt))+t(y-X%*%betat)%*%solve(vt)%*%(y-X%*%betat)+log(det(t(X)%*%solve(vt)%*%X))
        if (lt<l.save){
          l.save<-lt
          s0.save=abs(k)
          s1.save=abs(l)
          se.save=abs(m)
        }
      }
    }
    
  }  
  
  k<-cbind(s0.save,s1.save,se.save,d)
  return(k)
}

k1<-c(k,0.01)

for (i in 1:100){
  k1<-finddeci(k1[1],k1[2],k1[3],0.001)
}
k1
R<-diag(rep(k1[3],12))
G<-diag(c(rep(k1[1],4),rep(0,4)))
V<-Z%*%G%*%t(Z)+R
Betah<-solve(t(X)%*%solve(V)%*%X)%*%t(X)%*%solve(V)%*%y
u<-G%*%t(Z)%*%solve(V)%*%(y-X%*%Betah)
  u
Betah

  ###fixed effect
beta.fix<-matrix(rep(0,8),nrow=2)
TR<-c()
YR<-c()
library(MASS)
for(i in 1:4){
  TR<-X[1:3,]
  YR<-as.matrix(y)[(3*i-2):(3*i),]
  beta.fix[,i]<-ginv(t(TR)%*%TR)%*%t(TR)%*%YR
   
}
apply(beta.fix,1,mean)

###compare:
y.f<-c()
for (i in 1:4){
  y.f<-c(y.f,TR%*%beta.fix[,i])
  }
as.matrix(y.f)

X%*%Betah+Z%*%u

t(beta.fix)
