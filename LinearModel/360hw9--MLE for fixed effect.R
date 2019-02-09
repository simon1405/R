
pup<-read.table("C:/Users/simon/Desktop/STAT360/hw9.txt",header=T)
pup<-as.matrix(pup)
y<-pup[,3]
rep(1,12)
X<-matrix(c(rep(1,3),rep(0,12),rep(1,3),rep(0,12),rep(1,3),rep(0,12),rep(1,3)),byrow=F,ncol=4)
Z<-rbind(diag(rep(1,3)),diag(rep(1,3)),diag(rep(1,3)),diag(rep(1,3)))
z<-matrix(c(rep(1,3),rep(0,12),rep(1,3),rep(0,12),rep(1,3),rep(0,12),rep(1,3)),byrow=F,ncol=4)
z

sigma<-(y-mean(y))*(y-mean(y))/12
ydiff<-as.matrix(y-mean(y))
V<-ydiff*t(ydiff)
det(V)
V<-diag(sigma)
solve(V)
x<-matrix(c(rep(1,12),pup[,2]),ncol=2)

betah<-solve(t(x)%*%solve(V)%*%x)%*%t(x)%*%solve(V)%*%y
%*%t(z)%*%solve(V)%*%(y-mean(y))
betah
sr<-y-X%*%solve(t(X)%*%solve(V)%*%X)%*%t(X)%*%solve(V)%*%y
Reml2<-0.5*log(det(V))-0.5*log(det(t(X)%*%solve(V)%*%X))-0.5*t(sr)%*%solve(V)%*%sr-(12-3)/2*log(2*3.1415)
betah<-solve((t(x)%*%solve(V)%*%x))%*%t(x)%*%solve(V)%*%y
betah<-solve(as.numeric(Reml2)*t(X)%*%X)%*%t(X)%*%y/as.numeric(Reml2)
install.packages("reml")
library(reml)

install.packages("lme4")
library(lme4)

glmm<-lmer(pup$Y~pup$X+(1|pup$Litter),REML=F)
summary(glmm)
for (i in 1:12){
  for (j in 1:12){
    V[i,j]<-(y[i]-mean(y))*(y[j]-mean(y))/12
  }
}
det(V)
methods(lmer)
xz<-cbind(x,Z)
hxz<-xz%*%ginv(t(xz)%*%xz)%*%t(xz)
hx<-x%*%solve(t(x)%*%x)%*%t(x)
X1<-cbind(rep(1,12),xz)
solve(X1)

library(MASS)
ginv(t(xz)%*%xz)%*%t(xz)%*%y
library(Matrix)
r0<-rankMatrix((xz))
(t(y)%*%(diag(rep(1,12))-xz%*%ginv(t(xz)%*%xz)%*%t(xz))%*%y)/(12-3)
t(y)%*%(diag(rep(1,12))-xz%*%ginv(t(xz)%*%xz)%*%t(xz))

        yyhat<-(diag(rep(1,12))-xz%*%ginv(t(xz)%*%xz)%*%t(xz))%*%y
t(yyhat)%*%(yyhat)/9
yyhx<-(diag(rep(1,12))-hx)%*%y
t(yyhx)%*%(yyhx)/12
betahat<-solve(t(x)%*%x)%*%t(x)%*%y
15.47156*solve(t(x)%*%x)
20.628*ginv(t(xz)%*%(xz))
var(y)
z


b0<-diag(rep(1,4))
b01<-cbind(b0,b0)
b01<-rbind(b01,b01)
b01

b01<-cbind(z,z)
b00<-ginv(t(b01)%*%b01)%*%t(b01)%*%y

(b00[1:4,]-3.925)^2

var(c(2.7,5.3,7.55,6.65))
TR<-x[10:12,]
YR<-as.matrix(y)[10:12,]
t(YR)%*%(diag(rep(1,3))-TR%*%solve(t(TR)%*%TR)%*%t(TR))%*%YR
b0114<-ginv(t(TR)%*%TR)%*%t(TR)%*%YR
bset<-cbind(b0111,b0112,b0113,b0114)
yhehe<-rbind(TR%*%b0111,TR%*%b0112,TR%*%b0113,TR%*%b0114)
b0s<-bset[1,]-3.9250
b1s<-bset[2,]-3.5875
cov(b0s,b1s)
z
Z
b01
u<-matrix(rep(0,64),ncol=8)
var(bset)
B01<-(rbind(as.matrix(b0s),as.matrix(b1s)))
zx<-matrix(c(seq(1,3),rep(0,12),seq(1,3),rep(0,12),seq(1,3),rep(0,12),seq(1,3)),byrow=F,ncol=4)
z.n<-cbind(z,zx)
z.n
u<-B01
var(y-z.n%*%u)
var(z.n%*%u)
var(y-yhehe)
t(y-x%*%betahat)%*%(y-x%*%betahat)/12
t(y-yhehe)%*%(y-yhehe)/12

2.16+6.615+7.26+4.86-14.97852
20.895-15.47156
cov(b1s,b1s)
diag(u*u/4)

Z<-cbind(z,zx)

findt<-function(b0,b1,s0,s1,se){
  l.save<-9999
  b0.save=b0
  b1.save=b1
  s0.save=s0
  s1.save=s1
  se.save=se
  for (i in seq(b0-0.5,b0+0.5,by=0.1)){
    for (j in seq(b1-0.5,b1+0.5,by=0.1)){
      for (k in seq(s0-0.5,s0+0.5,by=0.1)){
        for (l in seq(s1-0.5,s1+0.5,by=0.1)){
          for (m in seq(se-0.5,se+0.5,by=0.1)){
            vt<-diag(rep(m,12))+Z%*%diag(c(rep(k,4),rep(l,4)))%*%t(Z)
            lt<-log(det(vt))+t(y-x%*%rbind(i,j))%*%solve(vt)%*%((y-x%*%rbind(i,j)))
            if (lt<l.save){
              l.save<-lt
              b0.save=i
              b1.save=j
              s0.save=k
              s1.save=l
              se.save=m
            }
          }
        }
      }
    }
  }
  k<-cbind(b0.save,b1.save,s0.save,s1.save,se.save)
  return(k)
}

k<-cbind(3.9,3.6,12.4,5,3.1)
for (p in 1:10){
  k<-findt(k[1],k[2],k[3],k[4],k[5])
}
k

finddecimal<-function(b0,b1,s0,se,d){
  l.save<-9999
  b0.save=b0
  b1.save=b1
  s0.save=s0
  se.save=se
  for (i in seq(b0-5*d,b0+5*d,by=d)){
    for (j in seq(b1-5*d,b1+5*d,by=d)){
      for (k in seq(s0-5*d,s0+5*d,by=d)){
          for (m in seq(se-5*d,se+5*d,by=d)){
            vt<-diag(rep(m,12))+Z%*%diag(c(rep(k,4),rep(0,4)))%*%t(Z)
            lt<-log(det(vt))+t(y-x%*%rbind(i,j))%*%solve(vt)%*%((y-x%*%rbind(i,j)))
            if (lt<l.save){
              l.save<-lt
              b0.save=i
              b1.save=j
              s0.save=k
              se.save=m
            
          }
        }
      }
    }
  }
  k<-cbind(b0.save,b1.save,s0.save,se.save)
  return(k)
}

for (p in 1:10){
  k<-finddecimal(k[1],k[2],k[3],k[4],0.0001)
}
k

tria<-rbind(rep(0,5),rep(10,5))
apply(tria,2,mean)
findall<-function(g)
  