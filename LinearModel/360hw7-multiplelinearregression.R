x<-c(1, 1, 0, 1,
1, 1, 0, 1,
1, 1, 0, 2,
1, 1, 0, 2,
1, 0, 1, 1,
1, 0, 1, 1)
X<-matrix(x,byrow=T,ncol=4)
qr(X)

library(MASS)
ginv((X))
solve(t(X)%*%X)

Null(X)

xx<-t(X)%*%X
xg
solve(xx[2:4,2:4])
xg<-matrix(rep(0,16),ncol=4)
xg[2:4,2:4]=solve(xx[2:4,2:4])
xx%*%xg%*%xx
X%*%ginv(t(X)%*%X)%*%t(X)
round(X%*%ginv(t(X)%*%X)%*%t(X),2)

XC<-matrix(c(-1,-1,-1,-1,1,1,
             0,0,0,0,2,2,
             0,0,2,2,0,0),ncol=3)
round(XC%*%solve(t(XC)%*%XC)%*%t(XC),2)

(t(X)%*%X)
XG<-ginv(t(X))
XG%*%t(X)%*%y5
xg%*%y4
round(xg%*%t(X)%*%X,2)

xa<-matrix(c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1),ncol=4)
xg<-matrix(c(1,-1,-1,0,-1,4/3,1,0,-1,1,3/2,0,0,0,0,0),ncol=4)
X%*%y3
t(X)%*%y5
xa
y1<-matrix(c(5,2,3,7),ncol=1)
y2<-matrix(c(1,1,2,2,4,4),ncol=1)
y3<-matrix(c(1,-1,-1,0),ncol=1)
y4<-matrix(c(8,6,2,12),ncol=1)
y5<-matrix(c(1,-1,1,-1,-1,1),ncol=1)

xx%*%xg%*%y4
xg%*%t(X)%*%y5



XG<-ginv(X%*%t(X))
XXXG<-round(X%*%t(X)%*%XG,2)
XXXG%*%y5
