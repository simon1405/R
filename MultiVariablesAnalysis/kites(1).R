kites <- read_excel("C:/Users/jmarti52/Dropbox/Vermont STAT/Multivariate/Data/Dan's Data/kites.xlsx")
View(kites)
kites <- as.matrix(kites)
p <- ncol(kites)
n <- nrow(kites)

plot(kites)

#Finding the summary statistics by hand
(meanvec <- 1/n*t(kites)%*%rep(1,n))
(S <- 1/(n-1)*t(kites)%*%(diag(rep(1,n))-1/n*matrix(1,nrow=n,ncol=n))%*%kites)
(D <- diag(diag(S)))
R <- solve(sqrt(D))%*%S%*%solve(sqrt(D))

#Finding the mean, S, and R
summary(kites)
(meanvec <- apply(kites,2,mean))
(varmat <- var(kites))
cor(kites)

#determinants and inverse matrices
det(varmat)
5solve(varmat)
round(varmat%*%solve(varmat))


#finding the eigenvalues and vectors
eigen(varmat)
lambda <- diag(eigen(varmat)$values)
E <- eigen(varmat)$vectors
E%*%lambda%*%t(E)

(var.sqrt <- E%*%sqrt(lambda)%*%t(E))
var.sqrt%*%var.sqrt

#changing variables to size and shape
size.vec <- c(1,1) #adding tail length to wing length to find size
shape.vec <- c(-1,1)# subtracting tail length from wing length to find shape

size <- as.matrix(kites)%*%size.vec
shape <- as.matrix(kites)%*%shape.vec

c(mean(size),mean(shape))
(varmat2 <- var(cbind(size,shape)))

#Finding the variance matrix of shape and size without having to calculate the variables
C<- rbind(size.vec,shape.vec)
C%*%meanvec
C%*%varmat%*%t(C)

a<-matrix(c(3,-5,1),nrow=1)
sp<-matrix(c(4,1,0,1,2,1,0,1,3),nrow=3)
a%*%sp%*%t(a)
b<-matrix(c(1,2,-1,1,1,-1),nrow=2)
mu<-matrix(c(2,-1,3),ncol=1)
a%*%mu
b%*%mu
b%*%sp%*%t(b)
sp

s13<-matrix(c(4,0,0,3),nrow=2)
s2.13<-matrix(c(1,1),nrow=1)
mu13<-matrix(c(2,3),nrow=2)
-1+s2.13%*%solve(s13)%*%(mu13-mean(mu13))
2-s2.13%*%solve(s13)%*%t(s2.13)
#-23/24,  17/12

s12<-matrix(c(4,1,1,2),nrow=2)
s12.3<-matrix(c(0,1),nrow=1)
mu12<-matrix(c(2,-1),nrow=2)
mu12+t(s12.3)%*%matrix(c(1/3))%*%matrix(c(3-3))
mu12
#2,-1
s12-t(s12.3)%*%matrix(c(1/3))%*%s12.3
#4,1,1,5/3 2x2


#360
y<-c(1,4,3,5,2,4,8,10,12)
x1<-c(6,4,6,2,8,1,3,5,4)
x2<-c(3,5,4,7,6,9,1,0,3)
x<-cbind(rep(1,9),x1,x2)

xt<-t(x)
(beta.hat<-solve(xt%*%x)%*%xt%*%y)

vb<-det(t(y-x%*%beta.hat)%*%(y-x%*%beta.hat))*solve(xt%*%x)/6
colnames(vb)<-c("beta0","beta1","beta2")
rownames(vb)<-c("beta0","beta1","beta2")
vb

findb<-function(y,x){
  xt<-t(x)
  beta.hat<-solve(xt%*%x)%*%xt%*%y
  df<-dim(x)[1]-dim(x)[2]
  vb<-det(t(y-x%*%beta.hat)%*%(y-x%*%beta.hat))*solve(xt%*%x)/df
  colnames(vb)<-c("beta0","beta1","beta2")
  rownames(vb)<-c("beta0","beta1","beta2")
  print("the estimator of beta is:")
  print(beta.hat)
  print("the variance of beta is:")
  print(vb)
}
findb(y,x)

#summary(lm(y~x1+x2))
summ<-summary(lm(y~x1+x2))
summ$coefficients[,2]^2
#$sigma

be<-as.matrix(beta.hat)
sebe<-(be-mean(be))%*%t((be-mean(be)))

var(x)%*%solve(t(x)%*%x)
t(x-mean(x))%*%t(x-mean(x))


s2<-2.934^2
sqrt(s2*(solve(t(x)%*%x)))

x0<-c(rep(1,9))
t(x-colMeans(x))%*%(x-colMeans(x))

#231
y1<-c(36.6,39.2,30.4,37.1,34.1,17.5,20.6,18.7,25.7,22,15,10.4,18.9,10.5,15.2)

y2<-c(17.5,20.6,18.7,25.7,22)
y3<-c(15,10.4,18.9,10.5,15.2)
manova(y1,y2,y3)
cbind(y1,y2,y3)
mean(y1)
mean(y2)
mean(y3)
(var(y1)+var(y2)+var(y3))/12
35.48-(20.9+14)/2
2.871417/5*1.5
