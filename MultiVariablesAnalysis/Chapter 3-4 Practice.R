#Generating the data desired for the first question
library(MASS)
n <- 100
p <- 3
mu <- c(2,4,5)
s1 <- 4; s2 <- 3; s3 <- 4; rho12 <- .8; rho13 <- 0; rho23 <- 0
sigma <- matrix(c(s1^2, s1*s2*rho12, s1*s3*rho13,
                  s1*s2*rho12, s2^2, s2*s3*rho23,
                  s1*s3*rho13, s2*s3*rho23, s3^2),
                nrow=3)

X <- mvrnorm(n,mu=mu,Sigma=sigma)

#1.
colMeans(X)
var(X)
#2
apply(X,2,shapiro.test)

d2 <- NULL
for (i in 1:nrow(X)){
  d2 <- c(d2,t(X[i,]-colMeans(X))%*%solve(var(X))%*%(X[i,]-colMeans(X)))
}
d2
j=1:n
chisqpoints <- qchisq((j-1/2)/n,df=p)
qqplot(chisqpoints,d2, main="Chisq Q-Q Plot")
points(chisqpoints,chisqpoints,type="l")

for (i in 1:3){
  qqnorm(X[,i])
  qqline(X[,i])
}

#4
Y<-cbind(X[,1],X[,2],X[,3]^2)
colMeans(Y)
var(Y)

apply(Y,2,shapiro.test)

for (i in 1:3){
  qqnorm(Y[,i])
  qqline(Y[,i])
}

d2 <- NULL
for (i in 1:nrow(Y)){
  d2 <- c(d2,t(Y[i,]-colMeans(Y))%*%solve(var(Y))%*%(Y[i,]-colMeans(Y)))
}
d2
j=1:n
chisqpoints <- qchisq((j-1/2)/n,df=p)
qqplot(chisqpoints,d2, main="Chisq Q-Q Plot")
points(chisqpoints,chisqpoints,type="l")


library(MASS)
m1<-lm(Y[,3]~1)
bc<-boxcox(m1,lambda=(seq(-4,4,by=0.1)))
max.loc<-which(bc$y==max(bc$y))
bc$x[max.loc]
Y[,3]<-Y[,3]^(bc$x[max.loc])
