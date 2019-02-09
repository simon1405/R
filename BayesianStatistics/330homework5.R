#1.b.
p.theta12<-matrix(c(1/3,2/3,2/3,1/3,3/4,1/4),nrow=2,ncol=3)
p.theta21<-matrix(c(1/6,1/3,1/2,1/2,1/4,1/4),nrow=3,ncol=2)
p.theta12
theta1.star<-1
theta2.star<-1
i<-1
save.theta1<-c(1)
save.theta2<-c(1)
n<-80000
k<-10000
set.seed(2017)
for (i in 1:n){
  u_theta1<-runif(1,0,1)
  if (u_theta1<=p.theta12[1,theta2.star]){theta1.star<-1}
  if (u_theta1>=p.theta12[1,theta2.star]){theta1.star<-2}
  
  
  u_theta2<-runif(1,0,1)
  if (u_theta2<=p.theta21[1,theta1.star]){theta2.star<-1}
  if (u_theta2>p.theta21[1,theta1.star]){
    if (u_theta2<=(p.theta21[1,theta1.star]+p.theta21[2,theta1.star])){theta2.star<-2}
  }
  if (u_theta2>p.theta21[1,theta1.star]+p.theta21[2,theta1.star]) {theta2.star<-3}
  save.theta1<-c(save.theta1,theta1.star)
  save.theta2<-c(save.theta2,theta2.star)
}
new.theta1<-data.frame(save.theta1[k+1:n])
new.theta2<-data.frame(save.theta2[k+1:n])


joint <- data.frame(cbind(save.theta1[(k+1):n], save.theta2[(k+1):n]))


j.11 <- length(joint[joint$X1==1&joint$X2==1,1])/(n-k)
j.21 <- length(joint[joint$X1==2&joint$X2==1,1])/(n-k)
j.12 <- length(joint[joint$X1==1&joint$X2==2,1])/(n-k)
j.22 <- length(joint[joint$X1==2&joint$X2==2,1])/(n-k)
j.13 <- length(joint[joint$X1==1&joint$X2==3,1])/(n-k)
j.23 <- length(joint[joint$X1==2&joint$X2==3,1])/(n-k)
pmf<-matrix(c(j.11,j.21,j.12,j.22,j.13,j.23),nrow=2)
pmf
# pmf is    [theta2=1][theta2=2][theta2=3]
#[theta1=1] 0.1002000 0.1995429 0.30054286
#[theta1=2] 0.2000857 0.1003429 0.09928571

#3.b.
n<-10000
k<-5000
#a=b=1
a<-1
b<-1
y<-c(8,9,9,10)
sumy<-sum(y)
ny<-length(y)
beta.star<-rgamma(1,a,rate=b)
lamda.star<-rgamma(1,2,rate=beta.star)
save.beta<-c(beta.star)
save.lamda<-c(lamda.star)
for (i in 1:n){
  beta.star<-rgamma(1,a,rate=b+lamda.star)
  lamda.star<-rgamma(1,2+sumy,rate=beta.star+ny)
  save.beta<-c(save.beta,beta.star)
  save.lamda<-c(save.lamda,lamda.star)
}

post.3.1<-data.frame(cbind(new.beta<-save.beta[k+1:n],new.lamda<-save.lamda[k+1:n]))
hist(post.3.1$X1,main="histogram of distribution for beta (a=b=1)",xlab="beta")
g=density(post.3.1$X1,na.rm=TRUE)
plot(g,main="Density plot of distribution for beta (a=b=1)",xlab="beta")
hist(post.3.1$X2,main="histogram of distribution for lamda (a=b=1)",xlab="lamda")
f=density(post.3.1$X2,na.rm=TRUE)
plot(f,main="Density plot of distribution for lamda (a=b=1)",xlab="lamda")
quantile(post.3.1$X1,c(0.025,0.975),na.rm = TRUE)
#95% credible intervals for beta
quantile(post.3.1$X2,c(0.025,0.975),na.rm = TRUE)
#95% credible intervals for lamda

#a=b=0.001
a<-0.001
b<-0.001
y<-c(8,9,9,10)
sumy<-sum(y)
ny<-length(y)
beta.star<-rgamma(1,a,rate=b)
lamda.star<-rgamma(1,2,rate=beta.star)
save.beta<-c(beta.star)
save.lamda<-c(lamda.star)
for (i in 1:n){
  beta.star<-rgamma(1,a,rate=b+lamda.star)
  lamda.star<-rgamma(1,2+sumy,rate=beta.star+ny)
  save.beta<-c(save.beta,beta.star)
  save.lamda<-c(save.lamda,lamda.star)
}

post.3.2<-data.frame(cbind(new.beta<-save.beta[k+1:n],new.lamda<-save.lamda[k+1:n]))
hist(post.3.2$X1,main="histogram of distribution for beta (a=b=0.001)",xlab="beta")
h=density(post.3.2$X1,na.rm = TRUE)
plot(h,main="Density plot of distribution for beta (a=b=0.001)",xlab="beta")
hist(post.3.2$X2,main="histogram of distribution for lamda (a=b=0.001)",xlab="lamda")
o=density(post.3.2$X2,na.rm = TRUE)
plot(o,main="Density plot of distribution for lamda (a=b=0.001)",xlab="lamda")
quantile(post.3.2$X1,c(0.025,0.975),na.rm = TRUE)
#95% credible intervals for beta
quantile(post.3.2$X2,c(0.025,0.975),na.rm = TRUE)
#95% credible intervals for lamda

