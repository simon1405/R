#STAT330 HOMEWORK
#Simon
#9/13/2017

#Problem 3.1
x <- seq(0.00,1,length=100)
y <- dbeta(x, 2, 7)
plot(x,y,type="l",main="beta(2,7)")
qbeta(0.5,2,7)
qbeta(c(0.05,0.95),2,7)
summary(y)
confint(y)
y
quantile(y,0.90)

qbeta( c(0.05,0.95), 2, 7)
qbeta( 0.5, 2, 7)

#Problem 3.4
y <- dbeta(x, 41, 81)
plot(x,y,type="l",main="beta(41,81)")
y <- dbeta(x, 11, 21)
plot(x,y,type="l",main="beta(11,21)")
y <- dbeta(x, 1, 1)
plot(x,y,type="l",main="beta(1,1)")
y <- dbeta(x, 6, 16)
plot(x,y,type="l",main="beta(6,16)")

install.packages("LearnBayes")
library("LearnBayes")
triplot(c(41,81),c(5,25))
triplot(c(11,21),c(5,25))
triplot(c(1,1),c(5,25))
triplot(c(6,16),c(5,25))

qbeta(c(0.025,0.975),11,36)
qbeta(c(0.025,0.975),46,106)
qbeta(c(0.025,0.975),16,46)
qbeta(c(0.025,0.975),6,26)

pbeta(0.25,11,36,lower.tail = F)
pbeta(0.25,46,106,lower.tail = F)
pbeta(0.25,16,46,lower.tail = F)
pbeta(0.25,6,26,lower.tail = F)

#Problem 2.A
#1.
sleep<-c(9.0,8.5,7.0,8.5,6.0,12.5,6.0,9.0,8.5,7.5,8.0,6.0,9.0,8.0,7.0,10.0,9.0,7.5,5.0,6.5)

mean(sleep)
ssy<-sd(sleep)*19
set.seed(2017)
sg<-1/rgamma(10000,mean(sleep),ssy/2)
rand<-rnorm(10000,mean(sleep),sqrt(sg/20))
#2.
install.packages("plot3D")
library("plot3D")
x_axis<-cut(rand,40)
y_axis<-cut(sg,40)
hi<-table(x_axis,y_axis)
hist3D(z=hi,xlab="mean of posterior prob",ylab="Var")
#3.
mean(rand)
quantile(rand,c(0.025,0.975))
#4.
t.test(rand)
t.test(sleep)
#5.
quantile(sg,c(0.025,0.975))
#6.
mean(rand+0.674*sg)
sd(rand+0.674*sg)

#Problem 3.A
#1.
rand5<-rnorm(5,10,1)
rand15<-rnorm(15,10,1)
rand30<-rnorm(30,10,1)
rand100<-rnorm(100,10,1)
#2.
mean(rand5)
ssy5<-sd(rand5)*4
sg5<-1/rgamma(10000,mean(rand5),ssy5/2)
rand5p<-rnorm(10000,mean(rand5),sqrt(sg5/5))
mean(rand5p)
sd(rand5)/sqrt(5)
sd(sg5)
t.test(rand5)
quantile(rand5p,c(0.025,0.975))

mean(rand15)
ssy15<-sd(rand15)*14
sg15<-1/rgamma(10000,mean(rand15),ssy15/2)
rand15p<-rnorm(10000,mean(rand15),sqrt(sg15/15))
mean(rand15p)
sd(rand15)/sqrt(15)
sd(sg15)
t.test(rand15)
quantile(rand15p,c(0.025,0.975))

mean(rand30)
ssy30<-sd(rand30)*29
sg30<-1/rgamma(10000,mean(rand30),ssy30/2)
rand30p<-rnorm(10000,mean(rand30),sqrt(sg30/30))
mean(rand30p)
sd(rand30)/sqrt(30)
sd(sg30)
t.test(rand30)
quantile(rand30p,c(0.025,0.975))

mean(rand100)
ssy100<-sd(rand100)*99
sg100<-1/rgamma(10000,mean(rand100),ssy100/2)
rand100p<-rnorm(10000,mean(rand100),sqrt(sg100/100))
mean(rand100p)
sd(rand100)/sqrt(100)
sd(sg100)
t.test(rand100)
quantile(rand100p,c(0.025,0.975))

x_axis<-cut(rand5p,40)
y_axis<-cut(sg5,40)
hi<-table(x_axis,y_axis)
hist3D(z=hi,xlab="mean of posterior prob",ylab="Var")
x_axis<-cut(rand15p,40)
y_axis<-cut(sg15,40)
hi<-table(x_axis,y_axis)
hist3D(z=hi,xlab="mean of posterior prob",ylab="Var")
x_axis<-cut(rand30p,40)
y_axis<-cut(sg30,40)
hi<-table(x_axis,y_axis)
hist3D(z=hi,xlab="mean of posterior prob",ylab="Var")
x_axis<-cut(rand100p,40)
y_axis<-cut(sg100,40)
hi<-table(x_axis,y_axis)
hist3D(z=hi,xlab="mean of posterior prob",ylab="Var")

#problem 4.A
fre<-0

for(i in 1:1000){
nor<-rnorm(5,10,1)
ssx<-sd(nor)/4
sgx<-1/rgamma(5,mean(nor),ssx/2)
rando<-rnorm(5,mean(nor),sqrt(sgx/4))
if(as.numeric(quantile(rando,0.025))<=mean(nor)){if(as.numeric(quantile(rando,0.975))>=mean(nor)){fre<-fre+1}}
ssx<-numeric(0)
sgx<-numeric(0)
}
fre
