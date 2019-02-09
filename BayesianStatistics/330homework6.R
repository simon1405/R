#1
a<-0.5
b<-0.5
ap<-0.45
bp<-0.45
n<-100
y<-35
i<-0
k<-10000
theta.t<-1
save.theta<-c()
total<-0
while (i<k){
  theta.star<-rbeta(1,ap,bp)
 # post.star<-pbinom(35,size=100,prob=theta.star)
# post.t<-pbinom(35,size=100,prob=theta)
  post.star<-choose(n,y)*(theta.star^(y+a-1))*((1-theta.star)^(n-y+b-1))
  post.t<-choose(n,y)*(theta.t^(y+a-1))*((1-theta.t)^(n-y+b-1))
  q.t<-(theta.t^(ap-1))*((1-theta.t)^(bp-1))
  q.star<-(theta.star^(ap-1))*((1-theta.star)^(bp-1))
  compar<-(post.star*q.t)/(post.t*q.star)
  r<-min(1,compar)
  u<-runif(1,0,1)
  total<-total+1
  if(u<r){
    theta.t<-theta.star
  save.theta<-c(save.theta,theta.t)
  i<-i+1
}
}
k/total
#acceptance rate is 0.09465932

burnin<-save.theta[1001:k]
burnin
plot(burnin,type="l",main="traceplot for theta")
plot(density(burnin),main="Result by MH algorithm",xlab="probability",ylab="Frequencies")
hist(burnin)
x1<-runif(10000,0,1)
y1<-dbeta(x1,a+y,b+n-y)
plot(x1,y1,xlim=c(0.2,0.6),main="The exact posterior Beta(35.5,65.5)",xlab="Probability",ylab="Frequencies")


#2.
generate<c()
m<-1000
initial<-c(m,0,0,0,0)
tran<-c(0,0,0,0,0)
for (p in 1:m)
  {
  tran<-c(0,0,0,0,0)
for (i in 1:5){
  if (initial[i]>0){
    generate<-sample(statenames,initial[i],prob=transition[i,],replace=T)
    for (j in 1:5){
      tran[j]<-tran[j]+sum(generate==j)
    }
  }
  }
  initial<-tran
}
initial/m

m<-5000
initial<-c(m,0,0,0,0)
tran<-c(0,0,0,0,0)
for (p in 1:m)
{
  tran<-c(0,0,0,0,0)
  for (i in 1:5){
    if (initial[i]>0){
      generate<-sample(statenames,initial[i],prob=transition[i,],replace=T)
      for (j in 1:5){
        tran[j]<-tran[j]+sum(generate==j)
      }
    }
  }
  initial<-tran
}
initial/m

install.packages("markovchain")
library("markovchain")

initialstate<-c(1,0,0,0,0)
statenames<-c("1","2","3","4","5")
transition<-matrix(c(0.2,0.8,0,0,0,
                     0.2,0.2,0.6,0,0,
                     0,0.4,0.2,0.4,0,
                     0,0,0.6,0.2,0.2,
                     0,0,0,0.8,0.2), nrow = 5, byrow = TRUE,dimnames=list(statenames,statenames))

steps<- new("markovchain", states = statenames, byrow = TRUE,transitionMatrix = transition, name = "randomwalk")
steps
after1000steps<-initialstate*steps^1000
after1000steps
after5000steps<-initialstate*(steps ^ 5000)
after5000steps