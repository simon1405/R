sigma0<-1/4
mu0<-0
n1<-5
y1<-5

#part b
theta<-rnorm(1000,0.145,sqrt(1/17.243))
mean.theta<-mean(theta)
1/(1+exp(-mean.theta))
pnorm(0, mean = 0.145,sqrt(1/17.243), lower.tail = TRUE)

#part c
#suppose c=1
theta.c<-array(1:100000,0)
tot<-0
i<-0
while (i<100000)
{
u<-runif(1,0,1)
theta.hat<-rnorm(1,0,1/4)
post.hap<-exp(5*theta.hat)/(1+exp(theta.hat))^5*exp(-8*(theta.hat)^2)*sqrt(8/3.14159)
proposal.c<-exp(-8*(theta.hat)^2)*sqrt(8/3.14159)
if (u<=post.hap/proposal.c){
theta.c[i]<-theta.hat
i<-i+1
}
tot<-tot+1
}
tot
#1083598
i/tot
#1000000/2769163= 0.03611199

hist(theta.c,main="Part C: rejection sampling by using N(0,1/16) as proposal density")
d<-density(theta.c)
plot(d,main="Part C: rejection sampling by using N(0,1/16) as proposal density")

#part E
post.e<-exp(5*theta.hat)/(1+exp(theta.hat))^5*sqrt(8/3.14159)*exp(-8*(theta.hat)^2)
proposal.e<-sqrt(7.5/3.14159)*exp(-7.5*(theta.hat-0.145)^2)
theta.hat<-rnorm(10000,0,1/4)
c<-max(post.e*proposal.e)
c
#c=0.09235441

theta.e<-array(1:100000,0)
n<-0
tot1<-0
while (n<100000)
{
  u<-runif(1,0,1)
  theta.hat<-rnorm(1,0.145,sqrt(1/15))
  post.e<-exp(5*theta.hat)/(1+exp(theta.hat))^5*sqrt(8/3.14159)*exp(-8*(theta.hat)^2)
  proposal.e<-sqrt(7.5/3.14159)*exp(-7.5*(theta.hat-0.145)^2)
  if (u<=post.e/(c*proposal.e)){
    theta.e[n]<-theta.hat
    n<-n+1
  }
  tot1<-tot1+1
}
tot1
n/tot1
#100000/255637=0.3911797

hist(theta.e,main="Part E: rejection sampling by using N(0.145,1/15) as proposal density")
e<-density(theta.e)
plot(e,main="Part E: rejection sampling by using N(0.145,1/15) as proposal density")

#Part F
theta.f<-runif(100000,0,1)
w.f<-exp(5*theta.f)/(1+exp(theta.f))^5*sqrt(8/3.14159)*exp(-8*(theta.f)^2)
sum.w<-sum(w.f)*1/100000
h.f<-1/(1+exp(-theta.f))
plot(h.f,w.f,main="posterior distribution by using Importance Sampling",xlab="posterior",ylab="freq")
sum.hw<-sum(h.f*w.f)/100000
ehy<-sum.hw/sum.w
ehy
#ehy=0.5630088

#Part G

theta.hat<-rnorm(20*10000,0.145,sqrt(1/17.243))
post.g<-exp(5*theta.hat)/(1+exp(theta.hat))^5*sqrt(8/3.14159)*exp(-8*(theta.hat)^2)
proposal.g<-sqrt(7.5/3.14159)*exp(-7.5*(theta.hat-0.145)^2)
w.g<-post.g/proposal.g
p.g<-w.g/sum(w.g)
theta.star<-sample(theta.hat,10000,prob=p.g)
hist(theta.star,main="Part G, distribution by using SIR")
g<-density(theta.star)
plot(g,main="Part G, distribution by using SIR")
1/(1+exp(-mean(theta.star)))
#theta.star=0.5363201
