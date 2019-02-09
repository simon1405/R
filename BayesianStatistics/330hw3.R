vx<-c(161,155,136,133,153,171,141,157,137,172)
install.packages("rstan")

n<-length(vx)
install.packages("StanHeaders") # from CRAN
devtools::install_github("stan-dev/rstan", subdir = "rstan/rstan", ref = "develop")

library(rstan)
library(StanHeaders)
stancode<-"
data{
int<lower=1>n;
int vx[n];
}
parameters{
real<lower=0>mu;
}
model{
vx~poisson(mu);
}
"
fit <- stan(model_code=stancode)
fit

MCMCpack
install.packages("MCMCpack")
library("MCMCpack")
outcome <- gl(3,1,9)
treatment <- gl(3,3)
posterior <- MCMCpoisson(vx)
plot(posterior)
summary(posterior)
mean(vix)
