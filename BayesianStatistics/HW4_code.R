#HW 4_Code_A.Becker Appendix

#Laplace Approximation

set.seed(1)

post_dat <- rnorm(10000,0.145, sqrt(1/17.24))

(1+exp(-mean(post_dat)))^(-1)

#Rejection-Acceptance Algorithm (Part C)

set.seed(1)

c <- 1

dat <- numeric(0)
num <- 0
n <- 10000

while(length(dat) < n){

u <- runif(1,0,1)

theta <- rnorm(1,0,1/4)

p <- exp(5*theta)/(1+exp(theta))^5*exp(-8*(theta)^2)

cut <- p/(c*exp(-8*(theta-0)^2))

num <- num+1

if(u <= cut){
  dat <- c(dat, theta)
}
}


hist(dat, xlab = "Posterior Mean", main = 'Rejection-Acceptance Distribution', breaks=20)

(1+exp(-mean(dat)))^(-1)

length(dat[dat>=0])/length(dat)

length(dat)/num

#R-A Algorithm (Part E)

set.seed(1)

c <- 0.0923

dat <- numeric(0)
num <- 0
n <- 10000

while(length(dat) < n){
  
  u <- runif(1,0,1)
  
  theta <- rnorm(1,0.145,sqrt(1/15))
  
  p <- exp(5*theta)/(1+exp(theta))^5*exp(-8*(theta)^2)*1/sqrt(2*pi*(1/16))
  
  cut <- p/(c*1/sqrt(2*pi*(1/15))*exp(-15*(theta-0.145)^2/2))
  
  num <- num+1
  
  if(u <= cut){
    dat <- c(dat, theta)
  }
}


hist(dat, xlab = "Posterior Mean", main = 'Rejection-Acceptance Distribution')

(1+exp(-mean(dat)))^(-1)

length(dat)/num

# Importance Sampling
set.seed(1)

m <- 10000

theta_j <- runif(m, -1, 1)

w_j <- (exp(5*theta_j)/(1+exp(theta_j))^5*exp(-8*(theta_j)^2))/sqrt(2*pi)*exp(-(theta_j)^2/2)

sum_denom <- 1/m*sum(w_j)

h_j <- (1+exp(-theta_j))^(-1)

sum_numer <- 1/m*sum(w_j*h_j)

sum_numer/sum_denom

# Sampling-Importance-Resampling

set.seed(1)

N <- 10000

theta_m <- rnorm(20*N, 0.145, sqrt(1/17.24))

w_j_s <- ((exp(5*theta_m)/(1+exp(theta_m))^5*exp(-8*(theta_m)^2)*1/sqrt(2*pi*1/16))
        /(1/sqrt(2*pi*(1/15))*exp(-15*(theta_m-0.145)^2/2)))

p_j <- w_j_s/(sum(w_j_s))

SIR_dat <- sample(theta_m, 10000, prob=p_j)

hist(SIR_dat)

(1+exp(-mean(SIR_dat)))^(-1)
