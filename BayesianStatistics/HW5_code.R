#Problem 1 Part B Gibbs Sampler

#n and k are iterations and discarded samples respectively

n <- 3000
k <- 1000

#initial values

theta_1 <- 1
theta_2 <- 1

#initialize values
dat_theta_1 <- theta_1
dat_theta_2 <- theta_2

set.seed(2)

for(i in 1:(n-1)){
  if(theta_2==1){
    theta_1 <- sample(c(1,2,2), 1)
  }
  if(theta_2==2){
    theta_1 <- sample(c(1,1,2), 1)
  }
  if(theta_2==3){
    theta_1 <- sample(c(1,1,1,2), 1)
  }
  if(theta_1==1){
    theta_2 <- sample(c(1,2,2,3,3,3), 1)
  }
  if(theta_1==2){
    theta_2 <- sample(c(1,1,2,3), 1)
  }
  dat_theta_1 <- c(theta_1, dat_theta_1)
  dat_theta_2 <- c(theta_2, dat_theta_2)
}

#Remove the first k observations

dat_theta_post <- data.frame(cbind(dat_theta_1[(k+1):n], dat_theta_2[(k+1):n]))

#Generate posterior probabilities

p_1_1 <- length(dat_theta_post[dat_theta_post$X1==1&dat_theta_post$X2==1,1])/(n-k)

p_1_2 <- length(dat_theta_post[dat_theta_post$X1==1&dat_theta_post$X2==2,1])/(n-k)

p_1_3 <- length(dat_theta_post[dat_theta_post$X1==1&dat_theta_post$X2==3,1])/(n-k)

p_2_1 <- length(dat_theta_post[dat_theta_post$X1==2&dat_theta_post$X2==1,1])/(n-k)

p_2_2 <- length(dat_theta_post[dat_theta_post$X1==2&dat_theta_post$X2==2,1])/(n-k)

p_2_3 <- length(dat_theta_post[dat_theta_post$X1==2&dat_theta_post$X2==3,1])/(n-k)

p_1_1+p_1_2+p_1_3+p_2_1+p_2_2+p_2_3

#Problem 3 Gibbs Sampler

#specify n & k

n <- 5000
k <- 1000

#input data

N <- 4

Y_vec <- c(8,9,9,10)

a <- 1

b <- 1

#initialize values

lambda <- 1
beta <- 1

lambda_dat <- lambda
beta_dat <- beta

#run sampler

for(i in 1:(n-1)){
  lambda <- rgamma(1,sum(Y_vec)+2, rate=beta+N)
  beta <- rgamma(1, a, rate=lambda+b)
  lambda_dat <- c(lambda_dat, lambda)
  beta_dat <- c(beta_dat, beta)
}

#remove first k iterations

dat_pois_post <- data.frame(cbind(beta_dat[(k+1):n], lambda_dat[(k+1):n]))

#find 95% credible intervals

lambda_cred <- quantile(dat_pois_post$X2, c(0.25, 0.975))
beta_cred <- quantile(dat_pois_post$X1, c(0.25,0.975))

#make histograms

hist(dat_pois_post$X2, main="Posterior of Lambda, a=b=0.001", breaks=20, xlab='Lambda')
hist(dat_pois_post$X1, main="Posterior of Beta, a=b=0.001", xlab='Beta', breaks=20)
