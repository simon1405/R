###Problem 2.1
A <- rbind(c(4,2,3),
           c(7,5,8))

B <- rbind(c(3,-2,4),
           c(9,6,-5))
#(a)
A+B

#(b)
t(A)%*%A
A%*%t(A)


###Problem 2.3
A <- matrix(c(1,3,2,-1),nrow=2,ncol=2, byrow=T)
B <- matrix(c(2,0,1,5),nrow=2,ncol=2, byrow=T)

#(a)
A%*%B
B%*%A

#(b)
det(A%*%B)
det(A)
det(B)


###Problem 2.11
a <- c(1,-3,2)
b <- c(2,1,3)

#(a)
t(a)%*%b
(t(a)%*%b)^2

#(b)
b%*%t(b)
t(a)%*%b%*%t(b)%*%a

#(c)
(t(a)%*%b)^2 == t(a)%*%b%*%t(b)%*%a


###Problem 2.15
A <- matrix(c(5,4,4,2,-3,1,3,7,2), nrow=3, ncol=3, byrow=T)
B <- matrix(c(1,0,1,0,1,0,1,2,3), nrow=3, ncol=3, byrow=T)

#(a)
sum(diag(A))
sum(diag(B))

#(b)
A+B
sum(diag(A+B))
sum(diag(A+B))==sum(diag(A))+sum(diag(B))

#(c)
det(A)
det(B)

#(d)
A%*%B
det(A%*%B)
det(A%*%B)==det(A)*det(B)


###Problem 2.21
A <- matrix(c(2,-1,-1,2),nrow=2,byrow=T)

(e <- eigen(A))

A2 <- e$vectors%*%diag(sqrt(e$values))%*%t(e$vectors)
A2%*%A2



###########################Chapter 3###########################################
###Problem 3.10
calcium <- as.matrix(T3_4_CALCIUM[,-1])
n = nrow(calcium)
p <- ncol(calcium)

#(a)
I <- diag(rep(1,n))
J <- matrix(1,nrow=n, ncol=n)
S <- 1/(n-1)*t(calcium)%*%(I-1/n*J)%*%calcium
var(calcium)

#(b)
r12 <- S[1,2]/sqrt(S[1,1]*S[2,2])
r13 <- S[1,3]/sqrt(S[1,1]*S[3,3])
r23 <- S[2,3]/sqrt(S[3,3]*S[2,2])
cor(calcium)

#(c)
D <- sqrt(diag(diag(S)))
R <- solve(D)%*%S%*%solve(D)
cor(calcium)


###Problem 3.11
calcium <- as.matrix(T3_4_CALCIUM[,-1])
n = nrow(calcium)
p <- ncol(calcium)

#(a)
det(var(calcium))

#(b)
sum(diag(var(calcium)))


###Problem 3.17
calcium <- as.matrix(T3_4_CALCIUM[,-1])
n = nrow(calcium)
p <- ncol(calcium)

#(a)
C <- rbind(c(1,1,1),
           c(2,-3,2),
           c(-1,-2,-3))
meanvec <- apply(calcium,2,mean)
zmean <- C%*%meanvec

zS <- C%*%var(calcium)%*%t(C)

#alternatively transforming y to z first
Z <- t(C%*%t(calcium))
1/n*t(Z)%*%matrix(1,nrow=n,ncol=1)
var(Z)

#(b)
D <- sqrt(diag(diag(zS)))
solve(D)%*%zS%*%solve(D)
cor(Z)


###Problem 3.20
bone <- as.matrix(T3_7_BONE[,-1])

C <- rbind(c(2,3,-1,4),
           c(-2,-1,4,-2),
           c(3,-2,-1,3))

meanvec <- apply(bone,2,mean)
varmat <- var(bone)
cor(bone)

zmean <- C%*%meanvec
zvar <-  C%*%varmat%*%t(C)
D <- sqrt(diag(diag(zvar)))
solve(D)%*%zvar%*%solve(D)


Z <- bone%*%t(C)
apply(Z,2,mean)
var(Z)
cor(Z)
