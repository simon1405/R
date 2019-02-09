library(readxl)
countries <- data.frame(read_excel("C:/Users/jmarti52/Dropbox/Vermont STAT/Multivariate/Data/Other data/track.xlsx"))

track <- as.matrix(countries[,-1])
n <- nrow(track)
p <- ncol(track)

#Naming the rows of the data for each country
row.names(track) <- as.factor(countries[,1])


###Question 1
E <- eigen(cov(track))$vectors
row.names(E) <- colnames(track)
(lambda <- eigen(cov(track))$values)

mean(lambda)

#Scree plot
plot(1:p, lambda, main="Scree plot", xlab="i",ylab=expression(lambda));lines(1:p, lambda)

#Variability captured by the 1st PC
lambda[1]/sum(lambda)

E[,1:2]
#1st PC is the marathon time
#2nd PC is mostly the 400m dash time.

#Finding the 8 PCs
Z <- track%*%E

#Sorting the 1st PC. the larger the number= worse the country did
sort(Z[,1])

###Question 2
E <- eigen(cor(track))$vectors
row.names(E) <- colnames(track)
(lambda <- eigen(cor(track))$values)

#Scree plot
plot(1:p, lambda, main="Scree plot", xlab="i",ylab=expression(lambda));lines(1:p, lambda)

#Variability captured by 1st 2 PCs
sum(lambda[1:2])/sum(lambda)

#First 2 eigenvectors
E[,1:2]
#First eigenvector combines the z-scores for the run times
#Second eigenvector contrasts the z-scores for short runs to long runs.

#Standardizing the track scores
track.sc <- scale(track, center=T, scale=apply(track,2,sd))

#Transforming the data into the PCs
Z <- track.sc%*%E

#Sorting the data: Larger numbers=better
sort(Z[,1])

###Question 3
distances <- c(100,200,400,800/60,1500/60,5000/60,10000/60,42195/60)

#Converting the data into meters per second
track1 <- t(apply(track,1,function(x) distances/x))
 
#Calculating the eigenvalues and eigenvectors for speed
E <- eigen(cov(track1))$vectors
row.names(E) <- colnames(track)
(lambda <- eigen(cov(track1))$values)

#average eigenvalue
mean(lambda)

#Scree plot
plot(1:p, lambda, main="Scree plot", xlab="i",ylab=expression(lambda));lines(1:p,lambda)

#Percentage explained by first 2 PCs
lambda[1:2]/sum(lambda)

#The first 2 Eigenvalues
E[,1:2]
#1st PC is combined run speed, 2nd contrasts short running speed to long running speed

#Calculating the PCs
Z <- track1%*%E

#Sorting the 1st PC: Larger score = better
sort(Z[,1])

###Question 4:
###Part a:
apply(track,2,shapiro.test)
qqnorm(track[,8])
qqline(track[,8])

#Part c
library(psych)
?principal

#Choosing m=2
FA.PC2 <- principal(r=track, nfactors=2, rotate="varimax") 
L2 <- FA.PC2$loadings
Psi.2 <- diag(diag(cor(track)-(L2%*%t(L2))))
(Resmat2 <- cor(track)-(L2%*%t(L2)+Psi.2) )
max(Resmat2)

#Fits very well, may be able to decrease the number of factors
FA.PC1 <- principal(r=track, nfactors=1, rotate="varimax") 
L1 <- FA.PC1$loadings
Psi.1 <- diag(diag(cor(track)-(L1%*%t(L1))))
(Resmat1 <- cor(track)-(L1%*%t(L1)+Psi.1) )
max(Resmat1)

#1 factor fits much worse, will stick with 2 factors

#Interpreting the loadings
L2


#Part 4e:
#Since the data aren't multivariate Normal, we will use Bartlett's method for m=2.
ybar <- colMeans(track)
D <- sqrt(solve(diag(diag(cov(track)))))
track["usa",]
(f.USA <- solve(t(L2)%*%solve(Psi.2)%*%L2)%*%t(L2)%*%solve(Psi.2)%*%D%*%(track["usa",]-ybar))
(f.china <- solve(t(L2)%*%solve(Psi.2)%*%L2)%*%t(L2)%*%solve(Psi.2)%*%D%*%(track["china",]-ybar))




###Question 5:
pollution <- read.table("C:/Users/jmarti52/Dropbox/Vermont STAT/Multivariate/Data/JW data/T1-5.DAT",header=F)
colnames(pollution) <- c("wind", "solar","CO","NO","NO2","O3","HC")
apply(pollution,2,shapiro.test)
