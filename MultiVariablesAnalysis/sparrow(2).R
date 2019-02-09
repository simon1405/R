sparrow <- read.table("sparrow.txt", header=T)
View(sparrow)

sparrow<-read.table("~/stat223/sparrow(1).txt",header=T)

#Removing the birdno column
sparrow<- as.matrix(sparrow[,-1])
n <- nrow(sparrow)
p <- ncol(sparrow)


#looking at the qqplots for all 5 variables
for (i in 1:5){
  qqnorm(sparrow[,i])
  qqline(sparrow[,i])
}

#Performing the Shapiro Wilks test for normality for all 5 variables
apply(sparrow,2,shapiro.test)

#calculating the mean vector and variance-covariance matrix
(meanvec <- apply(sparrow,2,mean))
(varmat <- cov(sparrow))

#Calculating the d2, the quadratic form for each observation
d2 <- NULL
for (i in 1:nrow(sparrow)){
  d2 <- c(d2,t(sparrow[i,]-meanvec)%*%solve(varmat)%*%(sparrow[i,]-meanvec))
}
d2
#Creating the Q-Q plot for the chisquared
j=1:n
chisqpoints <- qchisq((j-1/2)/n,df=p)
qqplot(chisqpoints,d2, main="Chisq Q-Q Plot")
points(chisqpoints,chisqpoints,type="l")




##Finding d2 removing the total length##
d2 <- NULL
for (i in 1:nrow(sparrow[,-1])){
  d2 <- c(d2,t(sparrow[i,-1]-meanvec[-1])%*%solve(varmat[-1,-1])%*%(sparrow[i,-1]-meanvec[-1]))
}

chisqpoints <- qchisq((j-1/2)/n,df=4)
qqplot(chisqpoints,d2, main="Chisq Q-Q Plot")
points(chisqpoints,chisqpoints,type="l")

library(MASS)
m1<-lm(sparrow[,1]~1)
bc<-boxcox(m1,lambda=(seq(-4,4,by=0.1)))
max.loc<-which(bc$y==max(bc$y))
bc$x[max.loc]
