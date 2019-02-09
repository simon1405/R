#####################################Problem 1#########################################
library(readxl)
USStates <-  read_excel("C:/Users/jmarti52/Dropbox/Vermont STAT/Multivariate/Exam/Exam Data/USStates.xlsx")

states <- USStates[,12:16]
N <- nrow(states)
p <- ncol(states)

#Separating the data by states that voted for Obama and McCain
obama <- subset(states,USStates$ObamaMcCain=="O")
mccain <- subset(states,USStates$ObamaMcCain=="M")

#Calculating the sample sizes
n1 <- nrow(obama)
n2 <- nrow(mccain)

#Finding the mean vectors
mean.o <-colMeans(obama)
mean.m <-colMeans(mccain)

#Finding the pooled variance
E <- (n1-1)*var(obama)+(n2-1)*var(mccain)
Spl <- E/(N-2)

###Part a)
T2 <- (n1*n2)/(n1+n2)*t(mean.o-mean.m)%*%solve(Spl)%*%(mean.o-mean.m)
crit.val <- p*(N-2)/(N-p-1)*qf(.95,p,(N-p-1))
#We reject the null hypothesis and say the two mean vectors are not the same
#There is a difference in demographics of states that voted for MC vs OB

###Part b)
#Calculating the discriminant function
(a <- solve(Spl)%*%(mean.o-mean.m))
(a.star <- sqrt(diag(Spl))*solve(Spl)%*%(mean.o-mean.m))

#The order of importance of the demographics in separating the groups is:
#Smokers, Obesity, College degree, Physical activity, then Non-white percentage.

###Part c) 
#Calculating the individual t-tests statistics
SE <- sqrt((1/n1+1/n2)*diag(Spl))
(t.stat <- (mean.o-mean.m)/SE)

#The order of importance is College, Obesity, Physical activity, smokers, then Non-white


###Part d)
T2.partial <- rep(-1,p)#Storing the partial T2 tests

#Looping through each variable and calculating T2 without it
for(i in 1:p){
  T2.partial[i] <- (n1*n2)/(n1+n2)*t(mean.o-mean.m)[-i]%*%solve(Spl[-i,-i])%*%(mean.o-mean.m)[-i]
}

#Calculating the partial F
(F.partial <- (N-2-p+1)*(T2-T2.partial)/(N-2+T2.partial))

#The order of importance of group separation for each variable is:
#College, Smokers, Obesity, Non-white percentage, then Physical activity

###Part f)
#Finding the difference in means and standard deviation of the transformed data
zbar.diff <- t(a)%*%(mean.o-mean.m)
SE <- sqrt((1/n1+1/n2)*t(a)%*%Spl%*%a)

#The largest t stat we can get using a linear combination of the demographic variables is
(z.tstat <- zbar.diff/SE)

#The test statistic using the transformed data is larger than the test statisics
#from the individual variables.

#######################################################################################
#####################################Problem 2#########################################
#######################################################################################

library(readxl)
USStates <- read_excel("C:/Users/jmarti52/Dropbox/Vermont STAT/Multivariate/Exam/Exam Data/USStates.xlsx")

states <- USStates[,c(6,12:16)]
N <- nrow(states)
p <- ncol(states)-1
k <- 2

########################################Part a)######################################
library(MASS)

#Finding the classification rule model
m.lda <- lda(ObamaMcCain~., data=states, prior=rep(1,k)/k)

#Predicting which candidate each state will vote for
pred.lda <- predict(m.lda)$class 

#Creating the confusion matrix
(tab.lda <- table(states$ObamaMcCain,pred.lda))#Comparing our predictions and R predictions

#Calculating the apparent error rate
(aer.lda <- 1-sum(diag(tab.lda))/sum(tab.lda))

##############################Part b)#############################################
library(class)
#We will set k= sqrt(25)=5
m.knn <- knn(train=states[,-1], test=states[,-1], cl = states$ObamaMcCain, k=5)

#Creating the confusion matrix
(tab.knn <- table(Actual = states$ObamaMcCain, Predicted = m.knn))

#Calculating the apparent error rate
(aer.knn <- 1- sum(diag(tab.knn))/sum(tab.knn))


##########################Part c)##############################################
library(rpart)
library(rpart.plot)

#Creating the classification tree
m.ct <- rpart(ObamaMcCain~., data=states, method="class")

#Creating the diagram of the classification tree
rpart.plot(m.ct, main= "Candidate Tree for US States", type=0,extra=101)

#Calculating the confusion matrix
p.ct <- predict(m.ct, states[,-1], type="class")

(tab.ct <- table(Actual = states$ObamaMcCain, Predicted = p.ct))

#calculating the apparent error rate
(aer.ct <- 1-sum(diag(tab.ct))/sum(tab.ct))

#####################Part d: Cross-validation#########################################
######Creating the confusion matrix using LDA with cross-validation#####
m.lda.cv <- lda(ObamaMcCain~., data=states, prior=rep(1,k)/k, CV=T)

tab.lda.cv <- table(Actual = states$ObamaMcCain,Predicted = m.lda.cv$class)

(cver.lda <- 1-sum(diag(tab.lda.cv))/sum(tab.lda.cv))




######Creating the confusion matrix using knn with cross-validation#####
#We will set k= sqrt(25)=5
m.knn.cv <- knn.cv(train=USStates[,c(12:16)], cl = states$ObamaMcCain, k=5)

#Creating the confusion matrix
(tab.knn.cv <- table(Actual = states$ObamaMcCain, Predicted = m.knn.cv))

#Calculating the error rate
(cver.knn <- 1- sum(diag(tab.knn.cv))/sum(tab.knn.cv))





#####Creating the confusion matrix using classification tree with cross-validation####
pred.ct.cv <- rep(0,N)#Creating a vector to hold the predictions

#Looping through each state, removing it, 
#then predicting which candidate it voted for
for (i in 1:N){
  m.ct.cv <- rpart(ObamaMcCain~., data=states[-i,], method="class")
  pred.ct.cv[i] <- predict(m.ct.cv, states[i,-1], type="class") 
}

#Creating the cross-validated confusion matrix and corresponding error
(tab.ct.cv <- table(Actual = states$ObamaMcCain, Predicted = pred.ct.cv))
(cver.ct <- 1-sum(diag(tab.ct.cv))/sum(tab.ct.cv))

#########################Part E)####################################
#Using a true/false vector to determine if the state was predicted to vote for Obama
obama <- m.lda.cv$class=="M"

#Calculating the number of electoral votes we would predict him to earn
(obama.ev <- sum(obama*USStates$ElectoralVotes))

#We would have predicted McCain to have won the election using the demographic
#info of each state.



#######################################################################################
#####################################Problem 3#########################################
#######################################################################################
###Reading in the Data
iris <- read.table("C:/Users/jmarti52/Dropbox/Vermont STAT/Multivariate/Data/Other data/iris.txt", header=T)
N <- nrow(iris)
p <- ncol(iris)-1
k <- length(unique(iris$Species))

###################################Part b)###########################################
#Calculating E and H
m.iris <- manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)~Species, data=iris)
E <- summary(m.iris,test="Wilks")$SS$Residuals
H <- summary(m.iris,test="Wilks")$SS$Species

#Calculating the eigenvalues and eigen vectors
(evals <- eigen(solve(E)%*%H)$values)
evecs <- eigen(solve(E)%*%H)$vectors


###############################################Part c)#############################
#Calculating the proportion of separation for each eigenvectors
evals/sum(evals)

#The first eigenvalue accounts over 99% of the sum of the eigenvalues.
#We only need one discriminant function



######################################Part d)######################################
evecs[,1]

#It appears that the petal length and petal width are the most important variables
#to separate the three subspecies.


#####################################Part e)#######################################
#Transforming the data using the first two eigenvectors
z1 <- as.matrix(iris[,-5])%*%evecs[,1]
z2 <- as.matrix(iris[,-5])%*%evecs[,2]

#Creating the scatterplot using the xyplot function in the lattice package
library(lattice)
xyplot(z2~z1,group=iris$Species, pch=19, auto.key = T)

#We can clearly see that the first discriminant function does an excellent job in 
#separating the 3 subspecies, but the second does almost nothing.

#######################################Part f)########################################
#Calculating the full lambda
m.full <- summary(manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)~
                         Species, data=iris), test="Wilks")$stats[1,2]

#Calculating the partial lambda for the four variables
m.sl <- summary(manova(cbind(Sepal.Width, Petal.Length, Petal.Width)~
                       Species, data=iris), test="Wilks")$stats[1,2]

m.sw <- summary(manova(cbind(Sepal.Length, Petal.Length, Petal.Width)~
                           Species, data=iris), test="Wilks")$stats[1,2]

m.pl <- summary(manova(cbind(Sepal.Length, Sepal.Width, Petal.Width)~
                           Species, data=iris), test="Wilks")$stats[1,2]

m.pw <- summary(manova(cbind(Sepal.Length, Sepal.Width, Petal.Length)~
                           Species, data=iris), test="Wilks")$stats[1,2]

#Calculating the ratio of the full vs partial lambda for each variable
lambda <- m.full/c(m.sl,m.sw,m.pl,m.pw)

#Calculating the partial F for each variable
(partial.F <- (N-k-p+1)/(k-1)*(1-lambda)/lambda)

#The order of importance of each variable using the partial F method:
#1) Petal Length, 2) Petal Width, 3) Sepal Width, 4) Sepal Length

evecs[,1]
#The partial F method reverses the importance of Petal width and length
#but sepal length and width stay the same.



##################################Part g)###########################################
crit.val <- qf(.95,k-1,N-k-p+1)

#All the partial F values are greater than the critical value; each variable 
#contributes to the separation of the subspecies.



#######################################################################################
#####################################Problem 3#########################################
#######################################################################################
#Reading in the data
iris <- read.table("C:/Users/jmarti52/Dropbox/Vermont STAT/Multivariate/Data/Other data/iris.txt", header=T)
N <- nrow(iris)
p <- ncol(iris)-1
k <- length(unique(iris$Species))

########################################Part a)######################################
library(MASS)

#Finding the classification rule model
m.lda <- lda(Species~., data=iris)

#Predicting which candidate each state will vote for
pred.lda <- predict(m.lda)$class 

#Creating the confusion matrix
(tab.lda <- table(Actual = iris$Species, Predicted = pred.lda))#Comparing our predictions and R predictions

#Calculating the apparent error rate
(aer.lda <- 1-sum(diag(tab.lda))/sum(tab.lda))

##############################Part b)#############################################
library(class)
#We will set k= sqrt(50)=7
m.knn <- knn(train=iris[,-5], test=iris[,-5], cl = iris$Species, k=7)

#Creating the confusion matrix
(tab.knn <- table(Actual = iris$Species, Predicted = m.knn))

#Calculating the apparent error rate
(aer.knn <- 1- sum(diag(tab.knn))/sum(tab.knn))


##########################Part c)##############################################
library(rpart)
library(rpart.plot)

#Creating the classification tree
m.ct <- rpart(Species~., data=iris, method="class")

#Creating the diagram of the classification tree
rpart.plot(m.ct, main= "Candidate Tree for Iris Subspecies", type=0,extra=101)

#Calculating the confusion matrix
p.ct <- predict(m.ct, iris[,-5], type="class")

(tab.ct <- table(Actual = iris$Species, Predicted = p.ct))

#calculating the apparent error rate
(aer.ct <- 1-sum(diag(tab.ct))/sum(tab.ct))



#####################Part d: Cross-validation#########################################
######Creating the confusion matrix using LDA with cross-validation#####
m.lda.cv <- lda(Species~., data=iris, CV=T)

tab.lda.cv <- table(Actual = iris$Species, Predicted = m.lda.cv$class)

(cver.lda <- 1-sum(diag(tab.lda.cv))/sum(tab.lda.cv))




######Creating the confusion matrix using knn with cross-validation#####
#We will set k= sqrt(50)=7
m.knn.cv <- knn.cv(train=iris[,-5], cl = iris$Species, k=7)

#Creating the confusion matrix
(tab.knn.cv <- table(Actual = iris$Species, Predicted = m.knn.cv))

#Calculating the error rate
(cver.knn <- 1- sum(diag(tab.knn.cv))/sum(tab.knn.cv))





#####Creating the confusion matrix using classification tree with cross-validation####
pred.ct.cv <- rep(0,N)#Creating a vector to hold the predictions

#Looping through each state, removing it, 
#then predicting which candidate it voted for
for (i in 1:N){
  m.ct.cv <- rpart(Species~., data=iris[-i,], method="class")
  pred.ct.cv[i] <- predict(m.ct.cv, iris[i,-5], type="class") 
}

#Creating the cross-validated confusion matrix and corresponding error
(tab.ct.cv <- table(Actual = iris$Species, Predicted = pred.ct.cv))
(cver.ct <- 1-sum(diag(tab.ct.cv))/sum(tab.ct.cv))



