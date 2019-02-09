library(readxl)
kites <- read_excel("kites.xlsx")
View(kites)
kites<-kites_1_
n=nrow(kites)
p=ncol(kites)
#The average wing length (according to Wikipedia) of a hook-billed kite is 190 and average tail length is 275
#Does our sample of 45 kites agree with Wikipedia?

#Calculating the mean vector and variance matrix
(meanvec <- apply(kites,2,mean))
(varmat <- var(kites))

#Writing the null hypothesis vector
nullvec <- c(190,275)

#Calculating the test statistic
T2 <- n*t(meanvec-nullvec)%*%solve(varmat)%*%(meanvec-nullvec)

#Calculating the critical value
(critval <- (n-1)*p/(n-p)*qf(.95,p,n-p))
(T2>critval)

#Since T2 is less than the critival value, we fail to reject the null hypothesis
#We claim we don't have evidence that wikipedia is wrong

#What if we wanted to test the size and the shape of the kite
#size = wing length + tail length
#shape = tail length -  wing length

#Transformation matrix
C <- rbind(c(1,1),c(-1,1))

#New null vector
(nullvec2 <- C%*%nullvec)

#New test statistic
T2.2 <- n*t(C%*%meanvec-nullvec2)%*%solve(C%*%varmat%*%t(C))%*%(C%*%meanvec-nullvec2)

#Since our new test is just a linear combination of the previous HT, we end up with the same results
