#Entering the sample mean vectors
ybar <- c(8.75, 5.33, 8.50, 4.75)
xbar <- c(12.57, 9.57, 11.49, 7.97)
ny <- 37
nx <- 12
p <- length(xbar)
#Entering the varcov matrix
varmat <- rbind(c(11.26, 9.41, 7.16, 3.38),
                c(9.41, 13.53, 7.38, 2.50),
                c(7.16, 7.38, 11.58, 2.62),
                c(3.38, 2.50,  2.62, 5.81))


#Calculating Hotelling's T2
(T2 <- (nx*ny)/(nx+ny)*t(xbar-ybar)%*%solve(varmat)%*%(xbar-ybar))

#Finding the critical value
critval <- p*(nx+ny-2)/(nx+ny-p-1)*qf(.95,p, nx+ny-p-1)

#Should we reject Ho?
T2>critval

#Finding the pvalue
a <- 1/(p*(nx+ny-2)/(nx+ny-p-1))
1-pf(a*T2, p, nx+ny-p-1)


#####Using the Bonferroni method to find the differences#####
#First, find the critical value using the Bonferroni correction
(critval2 <- qt(1-.05/p,ny+nx-2))


#Now we need to find the test statistics for each test score
meandiff <- xbar-ybar
(sediff <- sqrt((1/nx+1/ny)*diag(varmat)))
tstat <- abs(meandiff/sediff)

#Which test score differs across the groups?
tstat>critval2
1-pt(tstat,ny+nx-2)

#As it turns out, all of them!

###But which ones are the most different?###
solve(varmat)%*%meandiff



