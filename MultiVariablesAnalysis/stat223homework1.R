#Stat223 Homework #1
#Date 1/16/2018
#Name:Simon (Zihao Huang )

#2.1

A<-matrix(c(4,7,2,5,3,8),nrow=2,ncol=3)
B<-matrix(c(3,6,-2,9,4,-5),nrow=2,ncol=3)
A+B
#[,1] [,2] [,3]
#[1,]    7    0    7
#[2,]   13   14    3
A-B
#     [,1] [,2] [,3]
#[1,]    1    4   -1
#[2,]    1   -4   13
t(A)%*%A
#     [,1] [,2]
#[1,]    4    7
#[2,]    2    5
#[3,]    3    8
A%*%t(A)
#     [,1] [,2]
#[1,]   29   62
#[2,]   62  138

#2.3
A<-matrix(c(1,2,3,-1),nrow=2,ncol=2)
B<-matrix(c(2,1,0,5),nrow=2,ncol=2)
A%*%B
#     [,1] [,2]
#[1,]    5   15
#[2,]    3   -5
B%*%A
#     [,1] [,2]
#[1,]    2    6
#[2,]   11   -2
det(A%*%B)
#-70
det(A)
#-7
det(B)
#10
#Verification: |AB|=|A|*|B|

#2.11
a<-matrix(c(1,-3,2),ncol=1)
b<-matrix(c(2,1,3),ncol=1)
#a)
t(a)%*%b
# 5
(t(a)%*%b)^2
#25
#b)
b%*%t(b)
#     [,1] [,2] [,3]
#[1,]    4    2    6
#[2,]    2    1    3
#[3,]    6    3    9
t(a)%*%(b%*%t(b))%*%a
#25
#c)
#(a'b)^2=a'(bb')a

#2.15
a<-matrix(c(5,2,3,4,-3,7,4,1,2),nrow=3)
b<-matrix(c(1,0,1,0,1,2,1,0,3),nrow=3)
#a)
sum(diag(a))
sum(diag(b))
#tr(a)=5-3+2=4
#tr(b)=1+1+3=5
#b)
sum(diag(a+b))
#tr(a+b)=6-2+5=9
#tr(a+b)=tr(a)+tr(b)
#c
det(a)
#23
det(b)
#2
#d
a*b
#     [,1] [,2] [,3]
#[1,]    5    0    4
#[2,]    0   -3    0
#[3,]    3   14    6
det(a%*%b)
#46
#|AB|=|a||b|

#2.21
a<-matrix(c(2,-1,-1,2),nrow=2)
eigen(a)
#eigenvalues: 3 1
#egienvectors:
#           [,1]       [,2]
#[1,] -0.7071068 -0.7071068
#[2,]  0.7071068 -0.7071068
#sqrtm(a)
#  1.3660254 -0.3660254
# -0.3660254  1.3660254


c<-eigen(a)$vectors
d0.5<-diag(diag(diag(sqrt(eigen(a)$values))))
c%*%d0.5%*%solve(c)

#3.10
#a
a310<-matrix(c(1,35,3.5,2.80,
               2,35,4.9,2.70,
               3,40,30.0,4.38,
               4,10,2.8,3.21,
               5,6, 2.7,2.73,
               6,20,2.8,2.81,
               7,35,4.6,2.88,
               8,35,10.9,2.90,
               9,35,8.0,3.28,
               10,30,1.6,3.20),byrow=T,nrow=10,ncol=4)
a.3.10<-a310[,2:4]
s<-cov(a.3.10)
#[1,] 140.544444 49.680000 1.9412222
#[2,]  49.680000 72.248444 3.6760889
#[3,]   1.941222  3.676089 0.2501211
#b
r12<-s[1,2]/sqrt(s[1,1]*s[2,2])
r13<-s[1,3]/sqrt(s[1,1]*s[3,3])
r23<-s[2,3]/sqrt(s[2,2]*s[3,3])
#c
R<-matrix(rep(0,9),nrow=3)
i<-1
j<-1
for (i in 1:3){
  for (j in 1:3){
     if (i==j)  {R[i,j]=1}
    if (i!=j)  {R[i,j]=s[i,j]/sqrt(s[i,i]*s[j,j])}
   }
}
#[1,] 1.0000000 0.4930154 0.327411
#[2,] 0.4930154 1.0000000 0.864762
#[3,] 0.3274110 0.8647620 1.000000

#3.11
#a
a.3.11<-a310[,2:4]
det(cov(a.3.11))
#det=459.9555
#b
sum(diag(cov(a.3.11)))
#213.043

#3.17
#a
a.3.17<-a310[,2:4]
y1<-a.3.17[,1]
y2<-a.3.17[,2]
y3<-a.3.17[,3]
z1<-y1+y2+y3
z2<-2*y1-3*y2+2*y3
z3<-(-1)*y1-1*y2-3*y3
A<-matrix(c(1,2,-1,1,-3,-2,1,2,-3),nrow=3)
(zbar<-A%*%colMeans(a.3.17))
#zbar
#[1,]  38.369
#[2,]  40.838
#[3,] -51.727
(Sz<-A%*%s%*%t(A))
#[1,]  323.6376  19.2526 -460.9770
#[2,]   19.2526 588.6710  104.0717
#[3,] -460.9770 104.0717  686.269
#b
Rz<-matrix(rep(0,9),nrow=3)
i<-1
j<-1
for (i in 1:3){
  for (j in 1:3){
    if (i==j)  {Rz[i,j]=1}
    if (i!=j)  {Rz[i,j]=Sz[i,j]/sqrt(Sz[i,i]*Sz[j,j])}
  }
}
Rz
#[1,]  1.00000000 0.04410862 -0.9781430
#[2,]  0.04410862 1.00000000  0.1637378
#[3,] -0.97814302 0.16373782  1.0000000

#3.20
A<-matrix(c(2,-2,3,3,-1,-2,-1,4,-1,4,-2,3),nrow=3,ncol=4)
table3.7<-read.table("C:/Users/simon/Downloads/T3_7_BONE.DAT",header = F)
a.3.20<-as.matrix(table3.7[,2:5])
#a
(zbar<-A%*%colMeans(a.3.20))
#[1,] 401.395
#[2,] -47.545
#[3,] 150.480
S<-var(a.3.20)
(sz<-A%*%S%*%t(A))
#b
#[1,] 398.33103 -44.34655 148.35095
#[2,] -44.34655  12.36050 -16.89674
#[3,] 148.35095 -16.89674  59.46379
#
rz<-matrix(rep(0,9),nrow=3)
i<-1
j<-1
for (i in 1:3){
  for (j in 1:3){
    if (i==j)  {rz[i,j]=1}
    if (i!=j)  {rz[i,j]=sz[i,j]/sqrt(sz[i,i]*sz[j,j])}
  }
}
rz
#[1,]  1.0000000 -0.6320039  0.9639222
#[2,] -0.6320039  1.0000000 -0.6232446
#[3,]  0.9639222 -0.6232446  1.0000000

#3.21
table3.8<-read.table("C:/Users/simon/Downloads/T3_8_SONS.DAT",header = F)
a.3.21<-table3.8
ybar<-colMeans(a.3.21[,1:2])
#ybar is (185.72 151.12)
xbar<-colMeans(a.3.21[,3:4])
#xbar is (183.84 149.24)
yxbar<-colMeans(a.3.21[,1:4])

cov(a.3.21[,1:2],a.3.21[,1:2])
cov(a.3.21[,1:2],a.3.21[,3:4])
cov(a.3.21[,3:4],a.3.21[,1:2])
cov(a.3.21[,3:4],a.3.21[,3:4])
# 95.29333 52.86833 | 69.66167 46.11167
# 52.86833 54.36000 | 51.31167 35.05333
# -------------------------------------
# 69.66167 46.11167 | 100.8067 56.54000
# 51.31167 35.05333 | 56.5400 45.02333


