#lab3 13th January 2025 by Arushi
##Q6
#i)
vec<- c(11,2,83,94,45,62,71,18)
print (min(vec)) #2
print (max(vec)) #94
print(sum(vec)) #386
print(range(vec)) #2 94
print(sort(vec)) #2 11 18 45 62 71 83 94

#ii)
M <- matrix(c(11,2,83,94,45,62,71,18), nrow = 2)
colMeans(M) #  6.5 88.5 53.5 44.5

#iii)
X <- matrix(c(1, 2, 3), nrow = 1)  
Y <- matrix(c(4, 5, 6), ncol = 1)
Z <- X[1:4] %o% Y[1:3]
Z
#####  [,1] [,2] [,3]
#[1,]    4    5    6
#[2,]    8   10   12
#[3,]   12   15   18
#[4,]   NA   NA   NA
YoX <- Y[1:3] %o% X[1:4]
YoX
###### [,1] [,2] [,3] [,4]
#[1,]    4    8   12   NA
#[2,]    5   10   15   NA
#[3,]    6   12   18   NA
t(Z) #transpose
###### [,1] [,2] [,3] [,4]
#[1,]    4    8   12   NA
#[2,]    5   10   15   NA
#[3,]    6   12   18   NA
t(YoX)
####### [,1] [,2] [,3]
#[1,]    4    5    6
#[2,]    8   10   12
#[3,]   12   15   18
#[4,]   NA   NA   NA
dot<- X %*% Y #dot product
print(dot)
###### [,1]
#[1,]   32
X <- matrix(c(1, 2, 3), nrow = 1)  
Y <- matrix(c(4, 5, 6), nrow = 1) 
sum(X*Y) #another way to carry out dot product # 32
crossprod(X[1:4],Z)
diag(4) #identity matrix
######  [,1] [,2] [,3] [,4]
#[1,]    1    0    0    0
#[2,]    0    1    0    0
#[3,]    0    0    1    0
#[4,]    0    0    0    1
#iv)
class(X) #"matrix" "array"