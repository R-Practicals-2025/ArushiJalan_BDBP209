#lab3 13th January 2025 by Arushi
##Q5
y <- 1:24
dim(y) <- c(2,4,3)
y
#, , 1

###### [,1] [,2] [,3] [,4]
#[1,]    1    3    5    7
#[2,]    2    4    6    8

#, , 2

####### [,1] [,2] [,3] [,4]
#[1,]    9   11   13   15
#[2,]   10   12   14   16

#, , 3

####### [,1] [,2] [,3] [,4]
#[1,]    17   19   21   23
#[2,]    18   20   22   24

#i)
X <- matrix (c(1,0,0,0,1,0,0,0,1),nrow=3)
print(X)
####### [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1

#ii)
vector <- c(1,2,3,4,4,3,2,1)
V <- matrix(vector,byrow=T,nrow=2) #method 1
V
####### [,1] [,2] [,3] [,4]
#[1,]    1    2    3    4
#[2,]    4    3    2    1
#to print column wise
#V <- matrix(vector,byrow=F,nrow=2) #method 1
#V
dim(vector) <- c(4,2) #method 2
is.matrix(vector) # TRUE
vector
####### [,1] [,2]
#[1,]    1    4
#[2,]    2    3
#[3,]    3    2
#[4,]    4    1
dim(vector) <- c(4,2,1) 
is.matrix(vector) # FALSE
vector
#, , 1

####### [,1] [,2]
#[1,]    1    4
#[2,]    2    3
#[3,]    3    2
#[4,]    4    1