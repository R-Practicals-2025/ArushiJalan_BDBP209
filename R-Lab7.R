#lab7 14th February 2025 by Arushi
#1
# i)

amat <- matrix(seq(10, 120, by = 10), nrow = 3, ncol = 4, byrow = TRUE)
print("Matrix amat (byrow = TRUE):")
print(amat)
amat2 <- matrix(seq(10, 120, by = 10), nrow = 3, ncol = 4, byrow = FALSE)
print("Matrix amat2 (byrow = FALSE):")
print(amat2)
# amat fills numbers row-wise (left to right) amat2 fills numbers column-wise (top to bottom) amat2 is the transpose of amat
# ii)
amat <- matrix(seq(10, 120, by = 10), nrow = 3, ncol = 4, byrow = TRUE)
rownames(amat) <- c("R1", "R2", "R3")
colnames(amat) <- c("C1", "C2", "C3", "C4")
print("Matrix amat with row and column names:")
print(amat)

# iii)
A <- matrix(c(2, 5, 7, 3, 1, 8, 9, 10, 1, 12, 5, 10, 4, 17, 15, 11), nrow = 4, ncol = 4, byrow = TRUE)
B <- matrix(c(12, 5, 3, 17, 1, 18, 9, 10, 1, 12, 5, 10, 4, 15, 15, 4), nrow = 4, ncol = 4, byrow = TRUE)
print("Matrix A:")
print(A)
print("Matrix B:")
print(B)
element_wise<- A * B
print("Element-wise multiplication (A * B):")
print(element_wise)
matrix_multiplication_result <- A %*% B
print("Matrix-matrix multiplication (A %*% B):")
print(matrix_multiplication_result)
# iv)
X <- c(5, 6, 8, 9)
Y <- c(8, 10, 12, 5)
outer_product <- X %o% Y
print("Outer Product of X and Y:")
print(outer_product)
inner_product <- X %*% Y
print("Inner Product of X and Y:")
print(inner_product)

# v)
X <- c(5, 6, 8, 9)
n <- length(X)
diag_matrix <- matrix(0, n, n)
for (i in 1:n) {
  diag_matrix[i, i] <- X[i]
}
print("Diagonal Matrix:")
print(diag_matrix)

# vi)
A <- matrix(c(2, 5, 7, 3, 1, 8, 9, 10, 1, 12, 5, 10, 4, 17, 15, 11), nrow = 4, ncol = 4, byrow = TRUE)
print(A)
print(diag(A))

# vii)
print(diag(6))

# viii)
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow = 3, ncol = 3)
print(A)


# ix)
B <- matrix(c(5,-3,13), nrow = 3, ncol = 1)
print(B)

# x)
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow = 3, ncol = 3)
B <- c(5, -3, 2)
X <- solve(A,B)
print(X)
print(class(X))
print(typeof(X))

# xi)
Ainv = solve(A)
print(Ainv)
print(round(Ainv %*% A)) #rounding off the values

# xii)
result=eigen(A)
print(result)
# print(result$values)
# print(result$vectors)
second <- result$vectors[,2]
multiplication_result <- A %*% second
print(multiplication_result)

#2

# i)
data=read.csv("/home/ibab/R/BrainCancer.csv",header=TRUE)
result=data$gtv
square=result^2
data$new_col=data$time +square
print(head(data))

# ii)
print(rownames(data))
print(colnames(data))

# iii)
rownames(data) <- paste("Row", 1:nrow(data), sep="-")
print(data)

# iv)
data$ki <- NULL
print(colnames(data))

#3

# i)
install.packages("readxl")

# ii)
library(readxl) #loading the library

# iii)
data2=read_excel("/home/ibab/R/pone.0148733.s001.xlsx",1)

# iv)
print(names(data2))

#4

# i)

setA<- c("a","b","c","d","e")
setB<- c("d","e","f","g")
print(A)
print(B)

# ii)
union (setA, setB)

# iii)
intersect(setA, setB)

# iv)
setdiff(setA,setB)
setdiff(setB,setA)

# v)
setequal(c(setdiff(setA,setB),intersect(setA,setB),setdiff(setB,setA)),union(setA,setB))

# vi)
setB[setB %in% setA]

# vii)
setA[setA %in% setB]

#5
# i)
vec <- c(8,10,12,7,14,16,2,4,9,19,20,3,6)
vec[vec > 12]         
vec[vec > 10 & vec < 20]  

# ii)
A <- c(2,7,29,32,41,11,15,NA,NA,55,32,NA,42,109)
A_filtered <- A[!is.na(A) & A < 100]
print(A_filtered)

# iii)
A[is.na(A)] <- 0
print(A)        

# iv)
genes <- c("gene-1", "gene-2", "gene-3", "gene-4", "gene-5", "gene-6", "gene-7")
gender <- c("M", "M", "F", "M", "F", "F", "M")

# v)
result1 <- c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
result2 <- c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
result3 <- c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
result4 <- c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
result5 <- c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
result6 <- c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
result7 <- c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)

# vi)
dataframe1 <- data.frame(genes, gender, result1, result2, result3, result4, result5, result6, result7)
print(dataframe1)

# vii)
colnames(dataframe1) <- c("GeneName", "Gender", "expt1", "expt2", "expt3", "expt4", "expt5", "expt6", "expt7")

# viii)
subset_expt2 <- dataframe1[dataframe1$expt2 > 20, ]
print(subset_expt2)

# ix)
subset_female <- dataframe1[dataframe1$Gender == "F", ]
print(subset_female)

# x)
subset_male_expt2 <- dataframe1[dataframe1$Gender == "M" & dataframe1$expt2 < 30, ]
print(subset_male_expt2)


#6 

# i)
find_quadrant <- function(angle) {
  angle <- angle %% 360  
  
  if (angle > 0 & angle <= 90) {
    print("First quadrant")
  } else if (angle > 90 & angle <= 180) {
    print("Second quadrant")
  } else if (angle > 180 & angle <= 270) {
    print("Third quadrant")
  } else if (angle > 270 & angle < 360) {
    print("Fourth quadrant")
  } else {
    print("Angle lies on an axis or is 0/360 degrees")
  }
}

#calling the function
find_quadrant(45)  
find_quadrant(120)  
find_quadrant(360)  

# ii)
sort_num<- function(a, b, c) {
  if (a >= b & a >= c) {
    if (b >= c) {
      print(c(a, b, c))
    } else {
      print(c(a, c, b))
    }
  } else if (b >= a & b >= c) {
    if (a >= c) {
      print(c(b, a, c))
    } else {
      print(c(b, c, a))
    }
  } else {
    if (a >= b) {
      print(c(c, a, b))
    } else {
      print(c(c, b, a))
    }
  }
}

#sorting all the numbers in descending order
sort_num(15, 9, 20) 

# iii)
ticket_cost <- function(d, age) {
  if (d <= 100) {
    cost <- 100
  } else if (d <= 1000) {
    cost <- 100 + (d - 100) * 1.50
  } else {
    cost <- 100 + (900 * 1.50) + (d - 1000) * 2
  }
  
  
  if (age > 60) {
    cost <- cost * 0.75  
  } else if (age < 6) {
    cost <- cost * 0.50  
  }
  
  print(paste("Ticket cost: Rs.", round(cost, 2)))
}

ticket_cost(500, 65)  # Senior citizen discount 
ticket_cost(1200, 5)  # Child discount 
ticket_cost(2000, 40) # Normal fare for 2000 km


#7

# i)
replace <- function(vec) {
  vec[vec < 0] <- 0
  return(vec)
}

v <- c(-5, 10, -3, 8, -1, 7)
replace(v)  



# ii)
stirling <- function(n) 
{
  pi <- sqrt(2 * pi * n)
  power<- (n / exp(1))^n
  corr<- 1 + (1 / (12 * n)) + (1 / (288 * n^2)) - (139 / (51840 * n^3)) - (571 / (2488320 * n^4))
  return(pi * power* corr)
}

stirling(5) 


# iii)
sum_digits <- function(num) {
  total <- 0
  while (num > 0) {
    total <- total + (num %% 10)
    num <- num %/% 10  
  }
  return(total)
}

sum_digits(9876)  