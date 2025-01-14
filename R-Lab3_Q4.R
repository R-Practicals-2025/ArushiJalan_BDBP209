#lab3 13th January 2025 by Arushi
##Q4 
# i)
vec <- c(4,7,6,5,6,7)
vec  # 4 7 6 5 6 7
class(vec) # "numeric"
length(vec) # 6
min(vec) # 4
max(vec) # 7
# ii)
vec <- scan()
# iii)
vec[4]
# iv)
ind <- c(2,3,6)
vec[ind]
# v)
vec[-1]
# vi)
vec[-length(vec)]
# vii)
x <- c(10,30,20,40,50,70,60)
x
trim <- function(x) sort(x) [c(-1,-2,-(length(x)-1), -(length(x)))]
trim(x)
# viii)
vec[1:3]
vec[seq(2,length(vec),2)]
#to get the elements that have even indices
vec<- c(2,4,3,8,1,5)
even_index_elements <- vec[(1:length(vec)) %% 2 == 0]
print(even_index_elements)
# ix)
x <- 0:10
sum(x[x<5])
# x)
#sum of three largest values of a vector
vec<- c(3,7,2,9,6)
sorted_vec<- sort(vec)
sorted_vec[(length(sorted_vec)-2):length(sorted_vec)]
sum_large_three_nums<- sum(sorted_vec)
print (sum_large_three_nums)
# xi)
vec<- c(3,7,2,9,6)
which.max(vec) #gives the index and not the element
which.min(vec)
# xii)
cbind(1:10,10:1)
rbind(1:10,10:1)
# xiii)
X <- c(1:10)
X
Y <- c(1:10*5)
Y
X*Y
X+Y
X/Y
X^Y
log(X)
exp(Y)
