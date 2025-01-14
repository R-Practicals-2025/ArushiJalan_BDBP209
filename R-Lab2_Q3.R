#lab2 10th January 2025 by Arushi
##Q3
# i)
3/0 # Inf
# ii)
exp(-Inf) # 0
# iii)
(0:3)**Inf #  0   1 Inf Inf
# iv)
0/0 #NaN
# v)
Inf - Inf #NaN
# vi)
Inf/Inf #NaN
# vii)
is.finite(10) #TRUE
# viii)
is.infinite(10) #FALSE
# ix)
is.infinite(Inf) # TRUE
# x)
y<- c(4,NA,7)
y=="NA"  #FALSE NA FALSE
is.na(y) #FALSE  TRUE FALSE
# xi)
y[!is.na(y)] # 4 7
# xii)
c1<- c(1,2,3,NA)
c2<- c(5,6,NA,8)
c3<- c(9,NA,11,12)
c4<- c(NA,14,15,16)
full.frame <- data.frame(c1,c2,c3,c4)
full.frame
#   c1 c2 c3 c4
# 1  1  5  9 NA
# 2  2  6 NA 14
# 3  3 NA 11 15
# 4 NA  8 12 16
reduced.frame <- full.frame[! is.na(full.frame$c1),]
reduced.frame
#    c1 c2 c3 c4
# 1  1  5  9 NA
# 2  2  6 NA 14
# 3  3 NA 11 15
# xiii)
v <- c(1:6,NA,NA,9:12)
seq(along=v)[is.na(v)] # 7 8
which(is.na(v)) # 7 8 %>% 