#lab2 10th January 2025 by Arushi
##Q2
# i)
0:10 # 0  1  2  3  4  5  6  7  8  9 10
# ii)
15:5 # 15 14 13 12 11 10  9  8  7  6  5
# iii)
seq(0,1.5,0.1) # 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5
# iv)
seq(6,4,-0.2) # 6.0 5.8 5.6 5.4 5.2 5.0 4.8 4.6 4.4 4.2 4.0
# v)
N <- c(55,76,92,103,84,88,121,91,65,77,99)
print(N) #55  76  92 103  84  88 121  91  65  77  99
# vi)
seq(from=0.04,by=0.01,length=11) # 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14
seq(0.04,by=0.01,along=N) # 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14
# vii)
seq(from=0.04,to=0.14,along=N) # 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14
# viii)
sequence(c(4,3,4,4,4,5)) # 1 2 3 4 1 2 3 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 5
# ix)
rep(9,5) # 9 9 9 9 9
rep(1:4,2) # 1 2 3 4 1 2 3 4
rep(1:4,each=2) # 1 1 2 2 3 3 4 4
rep(1:4,each=2,times=3) # 1 1 2 2 3 3 4 4 1 1 2 2 3 3 4 4 1 1 2 2 3 3 4 4
rep(1:4,1:4) # 1 2 2 3 3 3 4 4 4 4
# x)
rep(1:4,c(4,1,4,2)) # 1 1 1 1 2 3 3 3 3 4 4
rep(c("cat","dog","goldfish","rat"), c(2,3,2,1)) # "cat"      "cat"      "dog"      "dog"      "dog"      "goldfish" "goldfish" "rat"
# xi)
seq(-1,1,by=0.1) # -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1  0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7  0.8  0.9  1.0
seq(-1,1,0.1) # -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1  0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7  0.8  0.9  1.0
# xii)
seq(-1,1,length=7) # -1.0000000000 -0.6666666667 -0.3333333333  0.0000000000  0.3333333333  0.6666666667  1.0000000000
# xiii)