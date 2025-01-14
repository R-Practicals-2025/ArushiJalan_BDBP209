#lab2 10th January 2025 by Arushi
##Q1
# i)
round(12.1343,digits=3) # 12.134
# ii)
round(123.12344,digits=3) # 123.123
# iii)
round(1234.12344,digits=3) # 1234.123
# iv)
round(12345.12344,digits=3) # 12345.123
# v)
options(digits=15)
# vi)
formatC(round(12345.12344,digits=3),format="f",digits=3) # "12345.123"
# vii)
print(1234.12344) # 1234.12344
# viii)
print(1234.723,digits=3) # 1235
print(1234.723,digits=5) # 1234.7
# ix)
round(123456788.123,digits=3) # 123456788.1
# x)
print(round(123456788.123,digits=2),digits=20) # 123456788.12000000477
# xi)
print(round(123456789.1234,digits=4),digits=20) # 123456789.12340000272
# xii)
paste("Hello World") # "Hello World"
paste("Hello","World") # "Hello World"
# xiii)
paste(1:10) # "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"
paste(1:10)[4] # "4"
# xiv)
as.numeric(paste(1:10)) # 1  2  3  4  5  6  7  8  9 10
# xv)
paste(1:10,collapose=".") # "1 ."  "2 ."  "3 ."  "4 ."  "5 ."  "6 ."  "7 ."  "8 ."  "9 ."  "10 ."
# xvi)
paste(c("Hello","World"), 1:10,sep="-") # "Hello-1"  "World-2"  "Hello-3"  "World-4"  "Hello-5"  "World-6"  "Hello-7" "World-8"  "Hello-9"  "World-10"
print(paste("Hello",1:10,sep="-")) # "Hello-1"  "Hello-2"  "Hello-3"  "Hello-4"  "Hello-5"  "Hello-6"  "Hello-7" "Hello-8"  "Hello-9"  "Hello-10"