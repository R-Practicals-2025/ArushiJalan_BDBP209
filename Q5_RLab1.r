#lab1 3rd January 2025 by Arushi
##Q5
# i)
x <- sqrt(2) # assigning value to x
print(x) # 1.414214
# ii)
x*x ==2 # FALSE because the value will be slightly less than 2
# iii)
x*x - 2 #not zero due to rounding off error
# iv)
all.equal(x*x,2) #TRUE because this function checks for equality within a tolerance, accounting for small rounding errors.