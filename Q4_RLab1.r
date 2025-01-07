#lab1 3rd January 2025 by Arushi
##Q4
# (i)
x <- c(5, 3, 7, 8)
print(x)  # 5 3 7 8

# (ii) 
is.integer(x)  # FALSE because default type is numeric

# (iii) 
is.numeric(x)  # TRUE

# (iv) 
#x <- integer(x)  # ERROR: invalid 'length' argument so I commented it out

# Correct usage
x <- as.integer(c(5, 3, 7, 8))
print(x)  # 5 3 7 8
is.integer(x)  # TRUE
