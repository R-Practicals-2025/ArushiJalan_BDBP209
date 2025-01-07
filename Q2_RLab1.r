#lab1 3rd January 2025 by Arushi
##Q2
ceiling <- function(x) {
  y <- floor(x + 1)
  print(paste("The ceiling of", x, "is", y))
}

# Test cases
ceiling(2.2) # 3
ceiling(3.5) # 4
ceiling(4.8) # 5

