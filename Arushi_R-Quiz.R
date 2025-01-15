#Quiz on 15th January by Arushi
#Question 1

arithmatic_operations <- function(k,n) {
  A <- k+n
  print(paste("The addition of",k,"and",n, "is", A))
  S <- k-n
  print(paste("The subtraction of",k,"and",n, " is", S))
  M <- k*n
  print(paste("The multiplication of",k,"and",n, " is", M))
  D <- k/n
  print(paste("The division of",k,"by",n, " is", D))
  Q <- k%/%n
  print(paste("The quotient of division of",k,"by",n, " is", Q))
  R <- k%%n
  print(paste("The remainder of division of",k,"by",n, " is", R))
  E <- k**n
  print(paste("The exponential of",k,"to the power of",n, " is", E))
}
arithmatic_operations(3,2)


#Question 2
Roots <- function(a,b,c) {
  first_root <- ((-b) + (((b**2) - (4*a*c))**0.5))/(2*a)
  second_root <- ((+b) + (((b**2) - (4*a*c))**0.5))/(2*a)
  print(paste("The roots are", first_root, "and", second_root))
}
Roots(2,6,4)
