# 7th March 2025
# 9th Lab by Arushi

# Q1
x = 2
y = 4
plot(x,y, pch=0, col='magenta')


# Q2
x <- seq(-pi, pi, 0.5)
y1 = sin(x)
y2 = cos(x)
plot(x, y1, pch=8, main="sin(x) vs cos(x)", col='blue', type='o', ylim=c(-2, 2), xlab='value of pi', ylab='trignometric functions')
lines(x, y2, pch=4,col='red', type='o')
legend('bottomright', inset=0.01,c('sin(x)', 'cos(x)'), lty=1, col=c('blue', 'red'), title='graph functions')


# Q3

x=c(1,2,3,4,5,6,7,8)
y=c(.2088,.1582,.1313,.1313,.1953,.1246,.0135,.0370)
a=barplot(names.arg=x,y, main="Bar Plot", xlab="x (Number of assistance programs)", ylab="Probability", ylim = c(0, 0.25), yaxt="n", xaxt = "n", space=0.6) 
axis(1, at = a, labels =x, tcl = 0.3, cex.axis = 0.9)
axis(2, tick = TRUE, tcl = 0.3, las=2) 
abline(h=0,lwd=3)
plot(x,y,type="h",lwd=10)


# Q4
# i) 
par(mfrow=c(2,3))  
x <- seq(-pi, pi,0.1)  
plot(x, cos(x), type="l", col="red", main="Cosine function", xlab="X-axis", ylab="cos(x)")

# ii) 
plot(x, (x^2/3)+4.2 , type="b",lwd=2,lty=1, col="violet", main="Quadratic Function", xlab="X-axis", ylab="(x^2/3) + 4.2 ")

# iii) 
binom_func <- function(n, m) {
  return(factorial(n) / (factorial(m) * factorial(n - m)))
}
bionom_dist <- function(n, p, m) {
  return(binom_func(n, m) * (p^m) * ((1 - p)^(n - m)))
}
n = 12
p = 0.3
m = 0:n
probabilities1 = sapply(m, function(m) bionom_dist(n, p, m))
hist(rep(m, times = round(probabilities1 * 1000)), xlab = "Successes", ylab = "Frequency", main = "Binomial Distribution(n=12, p=0.3)")

# iv)
n = 12
p = 0.8
m = 0:n
probabilities1 = sapply(m, function(m) bionom_dist(n, p, m))
hist(rep(m, times = round(probabilities1 * 1000)), xlab = "Successes", ylab = "Frequency", main = "Binomial Distribution(n=12, p=0.3)")

# v)
x=seq(1,10,0.5)
y=(50*x)/(x + 2)
colors <- rep(c("blue", "orange"))
plot(x,y,col=colors,type="h",main="Histogram plot",xlab="X-axis",ylab="Y-axis",lend=1,lwd=8)

# vi)
x <- seq(1, 10)  
plot(x, log(x), type="s", col="orange", main="Log function", xlab="x", ylab="log(x)")


# Q5
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
x <- 1:10
y <- c(2, 4.5, 7, 6, 5, 7, 10, 9, 8, 10)

plot(x, y, type = "l", col = "red", lty = "dashed", lwd = 2, xlab = "Time", ylab = "Performance", main = "This is a graph", col.main = "blue", font.main = 2, cex.main = 2, ylim = c(2, 10))
legend("topleft", legend = "Per curve", col = "red", lty = 2, inset = 0.05, box.lty = 1)

odd_x = x[x %% 2 == 1]
odd_x <- c(odd_x, 10)
odd_y = y[x %% 2 == 1]
odd_y <- c(odd_y, 12)

points(odd_x, odd_y, col = "red", pch = 16, cex = 1)
text(odd_x, odd_y, labels = odd_x, col = "red", pos = 3, cex = 1)


# Q6
chooseR <- function(n, k) {
  if (k > n || k < 0) return(0) 
  return(exp(lfactorial(n) - (lfactorial(k) + lfactorial(n - k))))
}
hypergeo_pmf <- function(k, N, K, n) {
  return((chooseR(K, k) * chooseR(N - K, n - k)) / chooseR(N, n))
}

# Defining parameters
N <- 500  # Total population size
K <- 50   # Total successes in population
n <- 30   # Sample size
num_samples <- 1000  # Number of samples for histogram

# Defining possible values of k (successes in the sample)
k_min <- max(0, n - (N - K))  # Lower bound for k
k_max <- min(n, K)            # Upper bound for k
k_values <- seq(k_min, k_max, by=1)  # Generate k values

pmf_values <- sapply(k_values, function(k) hypergeo_pmf(k, N, K, n))
sampled_data <- rhyper(num_samples, K, N - K, n)

hist(sampled_data, breaks=seq(k_min - 0.5, k_max + 0.5, by=1), col="lightblue", border="black", main="Hypergeometric Distribution (N=500, K=50, n=30)", xlab="Number of Successes in Sample (k)", ylab="Frequency", las=1, freq=TRUE)

# Q7

binomial_prob <- function(k, n, p) {
  choose(n, k) * (p^k) * ((1 - p)^(n - k))
}
hypergeometric_prob <- function(k, N, K, n) {
  (choose(K, k) * choose(N - K, n - k)) / choose(N, n)
}

N <- 50    
K <- 25    
n_values <- seq(5, 45, length.out = 9)  

par(mfrow = c(3, 3))

for (n in n_values) {
  x <- 0:min(K, n)  
  
  hyper_probs <- sapply(x, function(k) hypergeometric_prob(k, N, K, n))
  binom_probs <- sapply(x, function(k) binomial_prob(k, n, K / N))
  
  plot(x, hyper_probs, type = "h", col = "blue", lwd = 2, ylim = c(0, max(c(hyper_probs, binom_probs))), xlab = "Successes", ylab = "Probability", main = paste("n =", n)) lines(x, binom_probs, col = "red", type = "l")  
}

# Q8

poisson_pmf <- function(k, lambda) {
  return((exp(-lambda) * lambda^k) / factorial(k))
}

lambda_values <- c(3, 20, 45)

k_max <- max(lambda_values) + 10  
k_values <- 0:k_max

colors <- c("blue", "red", "green")

plot(k_values, poisson_pmf(k_values, lambda_values[1]), type="h", lwd=2, col=colors[1], 
     xlab="k (Occurrences)", ylab="P(X=k)", main="Poisson Distributions", 
     ylim=c(0, 0.2))

for (i in 2:length(lambda_values)) {
  lines(k_values, poisson_pmf(k_values, lambda_values[i]), type="h", lwd=2, col=colors[i])
}

legend("topright", legend=paste("λ =", lambda_values), col=colors, lwd=2 , bty="n")

# Q9
# i)
data <- read.csv("/home/ibab/SOCR-HeightWeight.csv")
heights <- data$Height.Inches
heights <- heights[!is.na(heights)]
height_mean <- sum(heights) / length(heights)
height_sd <- sqrt(sum((heights - height_mean)^2) / length(heights))

hist(heights, breaks=30, probability=TRUE, col="lightblue", border="black",
     main="Histogram of Heights", xlab="Height (inches)",
     ylab="Density")
cat("Height - Mean:", height_mean, "SD:", height_sd, "\n")

# ii)

weights <- data$Weight.Pounds
weights <- weights[!is.na(weights)]
mean_weight <- sum(weights) / length(weights)
std_weight <- sqrt(sum((weights - mean_weight)^2) / length(weights))
cat("Mean Weight:", mean_weight, "\nStandard Deviation of Weight:", std_weight, "\n")

hist(weights, breaks=30, col="lightblue", border="black", probability=TRUE,
     main="Histogram of Weights", xlab="Weight (pounds)", ylab="Density")


# iii) 

Z_height <- (heights - mean_height) / height_sd
Z_weight <- (weights - mean_weight) / weight_sd

gaussian_pdf <- function(z) {
  return((1 / sqrt(2 * pi)) * exp(-0.5 * z^2))
}
z_values <- seq(-4, 4, length.out = 100)
pdf_values_height <- gaussian_pdf(z_values)
pdf_values_weight <- gaussian_pdf(z_values)

plot(z_values, pdf_values_height, type="l", col="blue", lwd=2, lty=1, main="Gaussian Curve for Height & Weight (Z-transformed)", xlab="Z-score", ylab="P(Z)")

lines(z_values, pdf_values_weight, col="red", lwd=2, lty=2)
legend("topright", legend=c("Height", "Weight"), col=c("blue", "red"), lwd=2, lty=c(1, 2))

# iv)

par(mfrow=c(1,3)) 

hist(heights, breaks=10, col="lightblue", border="black", main="Bins = 10", xlab="Height", ylab="Frequency")
hist(heights, breaks=30, col="lightblue", border="black", main="Bins = 30", xlab="Height", ylab="Frequency")
hist(heights, breaks=50, col="lightblue", border="black", main="Bins = 50", xlab="Height", ylab="Frequency")


# Q10

uniform_pdf <- function(x, a=1, b=2) {
  ifelse(x >= a & x <= b, 1 / (b - a), 0)
}

uniform_cpdf <- function(x, a=1, b=2) {
  ifelse(x < a, 0, ifelse(x > b, 1, (x - a) / (b - a)))
}

x_vals <- seq(0, 3, 0.01)
pdf_vals <- uniform_pdf(x_vals)
cpdf_vals <- uniform_cpdf(x_vals)

plot(x_vals, pdf_vals, type="l", col="blue", lwd=2, main="Uniform PDF (U(1,2))", xlab="x", ylab="Density")
polygon(c(1, seq(1, 1.5, 0.01), 1.5), c(0, uniform_pdf(seq(1, 1.5, 0.01)), 0), col=rgb(0, 0, 1, 0.2))

plot(x_vals, cpdf_vals, type="l", col="red", lwd=2, main="Uniform CPDF (U(1,2))", xlab="x", ylab="Probability")



# Q11

exp_pdf <- function(x, lambda=10) {
  ifelse(x >= 0, lambda * exp(-lambda * x), 0)
}

exp_cpdf <- function(x, lambda=10) {
  ifelse(x >= 0, 1 - exp(-lambda * x), 0)
}

x_vals <- seq(0, 1, 0.01)
pdf_vals <- exp_pdf(x_vals)
cpdf_vals <- exp_cpdf(x_vals)

plot(x_vals, pdf_vals, type="l", col="blue", lwd=2, main="Exponential PDF (lambda=10)", xlab="x", ylab="Density")

polygon(c(0, seq(0, 2.8, 0.01), 2.8), c(0, exp_pdf(seq(0, 2.8, 0.01)), 0), col=rgb(0, 0, 1, 0.2))
plot(x_vals, cpdf_vals, type="l", col="red", lwd=2, main="Exponential CPDF (lambda=10)", xlab="x", ylab="Probability")


# Q12

gamma_pdf <- function(x, alpha=5, theta=3) {
  ifelse(x > 0, (x^(alpha-1) * exp(-x/theta)) / (theta^alpha * gamma(alpha)), 0)
}

gamma_cpdf <- function(x, alpha=5, theta=3) {
  pgamma(x, shape=alpha, scale=theta)
}

x_vals <- seq(0, 40, 0.1)
pdf_vals <- gamma_pdf(x_vals)
cpdf_vals <- gamma_cpdf(x_vals)

plot(x_vals, pdf_vals, type="l", col="blue", lwd=2, main="Gamma PDF (alpha=5, theta=3)", xlab="x", ylab="Density")
plot(x_vals, cpdf_vals, type="l", col="red", lwd=2, main="Gamma CPDF (alpha=5, theta=3)", xlab="x", ylab="Probability")


# Q13

chisq_pdf <- function(x, df=20) {
  ifelse(x > 0, (x^(df/2 - 1) * exp(-x/2)) / (2^(df/2) * gamma(df/2)), 0)
}

chisq_cpdf <- function(x, df=20) {
  pchisq(x, df=df)
}

x_vals <- seq(0, 50, 0.1)
pdf_vals <- chisq_pdf(x_vals)
cpdf_vals <- chisq_cpdf(x_vals)

plot(x_vals, pdf_vals, type="l", col="blue", lwd=2, main="Chi-Square PDF (df=20)", xlab="x", ylab="Density")
polygon(c(0, seq(0, 1.0, 0.01), 1.0), c(0, chisq_pdf(seq(0, 1.0, 0.01)), 0), col=rgb(0, 0, 1, 0.2))

plot(x_vals, cpdf_vals, type="l", col="red", lwd=2, main="Chi-Square CPDF (df=20)", xlab="x", ylab="Probability")