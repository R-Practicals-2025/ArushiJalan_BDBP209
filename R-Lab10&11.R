# Learning goals: Probability density functions relevant to statistical analyses, random deviates,
# sampling exercise to demonstrate central limit theorem and ROC curves.
# This lab uses the data in winequality-red.csv and winequality-white.csv data sets.
# Reference for these datasets:
# P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. ,Modeling wine preferences by data
# mining from physicochemical properties. Decision Support Systems, Elsevier, 47(4):547-553,
# 2009 and
# For intrinsic distribution functions in R, the following nomenclature is used:
# ‘d’<distn name> : probability density value is returned (for eg. pbinom)
# ‘p’<distn name>: cumulative probability value is returned up to specified x value
# ‘q’<distn name> : returns the x value up to which the input cumulative probability represents
# (also called the quantile function)
# ‘r’<distn name> : returns m random deviates from the distribution

# EXERCISES

# #I. Sampling, permutations and combinations

# #1) Sampling from a vector: One can sample from a set of items with or wihout replacement
# Test the result of the following commands:
# x <- seq(1,100)
# s <- sample(x,10)
# Is the above result happening with or without replacement?
# Try the command sample(x,10,replace=TRUE)

x <- seq(1,100)
print(x)
s <- sample(x,10)
print(s)
sample(x, 10, replace = TRUE)


# 2) The package gtools has permutations and combinations functions that should be used
# as follows.
# install.packages("gtools")
# library(gtools)
# x <- c("A","B","C","D")
# per <- permutations (n=length(x), r=3,v=x,repeats.allowed=TRUE)
# print(per)
# comb <- combinations(n=length(x), r=3, v=x)
# where r is the size of the target vector, v is the source vector, n is the size of the source
# vector.

install.packages("gtools") 
library(gtools)

x <- c("A", "B", "C", "D")
per <- permutations(n = length(x), r = 3, v = x, repeats.allowed = TRUE)
print(per)

comb <- combinations(n = length(x), r = 3, v = x)
print(comb)


# II. Distributions
# (1) Binomial distribution: The shape of this PDF is decided by the parameter p. Use
# n=10, p=0.4 and m=3 to do the following:
n <- 10
p <- 0.4
m <- 3
# (a) Print the probability value for the above combination of numbers. The syntax is
# dbinom(m,n,p).
dbinom(m,n,p)

# (b) Print the cumulative probability value for the above. Syntax: pbinom(m,n,p).
pbinom(m,n,p)

# (c) Find the m value corresponding to cumulative probability of 0.8. Syntax: qbinom(cum_prob,n,p)
cum_prob <- 0.8
qbinom(cum_prob,n,p)

# (d) Print 5 points randomly sampled from the Binomial distribution using rbinom(npts,n,p).
npts <- 5
rbinom(npts,n,p)

# (e) Plot the probability density function (PDF) for the above parameters. On the same
# plot, plot the PDF for p=0.7 with a different colour.
x_vals <- 0:10  # Possible values of X (successes)
pdf_0.4 <- dbinom(x_vals, size = 10, prob = 0.4)
pdf_0.7 <- dbinom(x_vals, size = 10, prob = 0.7)

plot(x_vals, pdf_0.4, type="b", col="pink", pch=8, xlab="Number of Successes", 
     ylab="Probability", main="Binomial PDFs (n=10)")
lines(x_vals, pdf_0.7, type="b", col="purple", pch=8)
legend("topright", legend=c("p=0.4", "p=0.7"), col=c("pink", "purple"), pch=8)


# (f) Generate 100 and 10000 points randomly from this distribution and make a frequency
# table of the sampled points. Plot these as bar plots in a 2x1 grid.

par(mfrow=c(2,1))

# for 100 points
sample_100 <- rbinom(100, size = 10, prob = 0.4)
freq_100 <- table(sample_100)
barplot(freq_100, col="mistyrose2", main="Frequency Distribution (n=100)")
box()

# for 10000 points
sample_10000 <- rbinom(10000, size = 10, prob = 0.4)
freq_10000 <- table(sample_10000)
barplot(freq_10000, col="peachpuff", main="Frequency Distribution (n=10000)")
box()

par(mfrow=c(1,1)) # to reset

# (2) Hypergeometric distribution: 
# The functions pertaining to this distribution <distn name>hyper. 
# The parameters are N, K, n and k (see class notes). 
# Carry out the following:

# (a) Plot a histogram type plot of the hypergeometric probability density function with
# N=100, K=70, p=0.3, n=12 and add text within the plot window of the parameter
# names and their values.
# Define the parameters
N <- 100  # Population size
K <- 70   # Number of success items in population
n <- 12   # Sample size

# Compute probability density values for all possible x values
x_vals <- 0:n  # Possible values of k (successes in sample)
pdf_vals <- dhyper(x_vals, K, N-K, n)

# Plot the histogram
barplot(pdf_vals, names.arg=x_vals, col="slategray2", main="Hypergeometric PDF (N=100, K=70, n=12)", xlab="Number of Successes in Sample", ylab="Probability")

# Add text inside plot window
text(4, max(pdf_vals) * 0.8, labels="N=100, K=70, n=12", col="lightpink2")

# (b) Compute the cumulative probability up to x=10 and print the result after rounding
# off to 3 decimal places.
cum_prob <- phyper(10, K, N-K, n)
round(cum_prob, 3)

# (c) Obtain the x value corresponding to a cumulative probability value of 0.9.
qhyper(0.9, K, N-K, n)

# (d) Sample 5 points randomly from this distribution and print these with two significant
# digits.
sample_points <- rhyper(5, K, N-K, n)
format(sample_points, digits=2)


# (3) Geometric distribution: 
# The functions pertaining to this distribution <distn name> use geom. 
# The parameters are p and m(trial number at which the first success is observed).

# (a) Plot 2 probability density functions for this distribution in a 1x2 grid with 
par(mfrow=c(1,2))
x_vals <- 1:10 # First success can occur at any trial m >= 1
# (i)p=0.3 
p1 <- 0.3
pdf_p1 <- dgeom(x_vals - 1, p1)
barplot(pdf_p1, names.arg=x_vals, col="lightsteelblue2",main="Geometric PDF (p=0.3)",
        xlab="Trial Number (m)", ylab="Probability")

# (ii) p=0.8. 
p2 <- 0.8
pdf_p2 <- dgeom(x_vals - 1, p2)
barplot(pdf_p2, names.arg=x_vals, col="thistle2",main="Geometric PDF (p=0.8)",
        xlab="Trial Number (m)", ylab="Probability")
# What differences do you see?
par(mfrow=c(1,1)) # to reset

# (b) Compute the cumulative probability up to x=4.
pgeom(4 - 1, prob = 0.3) 

# (c) Compute the value of m at which the cumulative probabilty is 0.2.
qgeom(0.2, prob = 0.3) + 1 

# (d) Generate 6 random deviates or sample points from this distribution with p=0.4.
rgeom(6, prob = 0.4) + 1  


# 4) Negative binomial distribution: The functions pertaining to this distribution uses
# nbinom. The parameters are p, r (number of successes desired) and y (number of failures before r successes).

# a) Compute and print the negative binomial probability density for y=5, r=3 and
# p=0.3
dnbinom(5, size = 3, prob = 0.3)

# b) Compute and print the cumulative negative binomial probability density up to y=5.
pnbinom(5, size = 3, prob = 0.3)

# c) What is the y value corresponding to a cumulative probabilty value of 0.5? (ie the median)
qnbinom(0.5, size = 3, prob = 0.3)

# d) Print 4 random points sampled from this distribution with r=3 and p=0.3.
rnbinom(4, size = 3, prob = 0.3)

# e) Plot the negative binomial distribution function using r=10, p=0.3.
# Define parameters
r <- 10
p <- 0.3
y_vals <- 0:30  # Range of possible failures

# Compute probability density function
pdf_vals <- dnbinom(y_vals, size = r, prob = p)

# Plot the probability distribution
barplot(pdf_vals, names.arg=y_vals, col="navajowhite", 
        main="Negative Binomial Distribution (r=10, p=0.3)",
        xlab="Number of Failures (y)", ylab="Probability")

# f) Generate a frequency histogram of 10,000 random deviates from this distribution
# with r=10 and p=0.3.
# Generate 10,000 random deviates
samples <- rnbinom(10000, size = 10, prob = 0.3)

# Create a histogram
hist(samples, breaks=30, col="plum2", main="Histogram of Negative Binomial Distribution", xlab="Number of Failures", ylab="Frequency", border="black")


# 5) Poisson distribution: The functions pertaining to this distribution uses pois.
# The key parameter is λ and the discrete variable being m.

# a) Compute and print the Poisson probability given λ = 10 and m = 7.
dpois(7, lambda = 10)

# b) Calculate and print the cumulative probability for the same values above.
ppois(7, lambda = 10)

# c) Make two barplots showing a binomial probability distribution with n = 1000, p = 0.3
# and a Poisson PDF with λ = np. Do the two distributions agree? Why? Why not?
n <- 1000
p <- 0.019
lambda <- n * p  
x_vals <- 1:1000
binom_probs <- dbinom(x_vals, size = n, prob = p)
pois_probs <- dpois(x_vals, lambda = lambda)
par(mfrow=c(1,2))
plot(binom_probs, type='h', xlim=c(0,40), col="hotpink1", lwd=3, xlab="Number of Successes", ylab="Probability", ylim=c(0, 0.12)) 
lines(pois_probs, type="h", col="steelblue2", lwd=3)      
legend("topright", legend=paste(c('bionomial(n=1000,p=0.3)', 'poisson(λ = np)')), col=c('hotpink1', 'steelblue2'), lwd=3)

# d) Find the quantile value corresponding to cumulative probability of 0.22 and λ = 10.
qpois(0.22, lambda = 10)

# e) Obtain 10000 random sample points from a Poisson distribution with λ = 9 and
# make a histogram plot.

samples <- rpois(10000, lambda = 9)
hist(samples, breaks=30, col="linen", main="Histogram of Poisson Distribution (λ=9)", xlab="Number of Events", ylab="Frequency", border="black")


# 6) Gaussian distribution: The functions in R for the Gaussian/Normal distribution are
# for the unit normal distribution with the suffix norm. As we know the relevant parameters are μ and σ.

# a) Compute and print the unit normal PDF value for μ = 12 and σ = 2.
dnorm(12, mean = 12, sd = 2)

# b) Calculate and print the cumulative probability for Z = 2.0. Is this same as 1-CPDF(Z=-2)?
pnorm(2)
1 - pnorm(-2)

# c) Plot a unit normal curve for the above parameters with X range of ±4σ and add a
# text box to the plot showing the parameter symbols and their values.
mu <- 12
sigma <- 2
x_vals <- seq(mu - 4*sigma, mu + 4*sigma, length=100)
y_vals <- dnorm(x_vals, mean=mu, sd=sigma)

plot(x_vals, y_vals, type="l", col="orchid2", lwd=2, main="Normal Distribution (μ=12, σ=2)",xlab="X", ylab="Density")

text(mu, max(y_vals)/2, labels = expression(paste(mu, " = 12, ", sigma, " = 2")),col="magenta4", cex=1.2)

# d) Generate the 75th quantile point for a unit normal distribution with the above parameters.
qnorm(0.75, mean = 12, sd = 2)

# e) Generate 10,000 random deviates from the unit normal distribution and plot a histogram.
# On top of this plot the unit normal curve from which you sampled.
# Generate 10,000 random samples
samples <- rnorm(10000, mean = 12, sd = 2)

# Plot histogram
hist(samples, breaks=30, probability=TRUE, col="lightcyan1",
     main="Histogram of Normal Samples", xlab="X")

# Overlay normal curve
curve(dnorm(x, mean=12, sd=2), add=TRUE, col="dodgerblue1", lwd=2)

# f) Make a histogram plot of a ‘normalised’ binomial distribution with μ = np = 10 and p = 0.5. 
# ‘Normalised’ here means computing a variable of type W = m−np/√np(1−p) where m is the number of successes
# in n trials. On top of this, plot a unit normal distribution N(np,np(1-p). 
# Do the two distributions agree?
# Define parameters
n <- 100
p <- 0.5
mu <- n * p
sigma <- sqrt(n * p * (1 - p))
m_values <- rbinom(10000, size=n, prob=p)
W_values <- (m_values - mu) / sigma
hist(W_values, breaks=30, probability=TRUE, col="lavender",main="Normalized Binomial Distribution vs Normal Curve", xlab="W")
curve(dnorm(x, mean=0, sd=1), add=TRUE, col="darkmagenta", lwd=2)

# g) Plot 4 Poisson probability distribution functions corresponding to λ values 1, 10,
# 100 and 1000 in the same graph. On the same graph of each Poisson PDF plot,
# plot a unit normal distribution with Z = m−λ/√λ .
# For what value of λ(s) do the two plots agree? Use a 2x2 grid for this problem.

lambda_vals <- c(1, 10, 100, 1000)
par(mfrow=c(2,2))

for (lambda in lambda_vals) {
  x_vals <- seq(0, 2*lambda, by=1)
  pois_vals <- dpois(x_vals, lambda=lambda)
  
  # Poisson PMF as a stem plot
  plot(x_vals, pois_vals, type="h", col="plum1", lwd=2, 
       main=paste("Poisson (λ =", lambda, ") vs Normal"), xlab="X", ylab="Probability")
  
  # Normal approximation
  norm_vals <- dnorm(x_vals, mean=lambda, sd=sqrt(lambda))
  lines(x_vals, norm_vals, col="olivedrab", lwd=2)
}


# h) The library MASS is used to generate two or more vectors of normally distributed
# random numbers that are correlated with one another to a specified degree. For example,
#  xy <- mvrnorm(1000, mu=c(50,60), matrix(c(4,3.7,3.7,9),2))
# will generate two sets of 1000 numbers each with a mean of 50 for the first set and
# 60 for the second set. The matrix option specifies the covariance matrix of the variables.

# i) Execute var(xy) to check how close the variances are to our specified values
# what is the covariance from these sample sets?

install.packages("MASS")
library(MASS)
xy <- mvrnorm(1000, mu=c(50,60), Sigma=matrix(c(4,3.7,3.7,9),2,2))
var(xy)

# ii) Extract the separate vectors x and y as x <- xy[,1] and y <- [xy,2] and
# plot them to look at the correlation. Print the individual variances as var(x) and var(y).
x <- xy[,1]
y <- xy[,2]
plot(x, y, main="Scatter Plot of Correlated Variables", col="plum4")
var(x)
var(y)

# iii) Are the two samples independent? If so then the sum of their variances
# should be equal to the variance of their sum.
var(x + y)
var(x) + var(y)

# iv) The reported covariance is var(xy). Compute the covariance using the correlation coefficient cor(x,y)
# and the individual variances and make sure this matches with the reported value.
cov_xy <- cor(x, y) * sqrt(var(x) * var(y))
print(cov_xy)

# 7) Uniform distribution: The function prefix here unif and the two parameters are the
# x-limits.

# a) Generate 5 uniform random numbers between 0 and 1 and print them.
runif(5, min=0, max=1)

# b) Generate 5 random samples from a uniform distribution between 50 and 100.
runif(5, min=50, max=100)

# c) Generate 10,000 uniform deviates and plot a histogram with x-limits 1 and 2.
# Generate 10,000 uniform random numbers between 1 and 2
uniform_samples <- runif(10000, min=1, max=2)
print(uniform_samples)

# Plot histogram
hist(uniform_samples, breaks=30, probability=TRUE, col="lavender",
     main="Histogram of Uniform Distribution U(1,2)", xlab="X", xlim=c(1,2))


# 8) Exponential distribution: Since this is derived from the Poisson distribution, the
# primary parameter here is λ. The function suffix in R is exp.

# a) What is the probability density corresponding to x = 3 and λ = 2?
dexp(3, rate=2)

# b) What is the quantile value corresponding to cumulative probability value of 0.995
# for the above distribution?
qexp(0.995, rate=2)

# c) Plot the exponential cumulative probability distributions on the same graph for
# λ = 2, 10 and 100.
x_vals <- seq(0, 2, length=100)

cdf_2 <- pexp(x_vals, rate=2)
cdf_10 <- pexp(x_vals, rate=10)
cdf_100 <- pexp(x_vals, rate=100)

plot(x_vals, cdf_2, type="l", col="pink1", lwd=4, main="Exponential CDF for Different Lambda Values", xlab="X", ylab="Cumulative Probability")
lines(x_vals, cdf_10, col="palegoldenrod", lwd=4)
lines(x_vals, cdf_100, col="palegreen", lwd=4)

legend("bottomright", legend=c("λ = 2", "λ = 10", "λ = 100"),
       col=c("pink1", "palegoldenrod", "palegreen"), lwd=4, lty=c(1,1,1))

# d) Compute and print 4 random deviates from an exponential distribution with λ = 3.
rexp(4, rate=3)