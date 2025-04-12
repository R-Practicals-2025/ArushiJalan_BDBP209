# 9) Gamma distribution: The function suffix in R is gamma. There are two parameters α
# (shape option) and θ(scale option).

# a) Plot the PDFs on the same graph with alpha values of 1,2,3,4 and θ value 4 with
# colors black, blue, red and magenta respectively. This is one of the two graphs on
# a 1x2 grid. Plot the second graph with θ values of 1,2,3,4 and α = 4 with colors
# black, blue, red and magenta respectively.

par(mfrow=c(1,2))
x_vals <- seq(0, 50, length=200)

# First plot: Different alpha values (1,2,3,4) with theta = 4
pdf_1 <- dgamma(x_vals, shape=1, scale=4)
pdf_2 <- dgamma(x_vals, shape=2, scale=4)
pdf_3 <- dgamma(x_vals, shape=3, scale=4)
pdf_4 <- dgamma(x_vals, shape=4, scale=4)

# First plot
plot(x_vals, pdf_1, type="l", col="black", lwd=2, main="Gamma PDFs (Varying α, θ=4)", xlab="X", ylab="Density")
lines(x_vals, pdf_2, col="blue", lwd=2)
lines(x_vals, pdf_3, col="red", lwd=2)
lines(x_vals, pdf_4, col="magenta", lwd=2)
legend("topright", legend=c("α=1", "α=2", "α=3", "α=4"), col=c("black", "blue", "red", "magenta"), lwd=2)

# Second plot: Different θ values (1,2,3,4) with α = 4
pdf_theta_1 <- dgamma(x_vals, shape=4, scale=1)
pdf_theta_2 <- dgamma(x_vals, shape=4, scale=2)
pdf_theta_3 <- dgamma(x_vals, shape=4, scale=3)
pdf_theta_4 <- dgamma(x_vals, shape=4, scale=4)

# Second plot
plot(x_vals, pdf_theta_1, type="l", col="black", lwd=2, main="Gamma PDFs (Varying θ, α=4)", xlab="X", ylab="Density")
lines(x_vals, pdf_theta_2, col="blue", lwd=2)
lines(x_vals, pdf_theta_3, col="red", lwd=2)
lines(x_vals, pdf_theta_4, col="magenta", lwd=2)
legend("topright", legend=c("θ=1", "θ=2", "θ=3", "θ=4"), col=c("black", "blue", "red", "magenta"), lwd=2)

# b) Compute and print the probability density corresponding to x = 6, α = 4 and
# θ = 1.
dgamma(6, shape=4, scale=1)

# c) Compute and print the cumulative probability up to x=6 for the above gamma PDF.
pgamma(6, shape=4, scale=1)

# d) Compute the x value which corresponds to a cumulative probability of 0.95
qgamma(0.95, shape=4, scale=1)

# e) Obtain 10,000 random deviates from the above gamma distribution and plot a
# histogram of this set.
gamma_samples <- rgamma(10000, shape=4, scale=1)
hist(gamma_samples, breaks=50, probability=TRUE, col="thistle1",
     main="Histogram of Gamma Distribution (α=4, θ=1)", xlab="X")
curve(dgamma(x, shape=4, scale=1), col="purple", lwd=2, add=TRUE)


# 10) Chi-square(χ^2 distribution: The suffix for the functions in R for this distribution is
# chisq. The key parameter is the degrees of freedom, r.

# a) Plot the χ^2 distribution with degree of freedom 2,3,5 and 10.
# Define x values
x_vals <- seq(0, 30, length=200)

# Compute the PDFs for different degrees of freedom
pdf_2  <- dchisq(x_vals, df=2)
pdf_3  <- dchisq(x_vals, df=3)
pdf_5  <- dchisq(x_vals, df=5)
pdf_10 <- dchisq(x_vals, df=10)

# Plot the curves
plot(x_vals, pdf_2, type="l", col="plum1", lwd=2, 
     main="Chi-Square PDFs", xlab="X", ylab="Density")
lines(x_vals, pdf_3, col="lightblue1", lwd=2)
lines(x_vals, pdf_5, col="khaki", lwd=2)
lines(x_vals, pdf_10, col="lavender", lwd=2)
legend("topright", legend=c("df=2", "df=3", "df=5", "df=10"), 
       col=c("plum1", "lightblue1", "khaki", "lavender"), lwd=2)

# b) Compute and print the probability density for x=6 and 5 degrees of freedom
dchisq(6, df=5)

# c) Compute and print the cumulative probability up to x=6 and 10 degrees of freedom
pchisq(6, df=10)

# d) Obtain the 85th quantile for this distribution for 6 degrees of freedom
qchisq(0.85, df=6)

# e) Plot a histogram of 10,000 random deviates from this distribution with 6 degrees of freedom with
# 30 bins, red filled bars and text within the plot ”r=6” in an appropriate blank portion
chi_samples <- rchisq(10000, df=6)
hist(chi_samples, breaks=30, probability=TRUE, col="mistyrose1",
     main="Histogram of Chi-Square (df=6)", xlab="X")
curve(dchisq(x, df=6), col="lightseagreen", lwd=2, add=TRUE)
text(10, 0.05, "r = 6", col="black", cex=1.2)

# f) Assuming μ = 2 and σ = 1 compute the variable Z^2 = (x − μ)^2/σ^2 and 
# plot the χ^2 PDF with 1 degree of freedom.
mu <- 2
sigma <- 1
x_vals <- seq(-5, 10, length=200)
z_squared <- ((x_vals - mu)^2) / sigma^2
pdf_chisq_1 <- dchisq(z_squared, df=1)
plot(z_squared, pdf_chisq_1, type="l", col="palevioletred1", lwd=2, main="Chi-Square PDF (df=1)", xlab="Z^2", ylab="Density")



# III. Central Limit Theorem

# (1) CLT case with sampling from non-normal distribution: Here we will demonstrate
# the central limit theorem by taking samples from a non-normal distribution such as the
# uniform distribution. Follow the steps below.

# (i) Generate a sample set of 5 random deviates from a uniform distribution in the
# interval [0,10]. Repeat this 10,000 times. We now have 10,000 samples of 5
# numbers each.
set.seed(42)
samples <- replicate(10000, runif(5, min=0, max=10))
dim(samples)

# (ii) Calculate the mean for each of the 10,000 samples and store it in a separate array.
# Plot this distribution of means as a histogram. Print the mean and standard
# deviation of the distribution of means.
sample_means <- colMeans(samples)
hist(sample_means, breaks=50, probability=TRUE, col="lavender", main="Histogram of Sample Means", xlab="Sample Mean")
mean_means <- mean(sample_means)
sd_means <- sd(sample_means)
print(paste("Mean of Sample Means:", round(mean_means, 3)))
print(paste("Standard Deviation of Sample Means:", round(sd_means, 3)))

# (iii) Generate a sequence of numbers between 0 and 10 with 0.1 spacing. Obtain the
# normal probability densities using dnorm function with the calculated mean and
# standard deviation in the last question and store it as a separate vector.
# Create a sequence of x values from 0 to 10 with 0.1 spacing
x_vals <- seq(0, 10, by=0.1)
normal_pdf <- dnorm(x_vals, mean=mean_means, sd=sd_means)

# (iv) Since we have 10,000 samples, we have to scale the normal probability density
# function to our particular case (since the area is 1.0 otherwise). The height and
# bin width of a histogram are related to each other – if we doubled the bin width,
# there would be roughly twice as many numbers in the bin and the bar would be
# twice as high on the y-axis. So to get the height of the bars on our frequency scale,
# we multiply the total frequency, i.e., 10,000 by the bin width 0.5 to get 5000. This
# is the scaling factor for the PDF. Perform this and save the scaled probabilities
# as a separate array.
# Scale the normal PDF to match the histogram
scaling_factor <- 10000 * 0.5
scaled_pdf <- normal_pdf * scaling_factor

# (v) Plot the normal curve on top of the histogram to see the level of agreement
# between the normal behaviour of the sample means and the normal curve.
hist(sample_means, breaks=50, probability=TRUE, col="lavender", main="CLT: Sample Mean Distribution vs. Normal Curve",
     xlab="Sample Mean", ylim=c(0, max(scaled_pdf) * 1.1))
lines(x_vals, scaled_pdf, col="mediumpurple1", lwd=2)
legend("topright", legend=c("Histogram", "Normal PDF Fit"), col=c("lavender", "mediumpurple1"), lwd=2, bty="n")



# (2) CLT demo with sampling from non-normal, non-functional distribution: Here
# we demonstrate CLT by taking samples not from an intrinsic definition of the uniform
# distribution. Here we will create samples of dice throwing exercise.

# (i) Create 10,000 samples of single dice throw using the sample() function:
#   a<- sample(1:6,replace=T,10000)
# Make a plot of this distribution. You should see a uniform distribution.
set.seed(42) 
a <- sample(1:6, size=10000, replace=TRUE)
hist(a, breaks=seq(0.5,6.5,1), col="plum", probability=TRUE,
     main="Histogram of Single Die Rolls", xlab="Die Roll Outcome", xlim=c(0,8), ylim=c(0,0.20))

# (ii) Throw two dice and add the scores together (this is the ancient game of craps).
# Generate a new object b similar to the above. Plot a histogram of a+b. You
# should see a triangular shape developing for the histogram.
b <- sample(1:6, size=10000, replace=TRUE) + sample(1:6, size=10000, replace=TRUE)
hist(b, breaks=seq(1.5,12.5,1), col="steelblue", probability=TRUE,
     main="Histogram of Sum of Two Dice Rolls", xlab="Sum of Two Dice")

# (iii) Repeat the exercise with three dice. The histogram should start showing the
# distinct bell shape.
c <- sample(1:6, size=10000, replace=TRUE) + 
  sample(1:6, size=10000, replace=TRUE) + 
  sample(1:6, size=10000, replace=TRUE)
hist(c, breaks=seq(2.5,18.5,1), col="hotpink1", probability=TRUE,
     main="Histogram of Sum of Three Dice Rolls", xlab="Sum of Three Dice")

# (iv) Repeat this with five dice. The histogram is now very close to a normal curve.
# Use the mean and standard deviation of the 5 dice to generate a normal PDF. As
# in the last problem, one has to scale the PDF to match the height of the normal
# curve with the height of the histogram.
d <- sample(1:6, size=10000, replace=TRUE) + 
  sample(1:6, size=10000, replace=TRUE) + 
  sample(1:6, size=10000, replace=TRUE) + 
  sample(1:6, size=10000, replace=TRUE) + 
  sample(1:6, size=10000, replace=TRUE)
mean_d <- mean(d)
sd_d <- sd(d)
x_vals <- seq(min(d), max(d), length=100)
normal_pdf <- dnorm(x_vals, mean=mean_d, sd=sd_d)
scaling_factor <- 10000 * (1 / 5)  
scaled_pdf <- normal_pdf * scaling_factor
hist(d, breaks=seq(4.5,30.5,1), col="navajowhite2", probability=TRUE,
     main="CLT: Sum of Five Dice Rolls vs. Normal Distribution", xlab="Sum of Five Dice")
lines(x_vals, scaled_pdf, col="coral4", lwd=2)
legend("topright", legend=c("Histogram", "Normal PDF Fit"),
       col=c("navajowhite2", "coral4"), lwd=2, bty="n")



# IV. ROC curve

# Here we will learn to use the pROC library of R to perform ROC analysis. The function to
# be used from this library is plot.roc(). There are two datasets pertaining to red wine and
# white wine samples from the north of Portugal. The goal is to model wine quality based on
# physicochemical tests.
# There are 11 input variables (see column names) and the output variable is the quality score in
# scale of 0 to 10 from wine tasting. We want to test various thresholds of the quality score to
# achieve the best possible binary classifier of good and bad quality wine.
# Try the following:
# 
# (1) Read in the white wine data into a dataframe, and create additional columns that classifies
# the data as good or bad wine based on threshold quality scores of 6, 7, 8, 9 and 10.
# 
# (2) Use plot.roc() function as follows to plot the ROC curves for each threshold value.
# Which threshold value brings the ROC curve to the perfect classifier?
#   plot.roc(data$winequality,data$alcohol, #data vectors
#            main="title", #main plot title
#            legacy.axes=TRUE, # plots sensitivity against (1-specificity)
#            ci=TRUE, # plot confidence interval of AUC
#            print.auc=TRUE, # print values of AUC
#            identity.lwd=2, # linewidth of the 45 deg line (worst classifier)
#            print.thres=TRUE, # print best threshold on the graph)

library(pROC)

data <- read.csv("/home/ibab/Downloads/winequality-white.csv", sep=";")

# Define threshold values
thresholds <- c(6, 7, 8, 9, 10)

# Set up a color palette for different thresholds
colors <- rainbow(length(thresholds))

# Initialize an empty plot
plot(1, type="n", xlab="1 - Specificity", ylab="Sensitivity", 
     xlim=c(0,1), ylim=c(0,1), main="ROC Curves for Different Thresholds")

# Loop through each threshold and plot the ROC curve
for (i in seq_along(thresholds)) {
  t <- thresholds[i]
  data$quality_binary <- ifelse(data$quality >= t, 1, 0)
  roc_obj <- roc(data$quality_binary, data$alcohol)
  lines(roc_obj, col=colors[i], lwd=2)
}

# Add a legend
legend("bottomright", legend=paste("Threshold", thresholds), col=colors, lwd=2)
