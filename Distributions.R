# Understanding distributions, quantiles, percentiles and theoratical_quantiles
data(heights)
head(heights)
a <- seq(min(heights$height), max(heights$height), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(heights$height<= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)
index <- heights$sex=="Male"
x <- heights$height[index]

# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum((x - average)^2)/length(x))

# built-in mean and sd functions - note that the audio and printed values disagree
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)

# calculate standard units
z <- scale(x)

# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
x <- heights %>% filter(sex=="Male") %>% pull(height)
1 - pnorm(70.5, mean(x), sd(x))
library(dslabs)
data(heights)
summary(heights$height)
p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3) # where 69 is the eman and 3 is the standard deviation

