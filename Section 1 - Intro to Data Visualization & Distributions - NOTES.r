# Yannique Hecht
# Harvardx: PH125.2 - (2) Data Science: Visualization
# SECTION 1: INTRO TO DATA VISUALIZATION & DISTRIBUTIONS
# CLASS NOTES

# # # SECTION 1.1: INTRO TO DATA VISUALIZATION

# # INTRO TO DATA VISUALIZATION

library(dslabs)
data(murders)
head(murders)

# # INSTALLING R & RSTUDIO

# # INTRO TO DISTRIBUTION

# # DATA TYPES

# # INTRO TO DISTRIBUTION



# # # SECTION 1.2: INTRO TO DISTRIBUTIONS

# load the dataset
library(dslabs)
data(heights)

# make a table of category proportions
prop.table(table(heights$sex))


# # SMOOTH DENSITY PLOTS


# # NORMAL DISTRIBUTION

# define x as vector of male heights
library(tidyverse)
library(dslabs)
data(heights)
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



# # # SECTION 1.3: QUANTILES, PERCENTILES & BOXPLOTS

# # QUANTILE-QUANTILE PLOTS

# define x and z
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)
# proportion of data below 69.5
mean(x <= 69.5)
# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))
# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p) 
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)


# # PERCENTILES


# # BOXPLOTS



# # # SECTION 1.4: EXPLORATORY DATA ANALYSIS

# # DISTRIBUTION


