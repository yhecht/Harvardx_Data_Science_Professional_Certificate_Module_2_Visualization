# Yannique Hecht
# Harvardx: PH125.2 - (2) Data Science: Visualization
# SECTION 3: SUMMARIZING WITH DPLYR
# CLASS NOTES

# # # SECTION 3.1: SUMMARIZING WITH DPLYR

# # DPLYR

library(tidyverse)
library(dslabs)
data(heights)

# compute average and standard deviation for males
s <- heights %>%
    filter(sex == "Male") %>%
    summarize(average = mean(height), standard_deviation = sd(height))

# access average and standard deviation from summary table
s$average
s$standard_deviation

# compute median, min and max
heights %>%
    filter(sex == "Male") %>%
    summarize(median = median(height),
                       minimum = min(height),
                       maximum = max(height))

# alternative way to get min, median, max in base R
quantile(heights$height, c(0, 0.5, 1))

# generates an error: summarize can only take functions that return a single value
heights %>%
    filter(sex == "Male") %>%
    summarize(range = quantile(height, c(0, 0.5, 1)))


# # THE DOT PLACEHOLDER
library(tidyverse)
library(dslabs)
data(murders)

murders <- murders %>% mutate(murder_rate = total/population*100000)
summarize(murders, mean(murder_rate))

# calculate US murder rate, generating a data frame
us_murder_rate <- murders %>%
    summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate

# extract the numeric US murder rate with the dot operator
us_murder_rate %>% .$rate

# calculate and extract the murder rate with one pipe
us_murder_rate <- murders %>%
    summarize(rate = sum(total) / sum(population * 100000)) %>%
    .$rate


# # GROUP BY

# libraries and data
library(tidyverse)
library(dslabs)
data(heights)
data(murders)

# compute separate average and standard deviation for male/female heights
heights %>%
    group_by(sex) %>%
    summarize(average = mean(height), standard_deviation = sd(height))

# compute median murder rate in 4 regions of country
murders <- murders %>%
    mutate(murder_rate = total/population * 100000)
murders %>%
    group_by(region) %>%
    summarize(median_rate = median(murder_rate))

# # SORTING DATA TABLES

# libraries and data
library(tidyverse)
library(dslabs)
data(murders)

# set up murders object
murders <- murders %>%
    mutate(murder_rate = total/population * 100000)

# arrange by population column, smallest to largest
murders %>% arrange(population) %>% head()

# arrange by murder rate, smallest to largest
murders %>% arrange(murder_rate) %>% head()

# arrange by murder rate in descending order
murders %>% arrange(desc(murder_rate)) %>% head()

# arrange by region alphabetically, then by murder rate within each region
murders %>% arrange(region, murder_rate) %>% head()

# show the top 10 states with highest murder rate, not ordered by rate
murders %>% top_n(10, murder_rate)

# show the top 10 states with highest murder rate, ordered by rate
murders %>% arrange(desc(murder_rate)) %>% top_n(10)