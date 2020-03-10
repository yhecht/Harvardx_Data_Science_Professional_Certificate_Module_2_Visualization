# Yannique Hecht
# Harvardx: PH125.2 - (2) Data Science: Visualization
# SECTION 2: INTRO TO GGPLOT2
# CLASS NOTES

# # # SECTION 2.1: BASICS OF GGPLOT2

# # CREATING A NEW PLOT

library(tidyverse)
library(dslabs)
data(murders)
ggplot(data = murders)
murders %>% ggplot
p <- ggplot(data = murders)
class(p)
print(p)    



# # # SECTION 2.2: CUSTOMIZING PLOTS

# # LAYERS

# ADDING LAYERS TO A PLOT
library(tidyverse)
library(dslabs)
data(murders)
murders %>% ggplot() +
    geom_point(aes(x = population/10^6, y = total))
# add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))
# add text layer to scatterplot
p + geom_point(aes(population/10^6, total)) +
    geom_text(aes(population/10^6, total, label = abb))

# EXAMPLE OF AES BEHAVIOR
# no error from this call
p_test <- p + geom_text(aes(population/10^6, total, label = abb))
# error - "abb" is not a globally defined variable and cannot be found outside of aes
p_test <- p + geom_text(aes(population/10^6, total), label = abb)


# # TINKERING

# change the size of the points
p + geom_point(aes(population/10^6, total), size = 3) +
    geom_text(aes(population/10^6, total, label = abb))
# move text labels slightly to the right
p + geom_point(aes(population/10^6, total), size = 3) +
    geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)
# simplify code by adding global aesthetic
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) +
    geom_text(nudge_x = 1.5)
# local aesthetics override global aesthetics
p + geom_point(size = 3) +
    geom_text(aes(x = 10, y = 800, label = "Hello there!"))


# # SCALES, LABELS & COLORS

# LOG-SCALE THE X- & Y-AXIS
# define p
library(tidyverse)
library(dslabs)
data(murders)
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
# log base 10 scale the x-axis and y-axis
p + geom_point(size = 3) +
    geom_text(nudge_x = 0.05) +
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10")
# efficient log scaling of the axes
p + geom_point(size = 3) +
    geom_text(nudge_x = 0.075) +
    scale_x_log10() +
    scale_y_log10()

# ADD LABELS & TITLE
p + geom_point(size = 3) +
    geom_text(nudge_x = 0.075) +
    scale_x_log10() +
    scale_y_log10() +
    xlab("Population in millions (log scale)") +
    ylab("Total number of murders (log scale)") +
    ggtitle("US Gun Murders in 2010")

# CHANGE COLOR OF THE POINTS
# redefine p to be everything except the points layer
p <- murders %>%
    ggplot(aes(population/10^6, total, label = abb)) +
    geom_text(nudge_x = 0.075) +
    scale_x_log10() +
    scale_y_log10() +
    xlab("Population in millions (log scale)") +
    ylab("Total number of murders (log scale)") +
    ggtitle("US Gun Murders in 2010")
# make all points blue
p + geom_point(size = 3, color = "blue")
# color points by region
p + geom_point(aes(col = region), size = 3)

# ADD A LINE WITH AVERAGE RATE
# define average murder rate
r <- murders %>%
    summarize(rate = sum(total) / sum(population) * 10^6) %>%
    pull(rate)
# basic line with average murder rate for the country
p + geom_point(aes(col = region), size = 3) +
    geom_abline(intercept = log10(r))    # slope is default of 1
# change line to dashed and dark grey, line under points
p + 
    geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
    geom_point(aes(col = region), size = 3)

# CHANGE LEGEND TITLE
p <- p + scale_color_discrete(name = "Region")    # capitalize legend title


# ADD-ON PACKAGES

# ADDING THEMES
# theme used for graphs in the textbook and course
library(dslabs)
ds_theme_set()
# themes from ggthemes
library(ggthemes)
p + theme_economist()    # style of the Economist magazine
p + theme_fivethirtyeight()    # style of the FiveThirtyEight website

# PUTTING IT ALL TOGETHER TO ASSEMBLE THE PLOT
# load libraries
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)
# define the intercept
r <- murders %>%
    summarize(rate = sum(total) / sum(population) * 10^6) %>%
    .$rate
# make the plot, combining all elements
murders %>%
    ggplot(aes(population/10^6, total, label = abb)) +
    geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
    geom_point(aes(col = region), size = 3) +
    geom_text_repel() +
    scale_x_log10() +
    scale_y_log10() +
    xlab("Population in millions (log scale)") +
    ylab("Total number of murders (log scale)") +
    ggtitle("US Gun Murders in 2010") +
    scale_color_discrete(name = "Region") +
    theme_economist()


# OTHER EXAMPLES

# HISTOGRAMS IN GGPLOT2
# load heights data
library(tidyverse)
library(dslabs)
data(heights)
# define p
p <- heights %>%
    filter(sex == "Male") %>%
    ggplot(aes(x = height))
# basic histograms
p + geom_histogram()
p + geom_histogram(binwidth = 1)

# histogram with blue fill, black outline, labels and title
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
    xlab("Male heights in inches") +
    ggtitle("Histogram")

# SMOOTH DENSITY PLOTS IN GGPLOT2
p + geom_density()
p + geom_density(fill = "blue")

# QUANTILE-QUANTILE PLOTS IN GGPLOT2
# basic QQ-plot
p <- heights %>% filter(sex == "Male") %>%
    ggplot(aes(sample = height))
p + geom_qq()
# QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>%
    filter(sex == "Male") %>%
    summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
    geom_abline()
# QQ-plot of scaled data against the standard normal distribution
heights %>%
    ggplot(aes(sample = scale(height)) +
    geom_qq() +
    geom_abline()

# GRIDS OF PLOT WITH THE GRID.EXTRA PACKAGE
# define plots p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")
# arrange plots next to each other in 1 row, 3 columns
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)

