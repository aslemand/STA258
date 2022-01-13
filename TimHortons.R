# Import the data file TimHortons in R
# name the data frame as "tims"
# note that R is case sensitive

# attach the data so that you can call the variable names in it
attach(tims)

# See variable names in the data file
names(tims)

## Display and Describe the Variable Sugar
# Call the variable Calories
Calories

# Display Calories in order
sort(Calories)

# Obtain min and max Calories
min(Calories)
max(Calories)

# Plot Histogram of Calories
# Use the argument col to change the color of histogram
# Default color is gray
hist(Calories, col = "bisque2")

# Retrieve arguments from hist() function
names(hist(Calories, col = "bisque2"))

# Count the number of observations in each bin
# use the $ sign to extract the counts info
hist(Calories, col = "bisque2")$counts

# Use the argument labels to show each bin count on histogram
hist(Calories, col = "bisque2", labels = TRUE)

# Find and count the number of donuts with at most 200 calories
# Indexing with a boolean vector to select a subset of data
Calories[Calories <= 200]
# The length function will count the number of observations 
# in the selected subset
length(Calories[Calories <= 200])

# Find and count the number of donuts with more than 300 calories
Calories[Calories > 300]
length(Calories[Calories > 300])

# Install ggplot2 package, part of tidyverse package
# ggplot2: grammar of graphics 
install.packages("ggplot2")
# Load the library ggplot2 to use its functions
library(ggplot2)

# Plot the histogram of calories with ggplot2
# aes: aesthetic mappings 
hist.plot = ggplot(data = tims, aes(x = Calories)) 
hist.plot = hist.plot + geom_histogram(color = "orange",
                                       fill = "bisque2", 
                                       binwidth = 30)
# Centre the title 
theme_update(plot.title = element_text(hjust = 0.5))
hist.plot = hist.plot+ggtitle("Histogram of Calories")
hist.plot 

# Obtain Histogram of Calories
hist(Calories, col = "bisque2")
# Obtain Density Plot of Calories
plot(density(Calories), main = "Density Plot of Calories")

# Construct a Multi panel, 1 rows and 2 columns
par(mfrow = c(1, 2))
# Big bandwidth
plot(density(Calories, bw = 40), lwd = 2,
     col = "red", main = "Too big bandwidth")
# Small bandwidth
plot(density(Calories, bw = 5), lwd = 2,
     col = "red", main = "Too small bandwidth")

# Relative Frequency Histogram with ggplot
hist.plot = ggplot(tims, aes(x = Calories)) 
hist.plot = hist.plot + geom_histogram(color = "orange",
                                       fill = "bisque2", 
                                       binwidth = 30, 
                                       aes(y = ..density..))
# Centre the title 
theme_update(plot.title = element_text(hjust = 0.5))
hist.plot = hist.plot+ggtitle("Relative Frequency Histogram of Calories")
hist.plot 

# Relative Frequency Histogram with ggplot
hist.plot = ggplot(tims, aes(x = Calories)) 
hist.plot = hist.plot + geom_histogram(color = "orange",
                                       fill = "bisque2", 
                                       binwidth = 30, 
                                       aes(y = ..density..))
# Centre the title 
theme_update(plot.title = element_text(hjust = 0.5))
hist.plot = hist.plot+ggtitle("Relative Frequency Histogram of Calories")
hist.plot 

# Density Plot with ggplot2
hist.plot = ggplot(tims, aes(x = Calories)) 
hist.plot = hist.plot + geom_density(color = "coral1", # Curve color
                                     fill = "coral1",  # Area color
                                     alpha = 0.7)   # Area transparency
# Centre the title 
theme_update(plot.title = element_text(hjust = 0.5))
hist.plot = hist.plot+ggtitle("Density Plot of Calories")
hist.plot 

# Add Normal Curve 
hist.plot = ggplot(tims, aes(x = Calories)) 
hist.plot = hist.plot + geom_density(color = "coral1", # Curve color
                                     fill = "coral1",  # Area color
                                     alpha = 0.7)   # Area transparency
# Centre the title 
theme_update(plot.title = element_text(hjust = 0.5))
hist.plot = hist.plot+ggtitle("Density Plot of Calories")
hist.plot = hist.plot + stat_function(fun = dnorm,
                                      args = list(mean = mean(Calories), sd = sd(Calories)),
                                      col = "blue",
                                      size = 1)
hist.plot

# Obtain mean Calories
mean(Calories)

# Obtain median Calories
median(Calories)

# Obtain mode Calories
# Obtain a frequency distribution table of Calories
table(Calories)

# Index through the table to obtain the mode value
table(Calories)[table(Calories) == max(table(Calories))]

# Obtain range of Calories
range(Calories)

# Calculate range of Calories
min.cal = min(Calories)
max.cal = max(Calories)
range.cal = max.cal - min.cal
range.cal

# Obtain Variance of Calories
var(Calories)

# Obtain Standard Deviation of Calories
sd(Calories)

sd(Calories)^2

# Obtain Boxplot of Calories
boxplot(Calories, 
        main = "Boxplot of Calories in Tim Hortons Donuts", 
        xlab = "Calories",
        col = "bisque2", 
        horizontal = TRUE)

# Using the library ggplot2 (inside tidyverse package)
# to create a boxplot of Calories
# ggplot is "grammar of graphics" 
library(ggplot2)
bp<-ggplot(tims, aes(x = Calories))
bp
bp<-bp+geom_boxplot(color = "brown", fill = "coral1")
bp
# Add a title to your plot
bp + labs(title = "Boxplot of Calories")
# Title of the plot will be centered
theme_update(plot.title = element_text(hjust = 0.5))
bp + labs(title = "Boxplot of Calories")

# Construct a normal probability plot of Calories
# Use the lenght function to obtain the size of data and store in n
n <- length(Calories)
n
# Find Corresponding Probability Values
prob.levels <- (seq(1:n)-0.5)/n
prob.levels
# Find Standard Normal Quantiles
norm.quantiles <- qnorm(prob.levels)
norm.quantiles
# Construct QQ plot
# Be sure to order the observations for calories, using sort function
plot(norm.quantiles, sort(Calories), 
     xlab = "Theoratical Quantiles", 
     ylab = "Sample Quantiles", 
     main = "Our Manually Constructed QQ Plot", 
     col = "blue", pch = 19)
qqline(Calories)

# Construct a normal probability plot of Calories, using R function
qqnorm(Calories, 
       main = "Normal QQ Plot of Calories", 
       col = "blue", 
       pch = 19)
qqline(Calories)

# Using the library ggplot2 (inside tidyverse package)
# to create a QQ Plot of Calories
# ggplot is "grammar of graphics" 
library(ggplot2)
qq.plot = ggplot(tims, aes(sample = Calories))
qq.plot = qq.plot + geom_qq()
qq.plot = qq.plot + geom_qq_line()
qq.plot

# Store Mean and SD in value name in R

# Mean Calories
mean.calories = mean(Calories)

# SD Calories
SD.calories = sd(Calories)

# Check Whether the Empirical Rule Holds
# Find the lower bound of the interval: mean - 1 SD
One.SD.below.Mean = mean.calories - SD.calories
One.SD.below.Mean
# Find the upper bound of the interval: mean + 1 SD
One.SD.above.Mean = mean.calories + SD.calories
One.SD.above.Mean
# Find the count of donuts within 1 SD of the mean calories
count.Donuts.Within.1.SD.of.Mean = length(Calories[(Calories > One.SD.below.Mean) & (Calories < One.SD.above.Mean)])
count.Donuts.Within.1.SD.of.Mean
# Find the proportion of donuts within 1 SD of the mean
Prop.Donuts.Within.1.SD.of.Mean = count.Donuts.Within.1.SD.of.Mean / length(Calories)
Prop.Donuts.Within.1.SD.of.Mean
# Find the percent of donuts within 1 SD of the mean
Prop.Donuts.Within.1.SD.of.Mean * 100

# Plot the histogram of Calories
# Indicate the interval: number of donuts within 1 SD of the mean
hist(Calories, col = "bisque2")
# Add 2 lines using abline function and "v" as vertical line argument
# Make line colors "red" and indicate type as dashed "2" and for width as "3"
abline(v =c(One.SD.below.Mean, One.SD.above.Mean), col = "red", lty=c(2,2), lwd=c(3, 3))

# Find the lower bound of the interval: mean - 2 SD
Two.SD.below.Mean = mean.calories - (2 * SD.calories)
Two.SD.below.Mean
# Find the upper bound of the interval: mean + 2 SD
Two.SD.above.Mean = mean.calories + (2 *SD.calories)
Two.SD.above.Mean
# Find the count of donuts within 2 SD of the mean calories
count.Donuts.Within.2.SD.of.Mean = length(Calories[(Calories > Two.SD.below.Mean) & (Calories < Two.SD.above.Mean)])
count.Donuts.Within.2.SD.of.Mean
# Find the proportion of donuts within 2 SD of the mean
Prop.Donuts.Within.2.SD.of.Mean = count.Donuts.Within.2.SD.of.Mean / length(Calories)
Prop.Donuts.Within.2.SD.of.Mean
# Find the percent of donuts within 2 SD of the mean
Prop.Donuts.Within.2.SD.of.Mean * 100

# Plot the histogram of Calories
# Indicate the interval: number of donuts within 2 SD of the mean
hist(Calories, col = "bisque2")
# Add 2 lines using abline function and "v" as vertical line argument
# Make line colors "red" and indicate type as dashed "2" and for width as "3"
abline(v =c(Two.SD.below.Mean, Two.SD.above.Mean), col = "red", lty=c(2,2), lwd=c(3, 3))

# Find the lower bound of the interval: mean - 3 SD
Three.SD.below.Mean = mean.calories - (3 * SD.calories)
Three.SD.below.Mean
# Find the upper bound of the interval: mean + 3 SD
Three.SD.above.Mean = mean.calories + (3 *SD.calories)
Three.SD.above.Mean
# Find the count of donuts within 3 SD of the mean calories
count.Donuts.Within.3.SD.of.Mean = length(Calories[(Calories > Three.SD.below.Mean) & (Calories < Three.SD.above.Mean)])
count.Donuts.Within.3.SD.of.Mean
# Find the proportion of donuts within 3 SD of the mean
Prop.Donuts.Within.3.SD.of.Mean = count.Donuts.Within.3.SD.of.Mean / length(Calories)
Prop.Donuts.Within.3.SD.of.Mean
# Find the percent of donuts within 3 SD of the mean
Prop.Donuts.Within.3.SD.of.Mean * 100

# Plot the histogram of Calories
# Indicate the interval: number of donuts within 3 SD of the mean
hist(Calories, col = "bisque2")
# Add 2 lines using abline function and "v" as vertical line argument
# Make line colors "red" and indicate type as dashed "2" and for width as "3"
abline(v =c(Three.SD.below.Mean, Three.SD.above.Mean), col = "red", lty=c(2,2), lwd=c(3, 3))

## More exploration to try on your own, 
# including some review from former stats course(s)

# Obtain z-Scores of Calories
# R function to obtain z-Scores is scale()
# Store the z-Scores in a new variable.
z.scores.calories<-scale(Calories)
# let's see the sorted values of z-scores of Calories
sort(z.scores.calories)

# Obtain summary statistics of z-scores of Calories

# Install the "mosaic" package (install once only)
# to obtain summary statistics in a neat way
install.packages("mosaic")

# Load the library mosaic 
# to use the functions inside this package
library(mosaic)

# Obtain summary statistics of z-scores of Calories
favstats(z.scores.calories)

# let's display the boxplot of z.scores of Calories
# As well as the boxplot of the original data: Calories
par(mfrow=c(1,2))
boxplot(z.scores.calories, main = "Boxplot of Z-Scores of Calories", ylab = "Z-Scores of Calories")
boxplot(Calories,main = "Boxplot of Calories", ylab = "Calories")

# Describe the variable "Type of Donuts"
table(`Type of Donut`)

# Bar plot of Type of Donuts
barplot(table(`Type of Donut`), col = rainbow(4))

# Turn off panel plot
dev.off()

# Bar plot of Type of Donuts
barplot(table(`Type of Donut`), col = rainbow(4))

# Obtain summary statistics for each type of donuts
favstats(Calories ~ `Type of Donut`)

# Obtain side-by-side boxplots of calories by type of donuts
# bp: just a name (not a code) to store boxplots
# We are using the function ggplot in ggplot2 packages included tidyverse package
bp <- ggplot(tims, aes(x = `Type of Donut`, 
                       y = Calories, 
                       fill = `Type of Donut`))
bp
bp + geom_boxplot()
