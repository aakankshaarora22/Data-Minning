# Aakanksha Arora
# The following code reads a csv file, imputes the missing data, determines 
# outliers in the feature, discretize and normalize feature. Dummy code the 
# Position feature and stores the result into a new tibble. The results are 
# displayed in a scatter-plot. 
# Install tidyverse package
# install.packages("tidyverse")
# Load tidyverse library
library(tidyverse)
# Install dummies packages
# install.packages("dummies", repos = NULL, type="source")
# Load dummies library
library(dummies)
# set the working directory
setwd("C:/Users/ual-laptop/Documents")
# Read TireTread.csv
tireTread1 <- read_csv(file = "TireTread.csv", 
                       col_types = "cfnni",
                       col_names = TRUE)
# Display tireTread1
print(tireTread1)
# Display the structure of tireTread1
print(str(tireTread1))
# Display the summary of tireTread1
print(summary(tireTread1))
# Impute missing data for UsageMonths with the mean method 
# Store the result into a new tibble called tireTread2
tireTread2 <- tireTread1 %>% 
  mutate(UsageMonths = ifelse(is.na(UsageMonths),
                              mean(UsageMonths, na.rm = TRUE),UsageMonths))
# Summary of tireTread2
print(summary(tireTread2))
# Determine outliers in the TreadDepth feature.
# calculate outlier min and max and store into variables 
# called outlierMin and outlierMax
outlierMin <- quantile(tireTread2$TreadDepth, .25) -
  (IQR(tireTread2$TreadDepth) * 1.5)
outlierMax <- quantile(tireTread2$TreadDepth, .75) + 
  (IQR(tireTread2$TreadDepth) * 1.5)
# Keep the outliers in the dataset, but add the outliers 
# to their own tibble called treadDepthOutliers
treadDepthOutliers <- tireTread2 %>%
  filter(TreadDepth < outlierMin | TreadDepth > outlierMax)
# Normalize the UsageMonths feature by taking the log of 
# UsageMonths into a new feature called LogUsageMonths and 
# store the additional column in a tibble called tireTread3
tireTread3 <- tireTread2 %>%
  mutate(LogUsageMonths = log(UsageMonths))
# Discretize TreadDepth into NeedsReplacing and store into new 
# tireTread4 tibble
tireTread4 <- tireTread3 %>%
  mutate(NeedsReplacing = TreadDepth <= 1.6)
# Convert tireTread4 into a data frame
tireTread4DataFrame <- data.frame(tireTread4)
# Dummy code the Position (LF, RF, LR, RR) feature, 
# convert it back into a tibble, and store the result into 
# a new tireTread5 tibble
tireTread5 <- as_tibble(dummy.data.frame(data = tireTread4DataFrame,
                                         names = "Position"))
print(tireTread5)
# Load ggplot2
library(ggplot2)
# Use ggplot() to build a scatter plot of Miles (x) with TreadDepth (y)
scatterPlotTreadDepthMiles <- ggplot(data = tireTread5,
                                     aes(x = Miles,
                                         y = TreadDepth))
# Add geometry layer
scatterPlotTreadDepthMiles + geom_point(color = "Dark Gray")
# Add a linear best fit line to the plot and color it red. 
# Add a title to the scatter plot, "Tire Miles and Tread Depth Scatter Plot."
scatterPlotTreadDepthMiles + geom_point(color = "Dark Gray") + 
  geom_smooth(method = lm,
              level = 0,
              color = "Red") +
  ggtitle("Tire Miles and Tread Depth Scatter Plot")
