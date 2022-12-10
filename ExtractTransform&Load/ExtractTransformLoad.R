# Aakanksha Arora
# The following code reads a csv, calculates a few additional columns,
# displays the statistics and summaries of the numerical columns and displays 
# the result in a histogram and box-plot.
# Install tidyverse package
install.packages("tidyverse")
# Load tidyverse
library(tidyverse)
# Set the working directory to the right location
setwd("C:/Users/ual-laptop/Documents")
# Read the csv file into a tibble
groceryTranscations1<-read_csv(file = "GroceryTransactions.csv", 
                               col_types = "iDffffifffffffin")
# Display the tibble
glimpse(groceryTranscations1)
# Display the first 20 rows of the tibble
head(groceryTranscations1,20)
# Display the structure of the tibble
str(groceryTranscations1)
# Display the summary of the tibble
summary(groceryTranscations1)
# Load dplyr package
library(dplyr)
# Mean of revenue
print(summarize(.data = groceryTranscations1, mean(Revenue)))
# Median of units sold
print(summarize(.data = groceryTranscations1, median(UnitsSold)))
# Standard deviation of revenue
print(summarize(.data = groceryTranscations1, sd(Revenue)))
# Inter-quartile range of units sold
print(summarize(.data = groceryTranscations1, IQR(UnitsSold)))
# Minimum of revenue
print(summarize(.data = groceryTranscations1, min(Revenue)))
# Maximum of children
print(summarize(.data = groceryTranscations1, max(Children)))
# Sub-setting features into a new tibble 
groceryTransactions2 <- select(.data = groceryTranscations1,
                               PurchaseDate,
                               Homeowner,
                               Children,
                               AnnualIncome,
                               UnitsSold,
                               Revenue)
# Display all of the features for transactions made by non-homeowners 
# with atleast 4 children
print(filter(.data = groceryTransactions2,
             Homeowner == "N" & Children >= 4))
# Display all of the records and features in groceryTransactions2 
# that were either made by customers in the $150K + annual income 
# category OR had more than 6 units sold. 
print(filter(.data = groceryTransactions2,
             AnnualIncome == "$150K+" | UnitsSold > 6))
# Display the average transaction revenue grouped by annual income level. 
# Sort the results by average transaction revenue from largest to smallest.
print(groceryTransactions2 %>%
        group_by(AnnualIncome)%>%
        summarize(AverageTransactionRevenue= mean(Revenue)) %>%
        arrange(desc(AverageTransactionRevenue)),
)
# Create a new tibble called groceryTransactions3 that contains all 
# of the features in groceryTransactions2 along with a new calculated 
# feature called AveragePricePerUnit calculated by dividing revenue by 
# units sold
groceryTransactions3 <- groceryTransactions2 %>%
  mutate(AveragePricePerUnit = Revenue / UnitsSold)
# Display the groceryTransactions3 tibble on the console
print(groceryTransactions3)
# Load ggplot2 library
library(ggplot2)
# Use ggplot() to create a histogram of AveragePricePerUnit with a 
# bin width of 1, a bin outline of black, a bin fill of orange, 
# and a bin transparency of 50%
histogramAvgPricePerUnit <- ggplot(data = groceryTransactions3,
                                   aes(x = AveragePricePerUnit))
histogramAvgPricePerUnit + geom_histogram(binwidth = 1,
                                          color = "black",
                                          fill = "orange",
                                          alpha = 0.5)+
  ggtitle("AveragePricePerUnit")
# Use ggplot() to create a boxplot of revenue with an outline color of 
# Arizona Blue (#0C234B) and a fill color of Arizona Red (#AB0520)
boxplotRevenue <- ggplot(data = groceryTransactions3,
                         aes(x = Revenue))
boxplotRevenue + geom_boxplot(color = "#0C234B",
                              fill = "#AB0520")