# Aakanksha Arora
# The following code reads a csv file, displays the histogram for all features,
# displays a correlation matrix and a correlation plot, generates
# a linear regression model, displays the beta coefficients and tests the
# multicollinearity.
# Install the tidyverse,corrplot, and olsrr packages
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("olsrr")
# Load the tidyverse,corrplot, and olsrr packages
library(tidyverse)
library(corrplot)
library(olsrr)
# Set the working directory
setwd("C:\\Users\\ual-laptop\\Documents\\Lab05")
# Read ZooVisitSpending.csv into a tibble called zooSpending
zooSpending <- read_csv(file = "ZooVisitSpending.csv",
                        col_types = "niil",
                        col_names = TRUE)
# Display zooSpending in the console
print(zooSpending)
# Display the structure of zooSpending in the console
print(str(zooSpending))
# Display the summary of zooSpending in the console
print(summary(zooSpending))
# Create a function called displayAllHistograms that takes in a tibble
# parameter that will display a histogram for all numeric features in
# the tibble
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x = value, fill = key),
                              color = "black") +
    facet_wrap(~key, scales = "free")+
    theme_minimal()
}
# Call the displayAllHistograms() function, passing in zooSpending as argument
displayAllHistograms(zooSpending)
# Display a correlation matrix of zooSpending rounded to two decimal places
print(round(cor(zooSpending),2))
# Display a correlation plot using the "number" method and
# limit output to the bottom left
corrplot(cor(zooSpending),
         method = "number",
         type = "lower")
# Generate the linear regression model and
# save it in an object called zooSpendingModel
zooSpendingModel <- lm(data = zooSpending,
                       formula = VisitSpending ~ PartySize + MilesFromZoo +
                         Member)
# Display the beta coefficients for the model on the console
print(zooSpendingModel)
# Display the linear regression model results using the summary() function
print(summary(zooSpendingModel))
# Test for multicollinearity using the ols_vif_tol() function
ols_vif_tol(zooSpendingModel)
