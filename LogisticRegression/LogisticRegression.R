# Aakanksha Arora
# The following code reads a csv file,displays the histogram,correlation matrix
# Correlation plot, splits the dataset into training and testing tibble.
# The class imbalance is handled using SMOTE. A logistic regression model is 
# generated. Odd ratios of 7 independent variables coefficients are calculated.
# Confusion matrix is generated and false positive and false negative rates are 
# calculated. Model's prediction accuracy is calculated. 
# Install the tidyverse, corrplot, olsrr, and smotefamily packages
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("smotefamily")
# Load the tidyverse, corrplot, olsrr, and smotefamily librarie
library(tidyverse)
library(corrplot)
library(olsrr)
library(smotefamily)
# Set the working directory to your Lab06 folder
setwd("C:\\Users\\ual-laptop\\Documents\\Lab06")
# Read MobilePhoneSubscribers.csv into a tibble called mobilePhone
mobilePhone <- read_csv(file = "MobilePhoneSubscribers.csv",
                        col_types = "lillnininn",
                        col_names = TRUE)
# Display mobilePhone in the console
print(mobilePhone)
# Display the structure of mobilePhone in the console
print(str(mobilePhone))
# Display the summary of mobilePhone in the console
print(summary(mobilePhone))
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
# Call the displayAllHistograms() function, passing in 
# mobilePhone as an argument
displayAllHistograms(mobilePhone)
# Display a correlation matrix of mobilePhonerounded to two decimal places
mobilePhonerounded <- round(cor(mobilePhone),2)
print(mobilePhonerounded)
# Display a correlation plot using the "number" method and limit 
# output to the bottom left
corrplot(cor(mobilePhone),
         method = "number",
         type = "lower")
# The correlation plot should reveal three pairwise 
# correlations that are above the threshold of 0.7. 
# Remove the data plan and data usage variables from the tibble
mobilePhone <- mobilePhone %>%
  select(-DataPlan)
mobilePhone <- mobilePhone %>%
  select(-DataUsage)
# Randomly split the dataset into mobilePhoneTraining (75% of records) 
# and mobilePhoneTesting (25% of records) using 203 as the random seed
set.seed(203)
SampleSet <- sample(nrow(mobilePhone),
                    round(nrow(mobilePhone)*0.75),
                    replace = FALSE)
mobilePhoneTraining <- mobilePhone[SampleSet,]
mobilePhoneTesting <- mobilePhone[-SampleSet,]
# Check if we have a class imbalance issue in CancelledService
summary(mobilePhoneTraining$CancelledService)
# Deal with class imbalance using the SMOTE technique using a 
# duplicate size of 3.Save the result into a new 
# tibble called mobilePhoneTrainingSmoted
mobilePhoneTrainingSmoted <- tibble(SMOTE(X = data.frame(mobilePhoneTraining),
                                          target = mobilePhoneTraining$CancelledService,
                                          dup_size = 3)$data)
# Convert CancelledService and RecentRenewal back into logical types
mobilePhoneTrainingSmoted <- mobilePhoneTrainingSmoted %>%
  mutate(CancelledService = as.logical(CancelledService),
         RecentRenewal = as.logical(RecentRenewal) )
# Get rid of the "class" column in the tibble 
# (this was added by the SMOTE technique)
mobilePhoneTrainingSmoted <- mobilePhoneTrainingSmoted %>%
  select(-class)
# Check for class imbalance on the smoted dataset
summary(mobilePhoneTrainingSmoted$CancelledService)
# Generate the logistic regression model and 
# save it in an object called mobilePhoneModel
mobilePhoneModel <- glm(data = mobilePhoneTrainingSmoted,
                        family = binomial,
                        formula = CancelledService ~.)
# Display the logistic regression model results using the summary() function
summary(mobilePhoneModel)
# Calculate the odds ratios for each of the 7 independent variable coefficients
exp(coef(mobilePhoneModel)["AccountWeeks"])
exp(coef(mobilePhoneModel)["RecentRenewalTRUE"])
exp(coef(mobilePhoneModel)["CustServCalls"])
exp(coef(mobilePhoneModel)["AvgCallMinsPerMonth"])
exp(coef(mobilePhoneModel)["AvgCallsPerMonth"])
exp(coef(mobilePhoneModel)["MonthlyBill"])
exp(coef(mobilePhoneModel)["OverageFee"])
# Use the model to predict outcomes in the testing dataset
# Treat anything below or equal to 0.5 as a 0, 
# anything above 0.5 as a 1
mobilePhoneModelPrediction <- predict(mobilePhoneModel, mobilePhoneTesting,
                                      type = "response")
mobilePhoneModelPrediction <- ifelse(mobilePhoneModelPrediction<=0.5,0,1)
# Generate a confusion matrix of predictions
mobilePhoneConfusionMatrix <- table(mobilePhoneTesting$CancelledService,
                                    mobilePhoneModelPrediction)
# Calculate the false positive rate
mobilePhoneConfusionMatrix[1,2]/(mobilePhoneConfusionMatrix[1,2] + 
                                   mobilePhoneConfusionMatrix[1,1])
# Calculate the false negative rate
mobilePhoneConfusionMatrix[2,1]/(mobilePhoneConfusionMatrix[2,1] + 
                                   mobilePhoneConfusionMatrix[2,2])
# Calculate the model prediction accuracy
sum(diag(mobilePhoneConfusionMatrix)/ nrow(mobilePhoneTesting))