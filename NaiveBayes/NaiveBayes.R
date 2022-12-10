# Aakanksha Arora
# The code generates a naive bayes prediction model to predict dwelling type 
# and calculates the predictive accuracy of the model.
# Install the tidyverse and e1071 packages
# install.packages("tidyverse")
# install.packages("e1071")
# Load the tidyverse and e1071 libraries
library(tidyverse)
library(e1071)
# Set the working directory to your Lab08 folder
setwd("C:\\Users\\ual-laptop\\Documents\\Lab08")
# Read DwellingType.csv into a tibble called dwellingType
dwellingType <- read_csv(file = "DwellingType.csv",
                         col_types = "filll",
                         col_names = TRUE)
# Display dwellingType in the console
print(dwellingType)
# Display the structure of dwellingType in the console
print(str(dwellingType))
# Display the summary of dwellingType in the console
print(summary(dwellingType))
# Use 154 as the random seed to split the dataset
set.seed(154)
# Randomly split the dataset into dwellingTypeTraining (75% of records) and
# dwellingTypeTesting (25% of records)
sampleSet <- sample(nrow(dwellingType),
                    round(nrow(iris) * 0.75),
                    replace = FALSE)
# Put the records of 75% sample into dwellingTypeTraining
dwellingTypeTraining <- dwellingType[sampleSet, ]
# Put the records 25% sample into dwellingTypeTesting
dwellingTypeTesting <- dwellingType[-sampleSet, ]
# Generate the Naive Bayes model to predict DwellingType 
# based on the other variables in the dataset.
dwellingTypeModel <- naiveBayes( formula = DwellingType~.,
                                 data = dwellingTypeTraining,
                                 laplace = 1)
# Build probabilities for each record in the testing 
# dataset and store them in dwellingTypeProbability
dwellingTypeProbability <- predict(dwellingTypeModel,
                                   dwellingTypeTesting,
                                   type = "raw")
# Display dwellingTypeProbability on the console
print(dwellingTypeProbability)
# Predict classes for each record in the testing 
# dataset and store them in dwellingTypePrediction
dwellingTypePrediction <- predict(dwellingTypeModel,
                                  dwellingTypeTesting,
                                  type = "class")
# Display dwellingTypePrediction on the console
print(dwellingTypePrediction)
# Evaluate the model by forming a confusion matrix
dwellingTypeConfusionMatrix <- table(dwellingTypeTesting$DwellingType,
                                     dwellingTypePrediction)
# Display the confusion matrix on the console
print(dwellingTypeConfusionMatrix)
# Calculate the model predictive accuracy and
# store it into a variable called predictiveAccuracy
predictiveAccuracy <- sum(diag(dwellingTypeConfusionMatrix)) / 
  nrow(dwellingTypeTesting)
# Display the predictive accuracy on the console
print(predictiveAccuracy)