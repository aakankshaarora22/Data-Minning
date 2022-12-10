# Aakanksha Arora
# This code demonstrates the summary of the IndonesianRiceFarms
# available in the form of a csv file. Performs data analysis for various
# features and displays DecisionTree visualizations.
# Installing rpart.plot packages
install.packages("rpart.plot") 
# Loading tidyverse and rpart.plot packages
library(rpart.plot) 
library(tidyverse) 
# Setting current directory
setwd("C:/Users/ual-laptop/Desktop/Lab09") 
# Loading data from csv into riceFarms
riceFarms <- read_csv(file = "IndonesianRiceFarms.csv", 
                      col_types = "fnniinf", 
                      col_names = TRUE
) 
# Displaying structure of tibble
print(str(riceFarms))
# Displaying summary of tibble
print(summary(riceFarms))
# Generating 75% and 25% for Training and Testing
set.seed(370) 
sampleSet <- sample(nrow(riceFarms),
                    round(nrow(riceFarms)*.75),
                    replace = FALSE) 
riceFarmsTraining <- riceFarms[sampleSet,]
riceFarmsTesting <- riceFarms[-sampleSet,]
# Generating Decision Tree model for tibble
riceFarmDecisionTreeModel <- rpart(formula = FarmOwnership ~ .,
                                   method = "class", 
                                   cp = 0.01, 
                                   data = riceFarmsTraining) 
# Generating Rpart.plot
rpart.plot(riceFarmDecisionTreeModel) 
# Generating prediction based on model
riceFarmPrediction <- predict(riceFarmDecisionTreeModel, 
                              riceFarmsTesting, 
                              type = 'class') 
# Displaying the prediction data
print(riceFarmPrediction) 
# Generating confusion matrix 
riceFramConfusionMatrix <- table(riceFarmsTesting$FarmOwnership, 
                                 riceFarmPrediction) 
# Displaying confusion matrix
print(riceFramConfusionMatrix) 
# Generating predictiveAccuracy model
predictiveAccuracy <- sum(diag(riceFramConfusionMatrix)) /
  nrow(riceFarmsTesting) 
# Displaying predictiveAccuracy
print(predictiveAccuracy) 
# Generating Decision Tree model for tibble using cp = 0.007 
riceFarmDecisionTreeModelNew <- rpart(formula = FarmOwnership ~ .,
                                      method = "class", 
                                      cp = 0.007, 
                                      data = riceFarmsTraining) 
# Generating Rpart.plot
rpart.plot(riceFarmDecisionTreeModelNew) 
# Generating prediction based on model
riceFarmPredictionNew <- predict(riceFarmDecisionTreeModelNew, 
                                 riceFarmsTesting, 
                                 type = 'class') 
# Displaying the prediction data
print(riceFarmPredictionNew) 
# Generating confusion matrix
riceFramConfusionMatrixNew <- table(riceFarmsTesting$FarmOwnership, 
                                    riceFarmPredictionNew) 
# Generating predictiveAccuracy model
print(riceFramConfusionMatrixNew) 
# Generating predictiveAccuracy model
predictiveAccuracyNew <- sum(diag(riceFramConfusionMatrixNew)) /
  nrow(riceFarmsTesting) 
# Displaying predictiveAccuracy
print(predictiveAccuracyNew)