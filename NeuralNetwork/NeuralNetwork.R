# Aakanksha Arora
# This code imports a dataset of people and generate a neural network model 
# that will predict if a fisher used a chartered boat service based on their 
# fishing catch rate and their annual income.
# Install tidyverse package
# install.packages("tidyverse")
# Load tidyverse package
library(tidyverse)
# Install neuralnet package
# install.packages("neuralnet")
# Load neuralnet library
library(neuralnet)
# Set working directory 
setwd("C:/Users/anjan/Downloads/")
# Read CSV file 
fishingCharter <- read_csv(file = "FishingCharter.csv",
                           col_types = "lnn",
                           col_names = TRUE)
# Display the tibble
print(fishingCharter )
# Display the structure
str(fishingCharter )
# Display the summary
summary(fishingCharter )
# Scale the AnnualIncome and CatchRate variables
fishingCharter <- fishingCharter %>%
  mutate(AnnualIncomeScaled =(AnnualIncome-min(AnnualIncome))/
           (max(AnnualIncome)-min(AnnualIncome)))
fishingCharter <- fishingCharter %>%
  mutate(CatchRateScaled =(CatchRate-min(CatchRate))/
           (max(CatchRate)-min(CatchRate)))
# Randomly split the dataset into fishingCharterTraining (75%)
# and fishingCharterTesting (25% of records) using 591 as seed
set.seed(591)
sampleSet <- sample(nrow(fishingCharter),
                    round(nrow(fishingCharter) * 0.75),
                    replace = FALSE)
fishingCharterTraining <- fishingCharter[sampleSet,]
fishingCharterTesting <- fishingCharter[-sampleSet, ]
# Generate the neural network model to predict CharteredBoat 
# (dependent variable) using AnnualIncomeScaled and CatchRateScaled 
# (independent variables). 
fishingCharterNeuralNet <- neuralnet(
  formula=CharteredBoat ~ AnnualIncomeScaled+CatchRateScaled,
  data=fishingCharterTraining,
  hidden=3,
  act.fct="logistic",
  linear.output =FALSE
)
# Display the neural network numeric results
print(fishingCharterNeuralNet$result.matrix)
# Visualize the neural network
plot(fishingCharterNeuralNet)
# Use fishingCharterNeuralNet to generate probabilities on the 
# fishingCharterTesting data set and store this in fishingCharterProbability
fishingCharterProbability <- compute(fishingCharterNeuralNet,
                                     fishingCharterTesting)
# Display the probabilities from the testing dataset on the console
print(fishingCharterProbability$net.result)
# Convert probability predictions into 0/1 predictions and store this 
# into fishingCharterPrediction
fishingCharterPrediction <-
  ifelse(fishingCharterProbability$net.result >0.5,1,0)
# Display the 0/1 predictions on the console
print(fishingCharterPrediction)
# Evaluate the model by forming a confusion matrix
fishingCharterConfusionMatrix <- table(fishingCharterTesting$CharteredBoat,
                                       fishingCharterPrediction)
# Display the confusion matrix on the console
print(fishingCharterConfusionMatrix)
# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(fishingCharterConfusionMatrix))/
  nrow(fishingCharterTesting)
# Display the predictive accuracy on the console
print(predictiveAccuracy)
