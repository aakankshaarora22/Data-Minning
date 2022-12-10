# Aakanksha Arora
# The following code reads a csv file, displays the statistics and summaries of
# attributes. Data pre-processing is also done. Some interesting insights of the
# dataset are calculated. The models created are logistic regression, k-nearest 
# neighbors, decision trees, naive bayes and neural network.

# Install packages used in the model
# install.packages("dummies",repos=NULL,type="source")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("dummies")
# install.packages("e1071")
# install.packages("rpart.plot")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("neuralnet")
# install.packages("class")

# Load libraries for the packages
library(dummies)
library(tidyverse)
library(corrplot)
library(olsrr)
library(class)
library(e1071)
library(rpart)
library(rpart.plot)
library(dplyr)
library(neuralnet)

# Set working Directory
setwd("C:\\Users\\ual-laptop\\Documents\\GitHub\\WorkersProductivityProject")

# Read file using read_csv
garmentProd <- read_csv(file= "garments_worker_productivity.csv",
                        col_types = "Dffffnnnninnnn",
                        col_names = TRUE)

# Display the garmentProd tibble on console
print(garmentProd)

# Display the structure of tibble
str(garmentProd)

# Display the summary of tibble 
summary(garmentProd)

# Boxplot of work in progress
boxplotWip <- ggplot(data = garmentProd,
               aes(x = wip))
boxplotWip + geom_boxplot(color = "blue",
                    fill = "lightblue") 

# Boxplot of over_time
boxplotOvertime <- ggplot(data = garmentProd,
               aes(x = over_time))
boxplotOvertime + geom_boxplot(color = "blue",
                    fill = "lightblue") 

# Remove the unwanted columns 
garmentProd <- garmentProd %>% 
  select(-idle_time, -idle_men, -no_of_style_change, -date)

# Display the summary of tibble 
summary(garmentProd)

# Determine outliers in the over_time feature 
# Calculate outlier min and max 
outlierMinOver_time <- quantile(garmentProd$over_time, .25)- 
  (IQR(garmentProd$over_time)*1.5)
outlierMaxOver_time <- quantile(garmentProd$over_time, .75)+ 
  (IQR(garmentProd$over_time)*1.5)

# Remove outliers from the dataset
garmentProd <- garmentProd %>%
  filter(over_time >= outlierMinOver_time & over_time <= outlierMaxOver_time)

# Determine outliers in the incentive feature
outlierMinIncentive <- quantile(garmentProd$incentive, .25)- 
  (IQR(garmentProd$incentive)*1.5)
outlierMaxIncentive <- quantile(garmentProd$incentive, .75)+ 
  (IQR(garmentProd$incentive)*1.5)

# Remove outliers from the incentive
garmentProd <- garmentProd %>%
  filter(incentive >= outlierMinIncentive & incentive <=outlierMaxIncentive)
garmentProd1 <- garmentProd %>%
  mutate(wip = ifelse(is.na(wip),
                   mean(wip, na.rm = TRUE),wip))

# Display the summary of tibble 
summary(garmentProd1)

# Normalise the wip, smv, over_time,incentive features
garmentProd2 <- garmentProd1 %>%
  mutate(wip = round((wip - min(wip))/
            (max(wip) - min(wip)),2)) %>%
         mutate(smv = round((smv - min(smv))/
                       (max(smv) - min(smv)),2)) %>%
         mutate(over_time = round((over_time - min(over_time))/
                       (max(over_time) - min(over_time)),2)) %>%
         mutate(incentive = round((incentive - min(incentive))/
                       (max(incentive) - min(incentive)),2))

# Display the summary of tibble 
summary(garmentProd2)

# Display all Histograms
displayAllHistograms <- function(tibbleDataset){
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping =aes(x=value, fill=key),
                              color="black")+
    facet_wrap(~ key, scales="free") +
    theme_minimal()
}
displayAllHistograms(garmentProd2)

# Analysing data using dplyr functions
# 1. Checking proportion of productivity vs unproductivity according to quarter
print(garmentProd2) %>%
  group_by(quarter)%>%
  summarize(
    
    percentProductive = 
      length(actual_productivity[actual_productivity==TRUE])*100/
      length(actual_productivity)
    ,
    percentUnproductive = 
      length(actual_productivity[actual_productivity==FALSE])*100/
      length(actual_productivity)
  )

# 2. Checking proportion of productivity vs unproductivity according to 
# day of week
print(garmentProd2 %>%
        mutate(orderOfDay = case_when(
          day =="Monday" ~ '1_Monday',
          day =="Tuesday" ~ '2_Tuesday',
          day =="Wednesday" ~ '3_Wednesday',
          day =="Thursday" ~ '4_Thursday',
          day =="Friday" ~ '5_Friday',
          day =="Saturday" ~ '6_Saturday',
          day =="Sunday" ~ '7_Sunday'
        ))) %>%
  group_by(orderOfDay)%>%
  summarize(
      percentProductive = 
        length(actual_productivity[actual_productivity==TRUE])*100/
        length(actual_productivity),
      percentUnproductive = 
        length(actual_productivity[actual_productivity==FALSE])*100/
        length(actual_productivity))

# 3. Checking productivity vs unproductivity according to number of workers
print(garmentProd2 %>%
        mutate(workerCount = case_when(
          no_of_workers <= 9 ~ '1_(0-9)',
          no_of_workers <= 34 ~ '2_(10-34)',
          no_of_workers <= 57 ~ '3_(35-57)',
          no_of_workers >57 ~ '4_(>57)',
      ))) %>%
  group_by(workerCount)%>%
  summarize(
    percentProductive = 
      length(actual_productivity[actual_productivity==TRUE])*100/
      length(actual_productivity),
    percentUnproductive = 
      length(actual_productivity[actual_productivity==FALSE])*100/
      length(actual_productivity)
  )


# Display correlation matrix rounded to two decimal places
round(cor(garmentProd2 %>% keep(is.numeric)),2)

# Correlation plot using number method
corrplot(cor(garmentProd2 %>% keep(is.numeric)),
         method= "number",
         type = "lower",
         number.cex = 0.5)

# Removing no_of_workers & over_time due to multicollinearity 
garmentProd3 <- garmentProd2 %>% 
  select(-no_of_workers, -over_time)

# Pairwise Correlation
cor(garmentProd3$actual_productivity, garmentProd3$targeted_productivity)
cor(garmentProd3$actual_productivity, garmentProd3$wip)
cor(garmentProd3$actual_productivity, garmentProd3$smv)
cor(garmentProd3$actual_productivity, garmentProd3$incentive)

# Dummy code departments using dummy.data.frame
garmentProdDataFrame1 <-data.frame(garmentProd3)
garmentProd4 <- as_tibble(dummy.data.frame(data=garmentProdDataFrame1,
                                           name="department"))

# Remove one of the department 
garmentProd4 <- garmentProd4 %>% 
  select(-departmentfinishing)

# Dummy code quarter using dummy.data.frame
garmentProdDataFrame2 <-data.frame(garmentProd4)
garmentProd5 <- as_tibble(dummy.data.frame(data=garmentProdDataFrame2,
                                           name="quarter"))
# Remove one of the quarter
garmentProd5 <- garmentProd5 %>% 
  select(-quarterQuarter5)

# Dummy code day using dummy.data.frame
garmentProdDataFrame3 <-data.frame(garmentProd5)
garmentProd6 <- as_tibble(dummy.data.frame(data=garmentProdDataFrame3,
                                           name="day"))

# Remove one of the day
garmentProd6 <- garmentProd6 %>% 
  select(-daySunday)

# Dummy code team using dummy.data.frame
garmentProdDataFrame4 <-data.frame(garmentProd6)
garmentProd7 <- as_tibble(dummy.data.frame(data=garmentProdDataFrame4,
                                           name="team"))

# Remove one of the team12
garmentProd7 <- garmentProd7 %>% 
  select(-team12)
                                           
# Logistic Regression Model
# Split dataset into training and testing
# The set.seed() function is used to ensure that we can get the same result 
# every time we run a random sampling process.
set.seed(1234)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSetLR <- sample(nrow(garmentProd7),
                    round(nrow(garmentProd7)*0.75),
                    replace = FALSE)

# Put the records from the 75% sample into garmentProdTrainingLR
garmentProdTrainingLR <- garmentProd7[sampleSetLR,]

# Put all other records(25%) into garmentProdTestingLR
garmentProdTestingLR <- garmentProd7[-sampleSetLR,]

# Class Imbalance in the training dataset
summary(garmentProdTrainingLR$actual_productivity)

# Store the  magnitude of class imbalance in a variable
classImbalanceMagnitude <- 533/356

# Convert actual_productivity to logical data type
garmentProdTrainingLR$actual_productivity <- 
  as.logical(garmentProdTrainingLR$actual_productivity)

# Generate Logistic Regression Model
garmentProdModelLR <- glm(data = garmentProdTrainingLR,
                         family = binomial,
                         formula = actual_productivity~.)

# Display the output of the logistic regression model
summary(garmentProdModelLR)

# Calculate the odds ratio for each coefficient
exp(coef(garmentProdModelLR)["quarterQuarter1"])
exp(coef(garmentProdModelLR)["quarterQuarter2"])
exp(coef(garmentProdModelLR)["quarterQuarter3"])
exp(coef(garmentProdModelLR)["quarterQuarter4"])
exp(coef(garmentProdModelLR)["departmentsewing"])
exp(coef(garmentProdModelLR)["dayThursday"])
exp(coef(garmentProdModelLR)["daySaturday"])
exp(coef(garmentProdModelLR)["dayMonday"])
exp(coef(garmentProdModelLR)["dayTuesday"])
exp(coef(garmentProdModelLR)["team1"])
exp(coef(garmentProdModelLR)["team2"])
exp(coef(garmentProdModelLR)["team3"])
exp(coef(garmentProdModelLR)["team4"])
exp(coef(garmentProdModelLR)["team5"])
exp(coef(garmentProdModelLR)["team6"])
exp(coef(garmentProdModelLR)["team7"])
exp(coef(garmentProdModelLR)["team8"])
exp(coef(garmentProdModelLR)["team9"])
exp(coef(garmentProdModelLR)["team10"])
exp(coef(garmentProdModelLR)["team11"])
exp(coef(garmentProdModelLR)["targeted_productivity"])
exp(coef(garmentProdModelLR)["wip"])
exp(coef(garmentProdModelLR)["smv"])
exp(coef(garmentProdModelLR)["incentive"])

# Use the model to predict outcomes in the testing dataset
garmentProdPredictionLR <- predict(garmentProdModelLR,
                                  garmentProdTestingLR,
                                  type= "response")
# Display prediction on console
print(garmentProdPredictionLR)

# Treat anything below or equal to 0.5 as 0 and anything above 0.5 as 1
garmentProdPredictionLR <- ifelse(garmentProdPredictionLR >= 0.5,1,0)

# Display prediction on console
print(garmentProdPredictionLR)

# Create a confusion Matrix 
garmentProdConfusionMatrixLR <- table(garmentProdTestingLR$actual_productivity,
                                     garmentProdPredictionLR)

# Display Confusion Matrix
print(garmentProdConfusionMatrixLR)

# Calculate the false positive rate
garmentProdConfusionMatrixLR[1,2]/
  (garmentProdConfusionMatrixLR[1,2]+
     garmentProdConfusionMatrixLR[1,1])

# Calculate the false negative rate
garmentProdConfusionMatrixLR[2,1]/
  (garmentProdConfusionMatrixLR[2,1]+
     garmentProdConfusionMatrixLR[2,2])

# Calculate the prediction accuracy by dividing the number of true positives 
# and true negatives by the total amount of predictions in the testing dataset  
sum(diag(garmentProdConfusionMatrixLR))/nrow(garmentProdTestingLR)

# Naive Bayes Prediction Model
# Defining number of bins
no_of_bins <- 155

# Smoothing by bins average
garmentProd8 <- garmentProd7 %>%
  mutate(wip = round(ave(garmentProd7$wip,
                         rep(1:length(garmentProd7$wip),
                             each = no_of_bins,
                             length.out = length(garmentProd7$wip))),2),
         incentive = round(ave(garmentProd7$incentive,
                               rep(1:length(garmentProd7$incentive),
                                   each = no_of_bins,
                                   length.out = length
                                   (garmentProd7$incentive))),2),
         smv = round(ave(garmentProd7$smv,
                         rep(1:length(garmentProd7$smv),
                             each = no_of_bins,
                             length.out = length(garmentProd7$smv))),2),
         targeted_productivity = round(ave(garmentProd7$targeted_productivity,
                                          rep(1:length(
                                           garmentProd7$targeted_productivity),
                                           each = no_of_bins,
                                           length.out = length
                                           (garmentProd7$targeted_productivity)
                                           )),2))

# Split dataset into training and testing
# The set.seed() function is used to ensure that we can get the same result
# every time we run a random sampling process.
set.seed(1234)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSetNaive <- sample(nrow(garmentProd8),
                         round(nrow(garmentProd8)*0.75),
                         replace = FALSE)

# Put the records from the 75% sample into garmentProdTrainingNB
garmentProdTrainingNB <- garmentProd8[sampleSetNaive,]

# Put all other records(25%) into garmentProdTestingNB
garmentProdTestingNB <- garmentProd8[-sampleSetNaive,]

# Train the Naive Bayes model
garmentProdModelNB <- naiveBayes(formula = actual_productivity ~.,
                                 data = garmentProdTrainingNB,
                                 laplace = 1)

# Build probabilities for each record in the testing dataset
# and store them in garmentProdNaiveProbability
garmentProdProbabilityNB <- predict(garmentProdModelNB,
                                    garmentProdTestingNB,
                                    type = "raw")

# Display garmentProdNaiveProbability on the console
print(garmentProdProbabilityNB)

# Predict classes for each record in the testing dataset
# and store them in garmentProdNaivePrediction
garmentProdPredictionNB <- predict(garmentProdModelNB,
                                   garmentProdTestingNB,
                                   type = "class")

# Display garmentProdNaivePrediction on the console
print(garmentProdPredictionNB)

# Evaluate the model by forming a confusion matrix
garmentProdConfusionMatrixNB <- table(garmentProdTestingNB$actual_productivity,
                                      garmentProdPredictionNB)

# Display the confusion matrix on the console
print(garmentProdConfusionMatrixNB)

# Calculate the false positive rate
garmentProdConfusionMatrixNB[1,2]/
  (garmentProdConfusionMatrixNB[1,2]+
     garmentProdConfusionMatrixNB[1,1])

# Calculate the false negative rate
garmentProdConfusionMatrixNB[2,1]/
  (garmentProdConfusionMatrixNB[2,1]+
     garmentProdConfusionMatrixNB[2,2])

# Calculate the model predictive accuracy
predictiveAccuracyNB <- sum(diag(garmentProdConfusionMatrixNB))/
  nrow(garmentProdTestingNB)

# Display predictiveNaiveAccuracy on the console
print(predictiveAccuracyNB)

# Decision Tree Model

# Split dataset into training and testing
# The set.seed() function is used to ensure that we can get the same result
# every time we run a random sampling process.
set.seed(1234)

# Discretizing values for wip,incentive,smv
garmentProd9 <- garmentProd7 %>%
  mutate(wip = cut(wip,
                   breaks=c
                   (-0.01,0.00,0.02,0.04,0.06,0.08,0.1,0.3,0.5,0.7,1.00),
                   labels=c('0.00','0.00', '0.03', '0.05', '0.07','0.09','0.2',
                            '0.4','0.6','1')))%>%
  mutate(incentive = cut(incentive,
                         breaks=c(-0.01,0.00,0.2,0.4,0.6,0.8,1),
                         labels=c('0.00','0.00', '0.3', '0.5', '0.7','1')))%>%
  mutate(smv = cut(smv,
                   breaks=c(-0.01,0.00,0.02,0.04,0.06,0.08,0.1,0.3,0.5,0.7,1),
                   labels=c
                   ('0.00','0.00', '0.03', '0.05', '0.07',
                    '0.09','0.2','0.4','0.6','1')))

# Display summary of the tibble
summary(garmentProd9)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSetDecision <- sample(nrow(garmentProd9),
                            round(nrow(garmentProd9)*0.75),
                            replace = FALSE)

# Put the records from the 75% sample into garmentProdTrainingDecision
garmentProdTrainingDecision <- garmentProd9[sampleSetDecision,]

# Put all other records(25%) into garmentProdTestingDecision
garmentProdTestingDecision <- garmentProd9[-sampleSetDecision,]

# Train the decision tree model
garmentProdDecisionTreeModel <- rpart(formula = actual_productivity ~ .,
                                      method = "class",
                                      cp = 0.01,
                                      data = garmentProdTrainingDecision)

# Adjusting the margin
par(mar=c(2,2,2,2))
par("mar")

# Display the decision tree plot
rpart.plot(garmentProdDecisionTreeModel)

# Predict classes for each record in the testing dataset
garmentProdDecisionPrediction <- predict(garmentProdDecisionTreeModel,
                                         garmentProdTestingDecision,
                                         type = "class")

# Display the predictions on the console
print(garmentProdDecisionPrediction)

# Evaluate the model by confusion matrix
garmentProdDecisionConfusionMatrix <-
  table(garmentProdTestingDecision$actual_productivity,
        garmentProdDecisionPrediction)

# Display the confusion matrix
print(garmentProdDecisionConfusionMatrix)

# Calculate the model predictive accuracy
garmentProdDecisionpredictiveAccuracy <-
  sum(diag(garmentProdDecisionConfusionMatrix)) /
  nrow(garmentProdTestingDecision)

# Display the predictive Accuracy
print(garmentProdDecisionpredictiveAccuracy)

# Generate a model with 0.007 as complexity parameter
garmentProdDecisionTreeModel <- rpart(formula = actual_productivity ~ .,
                                      method = "class",
                                      cp = 0.007,
                                      data = garmentProdTrainingDecision)

# Display the decision tree plot
rpart.plot(garmentProdDecisionTreeModel)

# Predict classes for each record in the testing dataset
garmentProdDecisionPrediction <- predict(garmentProdDecisionTreeModel,
                                         garmentProdTestingDecision,
                                         type = "class")

# Display the predictions on the console
print(garmentProdDecisionPrediction)

# Evaluate the model by confusion matrix
garmentProdDecisionConfusionMatrix <-
  table(garmentProdTestingDecision$actual_productivity,
        garmentProdDecisionPrediction)

# Display the confusion matrix
print(garmentProdDecisionConfusionMatrix)

# Calculate the false positive rate
garmentProdDecisionConfusionMatrix[1,2]/
  (garmentProdDecisionConfusionMatrix[1,2]+
     garmentProdDecisionConfusionMatrix[1,1])

# Calculate the false negative rate
garmentProdDecisionConfusionMatrix[2,1]/
  (garmentProdDecisionConfusionMatrix[2,1]+
     garmentProdDecisionConfusionMatrix[2,2])

# Calculate the model predictive accuracy
garmentProdDecisionPredictiveAccuracy <-
  sum(diag(garmentProdDecisionConfusionMatrix)) /
  nrow(garmentProdTestingDecision)

# Display the predictive Accuracy
print(garmentProdDecisionPredictiveAccuracy)

# K-Nearest Neighbors Model
# Separate the tibble into two. One with just the label and one with the other 
# variables 
garmentProd7KLabels <- garmentProd7 %>% select(actual_productivity)
garmentProd10 <- garmentProd7 %>% select(-actual_productivity)

# Split data into training and testing
# The set.seed() function is used to ensure that we can get the same result
# everytime we run a random sampling process
set.seed(1234)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSetK <- sample(nrow(garmentProd10),
                     round(nrow(garmentProd10)*0.75),
                     replace = FALSE)

# Put the records from the 75% sample into garmentProd10KTraining
garmentProd7KTraining <- garmentProd10[sampleSetK, ]
garmentProd7KTrainingLabels <- garmentProd7KLabels[sampleSetK, ]
garmentProd7KTrainingLabels <- na.omit(garmentProd7KTrainingLabels)

# Put all the records (25%) into garmentProd5KTesting 
garmentProd7KTesting <- garmentProd10[-sampleSetK, ]
garmentProd7KTestingLabels <- garmentProd7KLabels[-sampleSetK,]

# Generate the k-nearest neighbors model
garmentProd7KPrediction <- knn(train = garmentProd7KTraining,
                               test = garmentProd7KTesting,
                               cl = 
                               garmentProd7KTrainingLabels$actual_productivity,
                               k = 25)

# Display the predictions from the testing dataset on the console
print(garmentProd7KPrediction)

# Display the summary of the prediction 
summary(garmentProd7KPrediction)

# Evaluate the model by forming a confusion matrix
garmentProd7KConfusionMatrix <- 
  table(garmentProd7KTestingLabels$actual_productivity,
                                      garmentProd7KPrediction)

# Display the confusion Matrix
print(garmentProd7KConfusionMatrix)

# Calculate the model predictive accuracy 
garmentProd7KPredictiveAccuracy <- sum(diag(garmentProd7KConfusionMatrix))/
  nrow(garmentProd7KTesting)

print(garmentProd7KPredictiveAccuracy)

# Create the matrix of K-values with their predictive accuracy
kValueMatrix <- matrix(data= NA,
                       nrow = 0,
                       ncol = 2)

# Assign column names to the matrix
colnames(kValueMatrix) <- c("k value", "Predictive Accuracy")

# Loop through with different values of K to determine the best fitting 
# model using odd numbers from 1 to the number of observations in the 
# training data set
for(kValue in 1:100){
  # Only calculate predictive accuracy if the k value is odd 
  if(kValue %% 2!=0){
    
    # Generate the Model
    garmentProd7KPrediction <- knn(train = garmentProd7KTraining,
                                   test = garmentProd7KTesting,
                                   cl = garmentProd7KTrainingLabels$
                                        actual_productivity,
                                   k = kValue,
                                  )
    
    # Generate the Confusion Matrix 
    garmentProd7KConfusionMatrix <- 
      table(garmentProd7KTestingLabels$actual_productivity,
                                          garmentProd7KPrediction)
  
    # Display confusion matrix
    print(garmentProd7KConfusionMatrix)
    
    # Calculate the false positive rate
    garmentProd7KConfusionMatrix[1,2]/
      (garmentProd7KConfusionMatrix[1,2]+
         garmentProd7KConfusionMatrix[1,1])
    
    # Calculate the false negative rate
    garmentProd7KConfusionMatrix[2,1]/
      (garmentProd7KConfusionMatrix[2,1]+
         garmentProd7KConfusionMatrix[2,2])
    
    # Calculate the predictive accuracy
    garmentProd7PredictiveAccuracy <- sum(diag(garmentProd7KConfusionMatrix))/
      nrow(garmentProd7KTesting)
    
    # Add a new row to the kValueMatrix
    kValueMatrix <- rbind(kValueMatrix, 
                          c(kValue,garmentProd7KPredictiveAccuracy))
    
  }
  
}

# Display and view the kValueMatrix to determine the best k value
print(kValueMatrix)

# Neutral Network Model
# Scaling the targeted_productivity field
garmentProd11 <- garmentProd7 %>%
  mutate(targetedProductivityScaled = 
           (targeted_productivity - min(targeted_productivity)) /
           (max(targeted_productivity) - min(targeted_productivity)))

# Split dataset into training and testing
# The set.seed() function is used to ensure that we can get the same result 
# every time we run a random sampling process.
set.seed(1234)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSetNN <- sample(nrow(garmentProd11),
                    round(nrow(garmentProd11) * 0.75),
                    replace = FALSE)

# Put the records from the 75% sample into garmentProdTraining
# Put all other records(25%) into garmentProdTesting
garmentProdTrainingNN <- garmentProd11[sampleSetNN, ]
garmentProdTestingNN <- garmentProd11[-sampleSetNN, ]

# Displaying column names
print(colnames(garmentProd7))

# Generate the neural network model to predict actual_productivity
garmentProdNeuralNet <- neuralnet(
  formula = actual_productivity ~  wip + smv + incentive,
  data = garmentProdTrainingNN,
  hidden = 1,
  act.fct ="logistic",
  linear.output = FALSE,
  threshold = 0.02,
  rep = 2,
  stepmax = 1e+08
)

# Display and visualize the neural network numeric results
print(garmentProdNeuralNet$result.matrix)
plot(garmentProdNeuralNet)

# Using fishingCharterNeuralNet to generate probabilities and displaying them
garmentProductivityProbabilityNN <- neuralnet::compute(garmentProdNeuralNet,
                                                     garmentProdTestingNN)

# Display garmentProductivityProbabilityNN
print(garmentProductivityProbabilityNN$net.result)

# Converting probability predictions into 0/1 predictions
garmentProductivityPredictionNN <-
  ifelse(garmentProductivityProbabilityNN$net.result > 0.5,1,0)

# Display garmentProductivityPredictionNN
print(garmentProductivityPredictionNN)

# Evaluating the model by forming a confusion matrix
garmentProdConfusionMatrixNN <- 
  table(garmentProdTestingNN$actual_productivity,
                                      garmentProductivityPredictionNN)

# Display garmentProdConfusionMatrixNN
print(garmentProdConfusionMatrixNN)

# Calculate the false positive rate
garmentProdConfusionMatrixNN[1,2]/
  (garmentProdConfusionMatrixNN[1,2]+
     garmentProdConfusionMatrixNN[1,1])

# Calculate the false negative rate
garmentProdConfusionMatrixNN[2,1]/
  (garmentProdConfusionMatrixNN[2,1]+
     garmentProdConfusionMatrixNN[2,2])

# Calculating the model predictive accuracy
predictiveAccuracyNN <- sum(diag(garmentProdConfusionMatrixNN)) /
  nrow(garmentProdTestingNN)

# Display predictiveAccuracyNN
print(predictiveAccuracyNN)

