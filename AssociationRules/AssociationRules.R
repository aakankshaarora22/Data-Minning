# Aakanksha Arora
# Description: In this lab we will import a dataset of Instacart grocery
# transactions and generate association rules among items in a transaction
# Environment Setup --------------------------------------------------------
# Installing "arules" package
# install.packages("arules")
# Loading tidyverse package
library(tidyverse)
# Loading arules package
library(arules)
# Set the working directory to your Lab10 Folder
setwd("C:/Users/ual-laptop/Desktop/Lab10")
# Read InstacartTransactions.csv into a tibble called InstacartTransactions
instacartTransactions <- read.transactions(file = 
                                             "InstacartTransactions.csv",
                                           format = "single",
                                           header = "TRUE",
                                           sep = ",",
                                           cols = c("OrderID", "ItemID"))
# Displaying the summary of instacartTransactions tibble
print(summary(instacartTransactions))
# Displaying the first 3 transactions of instacartTransactions tibbel
inspect(instacartTransactions[1:3])
# Finding frequency of "24852" (bananas) in the instacartTransactions tibble
itemFrequency(instacartTransactions[,"24852"])
# Generating Frequency tibble frominstacartTransactions tibble
instacartTransactionsFrequency <-
  tibble(Items = names(itemFrequency(instacartTransactions)),
         Frequency = itemFrequency(instacartTransactions))
# Displaying the frequencies of items in the console
print(instacartTransactionsFrequency)
# Displaying top 10 items that are frequently purchased
instacartTransactionsFrequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)
# Generating association rules model in an object 
instacartTransactionRules <-
  apriori(instacartTransactions,
          parameter = list(
            support = 0.005,
            confidence = 0.2,
            minlen = 2)
  )
# Displaying summary of association rules model
summary(instacartTransactionRules)
# Displaying the top 10 rules of association rules model
inspect(instacartTransactionRules[1:10])
# Sorting the association rules model based on lift and printing top 10 
records
instacartTransactionRules %>%
  sort(by = "lift") %>%
  head(n=10) %>%
  inspect()