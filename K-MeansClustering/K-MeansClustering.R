# Aakanksha Arora
# The code generates a cluster for the dataset. Optimizes the value of K. 
# Other features help in determining the similarities and differences among 
# Clusters. 
# Install the tidyverse and factoextra packages 
# install.packages("tidyverse") 
# install.packages("factoextra") 
# load the required library 
library(tidyverse) 
library(factoextra) 
library(cluster) 
library(gridExtra) 
library(stats) 
# Set the working directory to Lab11 folder 
setwd("C:/Users/ual-laptop/Documents/Lab11") 
# Read CountryData.csv into a tibble called CountryData 
CountryData <- read_csv(file = "CountryData.csv", 
                        col_types = "cnnnnini", 
                        col_names = TRUE) 
# Display CountryData 
print(CountryData) 
# Display the structure of CountryData 
print(str(CountryData)) 
# Display the summary of CountryData 
print(summary(CountryData)) 
# Convert column containing the country name to the row title of the tibble 
CountryData <- CountryData %>% column_to_rownames(var = "Country") 
# Remove countries from the tibble with missing data in any feature 
CountryData <- CountryData %>% drop_na() 
# View the summary of the countries tibble again to ensure there are no NA 
# values 
print(summary(CountryData)) 
# Create a new tibble called countriesScaled containing only corruption index 
# and the number of days it takes to open a business 
countriesScaled <- CountryData %>% select(CorruptionIndex, DaysToOpenBusiness) 
# scale them so they have equal impact on the clustering calculations 
countriesScaled <- countriesScaled %>% scale() 
# Set the random seed to 679
set.seed(679)
# Generate the k-means clusters in an object called countries4Clusters using 4 
# clusters and a value of 25 for nstart
countries4Clusters <- kmeans(x = countriesScaled,
                             centers = 4,
                             nstart = 25)
# Display cluster sizes 
countries4Clusters$size
# Display cluster centers (z-scores) 
countries4Clusters$centers
# Visualize the clusters
fviz_cluster(object = countries4Clusters,
             data = countriesScaled,
             repel = FALSE)
# Optimize the value for k 
# elbow method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "wss")
# average silhouette method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "silhouette")
# gap statistic method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "gap_stat")
# Regenerate the cluster analysis using the optimal number of clusters
# Generate the k-means clusters in an object called countries3Clusters using 3 
# clusters and a value of 25 for nstart
countries3Clusters <- kmeans(x = countriesScaled,
                             centers = 3,
                             nstart = 25)
# Display cluster sizes
countries3Clusters$size
# Display cluster centers (z-scores) 
countries3Clusters$centers
# Visualize the clusters
fviz_cluster(object = countries3Clusters,
             data = countriesScaled,
             repel = FALSE)
# Determine similarities and differences among the clusters using the remaining 
# features in the dataset (GiniCoefficient, GDPPerCapita, EduPercGovSpend, 
# EduPercGDP, and CompulsoryEducationYears
clusterDiffSim <- CountryData %>%
  mutate(cluster = countries3Clusters$cluster) %>%
  select(cluster,
         GiniCoefficient, 
         GDPPerCapita, 
         EduPercGovSpend, 
         EduPercGDP, 
         CompulsoryEducationYears) %>%
  group_by(cluster) %>%
  summarise_all("mean")
print(clusterDiffSim)