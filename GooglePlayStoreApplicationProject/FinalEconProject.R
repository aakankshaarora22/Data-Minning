setwd("C:\\Users\\ual-laptop\\Desktop\\FinalProject")
install.packages("dummies",repos=NULL,type="source")
install.packages("corrplot")
install.packages("tidyverse")
install.packages("olsrr")
install.packages("writexl")
library(dummies)
library(tidyverse)
library(corrplot)
library(olsrr)

# Read file
gpdata <- read_csv(file= "googleplaystore.csv",
                   col_types = "cfnncffnffccc",
                   col_names = TRUE)
# Clean Type column
gpdataCleaned <- gpdata %>%
  mutate(Type = ifelse(Type == "Free", 0, 1))

# Remove unwanted columns
gpDataCleaned <- gpdataCleaned %>% 
  select(-App, -Category, -Content_Rating, -Genres, 
         -Current_Ver, -Last_Updated, -Android_Ver)


summary(gpDataCleaned$Installs)
summary(gpDataCleaned$Reviews)

gpDataCleaned$Size <- as.character(gpDataCleaned$Size)
convert_k_M <- function(x){  
  last_char <- substr(x,nchar(x), nchar(x))  
  if (last_char == 'k'){    
    x <- round(as.numeric(substr(x, 1, nchar(x)-1))/1024,2)    
    return (paste(x,'M', sep='')) 
  }
  else   {
    return (x)  
  }
}
tmp <- sapply(gpDataCleaned$Size, convert_k_M)
gpDataCleaned$Size <- tmp
gpDataCleaned$Size <- str_replace_all(gpDataCleaned$Size,"M","")
gpDataCleaned$Size <- as.numeric(gpDataCleaned$Size)

summary(gpDataCleaned)

# # # Write in excel
#  df <- data.frame(gpDataCleaned)
# library(writexl)
#  write_xlsx(df,"C:\\Users\\ual-laptop\\Desktop\\FinalProject\\nahichahiye.csv")
#  gpDataCleaned <- as_tibble(gpDataCleaned)
 
 # Histogram
 displayAllHistograms <- function(tibbleDataset){
   tibbleDataset %>%
     keep(is.numeric) %>%
     gather() %>%
     ggplot() + geom_histogram(mapping =aes(x=value, fill=key),
                               color="black")+
     facet_wrap(~ key, scales="free") +
     theme_minimal()
 }
 displayAllHistograms(gpDataCleaned)

gpDataCleaned <- gpDataCleaned %>%
  mutate(Installs = ifelse(Installs == "0+" | Installs == "1+" |Installs == "5+",1,
                           ifelse(Installs == "10+"| Installs == "50+",2,
                                  ifelse(Installs == "100+"  |Installs == "500+",3,
                                         ifelse( Installs == "5,000+" |Installs == "1,000+",4,
                                                 ifelse(Installs == "10,000+"|Installs == "50,000+" ,5,
                                                        ifelse( Installs == "100,000+" | Installs == "500,000+",6,
                                                                ifelse( Installs == "1,000,000+" | Installs == "5,000,000+",7,
                                                                        ifelse(  Installs == "10,000,000+"| Installs == "50,000,000+",8,
                                                                                 ifelse( Installs == "100,000,000+"| Installs == "500,000,000+",9,
                                                                                         ifelse( Installs == "1,000,000,000+",10,11)))))))))))

gpDataCleaned <- gpDataCleaned %>%
  mutate(Reviews = ifelse(Reviews >= 0 & Reviews <= 1000,1,
                          ifelse(Reviews >= 1001 & Reviews <= 2000,2,
                                 ifelse(Reviews >= 2001 & Reviews <= 3000,3,
                                        ifelse(Reviews >= 3001 & Reviews <= 4000,4,
                                               ifelse(Reviews >= 4001 & Reviews <= 5000,5,
                                                      ifelse(Reviews >= 5001 & Reviews <= 6000,6,
                                                             ifelse(Reviews >= 6001 & Reviews <= 7000,7,
                                                                    ifelse(Reviews >= 7001 & Reviews <= 8000,8,
                                                                           ifelse(Reviews >= 8001 & Reviews <= 9000,9,
                                                                                  ifelse(Reviews >= 9001 & Reviews <= 10000,10,11))
                                                                    )))))))))


# Dummy coding Installs
gpdataDataFrame <- data.frame(gpDataCleaned)
gpdataDataFrame <- na.omit(gpdataDataFrame)
gpDataCleaned1 <- as_tibble(dummy.data.frame(data = gpdataDataFrame,
                                             names = "Installs"))

# Dummy coding Reviews
gpdataDataFrame1 <- data.frame(gpDataCleaned1)
gpdataDataFrame1 <- na.omit(gpdataDataFrame1)
gpDataCleaned2 <- as_tibble(dummy.data.frame(data = gpdataDataFrame1,
                                             names = "Reviews"))

summary(gpDataCleaned2)
gpDataCleaned2 <- unique(gpDataCleaned2)

# Outlier
# outlierMinReviews <- quantile(gpDataCleaned1$Reviews, .25)-(IQR(gpDataCleaned1$Reviews)*1.5)
# outlierMaxReviews <- quantile(gpDataCleaned1$Reviews, .75)+(IQR(gpDataCleaned1$Reviews)*1.5)
# gpDataCleaned1 <- gpDataCleaned1 %>% 
#   filter(Reviews >= outlierMinReviews & Reviews <= outlierMaxReviews )

#Normalise Price
gpDataCleaned2 <- gpDataCleaned2 %>%
  mutate((Price = (Price - min(Price))/
            (max(Price) - min(Price))))

gpDataCleaned2 <- gpDataCleaned2 %>% 
  select(-Price)

# Normalise Reviews
# gpDataCleaned1 <- gpDataCleaned1 %>%
#   mutate((Reviews = (Reviews - min(Reviews))/
#             (max(Reviews) - min(Reviews))))
# 
# gpDataCleaned1 <- gpDataCleaned1 %>% 
#   select(-Reviews)

# Normalise Size
gpDataCleaned2 <- gpDataCleaned2 %>%
  mutate((Size = (Size - min(Size))/
            (max(Size) - min(Size))))

gpDataCleaned2 <- gpDataCleaned2 %>% 
  select(-Size)



# gpDataCleaned <- gpDataCleaned %>%
#   mutate(Reviews = ifelse(Reviews >= 0 & Reviews <= 1000,1,
#                            ifelse(Reviews >= 1001 & Reviews <= 2000,2,
#                                   ifelse(Reviews >= 2001 & Reviews <= 3000,3,
#                                          ifelse(Reviews >= 3001 & Reviews <= 4000,4,
#                                                 ifelse(Reviews >= 4001 & Reviews <= 5000,5,
#                                                        ifelse(Reviews >= 5001 & Reviews <= 6000,6,
#                                                               ifelse(Reviews >= 6001 & Reviews <= 7000,7,
#                                                                      ifelse(Reviews >= 7001 & Reviews <= 8000,8,
#                                                                             ifelse(Reviews >= 8001 & Reviews <= 9000,9,
#                                                                                    ifelse(Reviews >= 9001 & Reviews <= 10000,10,11))
#                                   )))))))))
# 
# # Dummy coding Reviews3
# gpdataDataFrame3 <- data.frame(gpDataCleaned1)
# gpdataDataFrame3 <- na.omit(gpDataCleaned1)
# gpDataCleaned1 <- as_tibble(dummy.data.frame(data = gpdataDataFrame3,
#                                              names = "Reviews"))
# print(gpDataCleaned1$Reviews)
# summary(gpDataCleaned1)
# change column names 
print(colnames(gpDataCleaned2))
gpDataFrameCleaned3 <- data.frame(gpDataCleaned2)
colnames(gpDataFrameCleaned3)[24] <- "Price"
colnames(gpDataFrameCleaned3)[25] <- "Size"
gpDataCleaned3 <- as_tibble(gpDataFrameCleaned3)
summary(gpDataCleaned3)

# Correlation
corrplot(cor(gpDataCleaned3 %>% keep(is.numeric)),
         method= "number",
         type = "lower",
         number.cex = 0.01)

round(cor(gpDataCleaned3 %>% keep(is.numeric)),2)

# Linear Regression Model for Reviews
gpDataCleaned2Model <- lm(data = gpDataCleaned3,
                          formula = Rating ~ Reviews2+ Reviews3+Reviews4+Reviews5+ Reviews6+
                          Reviews7+Reviews8+Reviews9+ Reviews10+Reviews11)
                                                
                                          
print(gpDataCleaned2Model)
summary(gpDataCleaned2Model)

# Linear Regression Model on Installs

gpDataCleaned4Model <- lm(data = gpDataCleaned3,
                          formula = Rating ~ Installs1+Installs3+Installs4+Installs5+
                            Installs6+Installs7+Installs8+Installs9+Installs10)
summary(gpDataCleaned4Model)


# multiple regression

gpDataCleaned5Model <- lm(data = gpDataCleaned3,
                          formula = Rating ~.)
summary(gpDataCleaned5Model)


gpCorrelation <- gpDataCleaned3 %>%
  select(-Type,-Size,-Price)
corrplot(cor(gpCorrelation %>% keep(is.numeric)),
         method= "number",
         type = "lower",
         number.cex = 0.01)
round(cor(gpCorrelation %>% keep(is.numeric)),2)

gpDataCleaned21Model <- lm(data = gpDataCleaned3,
                          formula = Rating ~ Installs1+Installs3+Installs4+Installs5+
                            Installs6+Installs7+Installs8+Installs9+Installs10+Reviews2+ Reviews3+Reviews4+Reviews5+ Reviews6+
                            Reviews7+Reviews8+Reviews9+ Reviews10+Reviews11)
summary(gpDataCleaned21Model)

x <- gpdata$Installs
y <- gpdata$Rating
plot(x,y, main="Scatterplot Example",
     xlab="Reviews ", ylab="Ratings ", pch=190, frame= FALSE)
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = gpdata), col = "blue")
