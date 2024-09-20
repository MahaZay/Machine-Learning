
# Libraries
library(dplyr)
library(mice)
library(caret)
library(tidyverse)
library(ggplot2)
library(visdat)
library(gridExtra)
library(corrplot)
library(stringr)
library(mgcv)

# Load the dataset
housing_data <- read.csv("Melbourne_housing_FULL.csv")

#  Display the structure of the dataset-
str(housing_data)

# Summary statistics of the dataset
summary(housing_data)

# Variable names
names(housing_data)

# Convert a character column to a factor
housing_data$Suburb <- as.factor(housing_data$Suburb)
housing_data$Type <- as.factor(housing_data$Type)
housing_data$Method <- as.factor(housing_data$Method)
housing_data$Regionname <- as.factor(housing_data$Regionname)

# Convert Distance and Propertycount to numeric
housing_data$Distance <- as.numeric(housing_data$Distance)
housing_data$Propertycount <- as.numeric(housing_data$Propertycount)


# Cleaning the dataset

# Check for missing values
sum(is.na(housing_data))

# Check for missing values in each column
missing_columns <- colSums(is.na(housing_data)) > 0
names(housing_data)[missing_columns]

# Create and display a missing value plot
missing_plot <- vis_miss(housing_data)
print(missing_plot)

# Drop rows with missing data
housing_data <- na.omit(housing_data)

# Drop unnecessary columns
housing_data <- housing_data %>%
  select(-SellerG, -CouncilArea, -Address, -Postcode)

# Detect and handel outliers 

# Calculate summary statistics for BuildingArea
summary(housing_data$BuildingArea)

# Calculate the lower and upper bounds for outliers using IQR
Q1 <- quantile(housing_data$BuildingArea, 0.25)
Q3 <- quantile(housing_data$BuildingArea, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Remove outliers from the BuildingArea
cleaned_data <- housing_data[housing_data$BuildingArea >= lower_bound & housing_data$BuildingArea <= upper_bound, ]

# Check summary statistics of BuildingArea in the cleaned dataset
summary(cleaned_data$BuildingArea)

# Check for zero values in the BuildingArea column
sum(housing_data$BuildingArea == 0, na.rm = TRUE)

# Calculate the mean of non-zero BuildingArea values
building_area_mean <- mean(housing_data$BuildingArea[housing_data$BuildingArea != 0], na.rm = TRUE)

# Replace zero values in BuildingArea with the mean
housing_data$BuildingArea[housing_data$BuildingArea == 0] <- building_area_mean


# ************************************Explanatory analysis ***************************

# Correlation

# Select numeric columns from the housing_data dataset
numeric_columns <- sapply(housing_data, is.numeric)
numeric_data <- housing_data[, numeric_columns]

# Calculate the correlation matrix
correlation <- cor(numeric_data)


# Create the correlation map
corrplot(correlation, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         diag = FALSE)

#####****************************GLM models***********************************************************************
##************************************************************************************************************
#*************************************************************************************************************
#************************************************************************************************************


# To help the agent evaluate whether the number of car spots are coherent to other criteria of the property
# this model will estimate the number of car spots based on other criterias such as number of room, bathroom or bedroom
# First let's check the distribution of response variable "Car" and the mean and variance of the variable
car<-housing_data$Car

ggplot(housing_data, aes(x = Car)) +
  geom_histogram(alpha=1, fill = "blue", color = "white",binwidth =0.9) +
  labs(x = "Car", y = "Frequency", title = "Distribution of Car spots")


mean(car)
var(car)

continuous <-select_if(housing_data, is.numeric)


# the plot shows a right skewed distribution which is a typical behavior of variable of type "count"
# For the estimation a GLM quasi poisson model will be implemented. only variable with a correlation coefficient
# above 0.2 with be used
glm.quasi.car <- glm(Car ~ log(Price)+Rooms+Bathroom+Bedroom2+BuildingArea+Distance , data = housing_data,
                     family = "quasipoisson")

# display summary to view results of the model
summary(glm.quasi.car)



glm.quasipoisson.car <- glm(Car ~ log(Price)+Bathroom+Bedroom2+BuildingArea+Distance , data = housing_data,
                            family = "quasipoisson")

print(summary(glm.quasipoisson.car))
# All variables have an influence on the response variable as p_value < 0.05 and We observe also an underdispersion situation with this dataset
# Interpretation of results
coef(glm.quasi.car)
exp(coef(glm.quasi.car))

# **"For a given property,
# increasing the log price by 6 or increase the price by 1million,would results in about 100% more car spot"
# Loop
set.seed(121)
##
r.squared.simple <- c()
##
for(i in 1:10^2){
  ## 1) prepare data
  
  train_indices <- createDataPartition(housing_data$Price, p = 0.7, list = FALSE)
  train_data <- housing_data[train_indices, ]
  test_data <- housing_data[-train_indices, ]
  ##
  ## quasi poisson model ##
  ##
  ## 2) fit the model with "train" data
  glm.quasi.car <- glm(Car ~ log(Price)Bathroom+Bedroom2+BuildingArea+Distance , data = train_data,
                       family = "quasipoisson")
  ##
  ## 3) make prediction on the test data
  predict.car <- predict(glm.quasi.car, newdata=test_data, type = "response")
  ##
  ## 4) compute R^2
  r.squared.simple[i] <- cor(predict.car, test_data$Car)^2
}

mean(r.squared.simple)

boxplot(r.squared.simple)

test_data.2 <- test_data
test_data.2["Car"] <- round(predict.car, digits = 0)

t1 <- ggplot(data = test_data, aes(x = Price, y = Car)) +
  geom_point(color = "blue")
t2 <- ggplot(data = test_data.2, aes(x = Price, y = Car)) +
  geom_point(color = "red")

# Arrange the subplots in a grid
grid.arrange(t1, t2)



# ***********************************Quasi binomial model*************************

# In order to sell property, it has to be classified and also that help to determine the right price based on its type.
# For this case we are not dealing with continuous variable but categorical variable as response
# 
# Let's explore how data are distributed

housing_data$Type[housing_data$Type == "u"] <- "t"

housing_data$Type.new  <- as.factor(housing_data$Type)
housing_data$Type.new=delevels(housing_data$Type.new,"u","t")

housing_data$Type.new <- droplevels(housing_data$Type.new)
table(housing_data$Type.new)


glm.quasi.type <- glm(Type.new ~ log(Price)+Rooms+Bathroom+Bedroom2+BuildingArea+Distance+Lattitude+Longtitude , data = housing_data, family = 'binomial')

print(summary(glm.quasi.type))


boxplot(log(Price)~Type.new, ylab="price", xlab= "type", col="light blue",data = housing_data)
boxplot(Distance~Type.new, ylab="distance", xlab= "type", col="light blue",data = housing_data)
boxplot(Bathroom~Type.new, ylab="bathroom", xlab= "type", col="light blue",data = housing_data)
boxplot(Bedroom2~Type.new, ylab="bedroom", xlab= "type", col="light blue",data = housing_data)



glm.quasi2.type <- glm(Type.new ~ log(Price)+Rooms+Bathroom+Bedroom2+Distance+Lattitude+Longtitude , data = housing_data, family = 'binomial')
print(summary(glm.quasi2.type))
coef(glm.quasi2.type)
print(exp(coef(glm.quasi2.type)))


# some coefficients are modified to get interpretable results

coef.price <- coef(glm.quasi2.type)["log(Price)"]*0.1
print(exp(coef.price))

coef.lat <- coef(glm.quasi2.type)["Lattitude"]*0.01
print(exp(coef.lat))

coef.long <- coef(glm.quasi2.type)["Longtitude"]*0.01
print(exp(coef.long))

print(contrasts(housing_data$Type.new))


#######prediction
## 1) prepare data

train_indices <- createDataPartition(housing_data$Type.new, p = 0.7, list = FALSE)
train_data_logi <- housing_data[train_indices, ]
test_data_logi <- housing_data[-train_indices, ]

## 2) fit the model with "train" data
glm.quasi2.type <- glm(Type.new ~ log(Price)+Rooms+Bathroom+Bedroom2+Distance+Lattitude+Longtitude , data = train_data_logi, family = 'binomial')

## 3) make prediction on the test data
predict.type <- predict(glm.quasi2.type, newdata=test_data_logi, type = "response")


# Converting from probability to actual output
test_data_logi$pred_type <- ifelse(predict.type >= 0.5, "t", "h")
# Generating the classification table
table_test <- table(test_data_logi$Type.new, test_data_logi$pred_type)
rownames(table_test) <- c("Obs. t","Obs. h")
colnames(table_test) <- c("Pred. t","Pred. h")

table_test

#Evaluate efficiency of the model

efficiency <- sum(diag(table_test))/sum(table_test)
efficiency
