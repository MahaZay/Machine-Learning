
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


# Explanatory analysis 

#Histogram of Price:
ggplot(housing_data, aes(x = Price)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  labs(x = "Price", y = "Frequency", title = "Distribution of Price")

# View the "Regionname" column before abbreviating
head(housing_data$Regionname)

# Abbreviate Regionnames
housing_data$Regionname <- str_replace_all(housing_data$Regionname, c('Northern Metropolitan'='N Metro',
                                                                      'Western Metropolitan'='W Metro', 
                                                                      'Southern Metropolitan'='S Metro', 
                                                                      'Eastern Metropolitan'='E Metro', 
                                                                      'South-Eastern Metropolitan'= 'SE Metro', 
                                                                      'Northern Victoria'='N Vic',
                                                                      'Eastern Victoria'='E Vic',
                                                                      'Western Victoria'='W Vic'))
# Boxplot of Price by Region:
ggplot(housing_data, aes(x = Regionname, y = Price)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Regionname", y = "Price", title = "Price Variation by Region")

# Graphical Analysis (categorical)
# Set the theme
theme_set(theme_minimal())
# Create the subplots
p1 <- ggplot(data = housing_data, aes(x = Type, y = Price)) +
  geom_boxplot() +
  labs(x = "Type", y = "Price") +
  ggtitle("Type v Price")

p2 <- ggplot(data = housing_data, aes(x = Method, y = Price)) +
  geom_boxplot() +
  labs(x = "Method", y = "Price") +
  ggtitle("Method v Price")

p3 <- ggplot(data = housing_data, aes(x = Regionname, y = Price)) +
  geom_boxplot() +
  labs(x = "Regionname", y = "Price") +
  ggtitle("Region Name v Price")

# Arrange the subplots in a grid
grid.arrange(p1, p2, p3, ncol = 2, nrow = 2)


# Graphical Analysis (Numeric Features)

# Set the theme
theme_set(theme_minimal())
# Create the subplots
p1 <- ggplot(data = housing_data, aes(x = Rooms, y = Price)) +
  geom_point(color = "blue") +
  xlab("Rooms") +
  ylab("Price") +
  ggtitle("Rooms v Price")

p2 <- ggplot(data = housing_data, aes(x = Distance, y = Price)) +
  geom_point(color = "blue") +
  xlab("Distance") +
  ylab("Price") +
  ggtitle("Distance v Price")+
  scale_x_continuous(breaks = seq(0, max(housing_data$Distance), 10))

p3 <- ggplot(data = housing_data, aes(x = Bathroom, y = Price)) +
  geom_point(color = "blue") +
  xlab("Bathroom") +
  ylab("Price") +
  ggtitle("Bathroom v Price")

p4 <- ggplot(data = housing_data, aes(x = Car, y = Price)) +
  geom_point(color = "blue") +
  xlab("Car") +
  ylab("Price") +
  ggtitle("Car v Price")

p5 <- ggplot(data = housing_data, aes(x = Landsize, y = Price)) +
  geom_point(color = "blue") +
  xlab("Landsize") +
  ylab("Price") +
  ggtitle("Landsize v Price")

p6 <- ggplot(data = housing_data, aes(x = BuildingArea, y = Price)) +
  geom_point(color = "blue") +
  xlab("BuildingArea") +
  ylab("Price") +
  ggtitle("BuildingArea v Price")

p7 <- ggplot(data = housing_data, aes(x = YearBuilt, y = Price)) +
  geom_point(color = "blue") +
  xlab("YearBuilt") +
  ylab("Price") +
  ggtitle("YearBuilt v Price") +
  scale_x_continuous(breaks = seq(1850, 2020, by = 10), labels = seq(1850, 2020, by = 10))

p8 <- ggplot(data = housing_data, aes(x = Propertycount, y = Price)) +
  geom_point(color = "blue") +
  xlab("Propertycount") +
  ylab("Price") +
  ggtitle("Propertycount v Price")+ 
  scale_x_continuous(breaks = seq(0, max(housing_data$Distance), 5000))

# Arrange the subplots in a grid
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 2, nrow = 4)



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


# Split the data into training and testing sets
set.seed(123)  # Set a seed for reproducibility
train_indices <- createDataPartition(housing_data$Price, p = 0.7, list = FALSE)
train_data <- housing_data[train_indices, ]
test_data <- housing_data[-train_indices, ]

# Fit a simple linear model
lm_housing <- lm(Price ~ BuildingArea, data = train_data)

# coefficient Regression
coef(lm_housing)

# Summary
summary(lm_housing)

# visualise Simple Regression
ggplot(train_data, aes(x = BuildingArea, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Building Area", y = "Price", title = "Price vs. Building Area") +
  scale_x_continuous(limits = c(0, 800))


# Fit multiple linear regression model
lm_housing.2 <- lm(Price ~ BuildingArea + YearBuilt, data = train_data)

# Summary
summary(lm_housing.2)

# Coefficients
coef(lm_housing.2)

# Create scatter plot for BuildingArea
plot_building_area <- ggplot(train_data, aes(x = BuildingArea, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Building Area", y = "Price", title = "Price vs. Building Area")

# Create scatter plot for YearBuilt
plot_year_built <- ggplot(train_data, aes(x = YearBuilt, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year Built", y = "Price", title = "Price vs. Year Built") +
  scale_x_continuous(limits = c(1850, 2016))

# Combine the plots
combined_plots <- grid.arrange(plot_building_area, plot_year_built, nrow = 1)

# Display the combined plot
print(combined_plots)


# Fit multiple linear regression model with interaction
lm_housing.3 <- lm(Price ~ BuildingArea * YearBuilt, data = train_data)
summary(lm_housing.3)



# Fit multiple linear regression model with additional predictors

lm_housing.4 <- lm(Price ~ BuildingArea +  YearBuilt + Rooms + Distance + Bathroom + Car + Landsize +  Propertycount, data = train_data)

# coefficient Regression
coef(lm_housing.4)

# Summary
summary(lm_housing.4)


# Create scatter plots with regression line for each predictor variable
scatter_plots <- lapply(names(lm_housing.4$model)[-1], function(var) {
  ggplot(lm_housing.4$model, aes_string(x = var, y = "Price")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    xlab(var) +
    ylab("Price")
})

# Arrange the scatter plots in a grid layout
grid_plot <- do.call(gridExtra::grid.arrange, scatter_plots)

# Display the grid of scatter plots
print(grid_plot)




# Calculate R-squared 
# model with no interaction 
formula(lm_housing.2)
summary(lm_housing.2)$r.squared

formula(lm_housing.4)
summary(lm_housing.4)$r.squared

# model with interaction 
formula(lm_housing.3)
summary(lm_housing.3)$r.squared


# Calculate adjusted R-squared
summary(lm_housing.2)$adj.r.squared
summary(lm_housing.3)$adj.r.squared
summary(lm_housing.4)$adj.r.squared

# Fitted values
fitted_values <- fitted(lm_housing)

str(fitted_values)
head(fitted_values)


# Fitted values plot
plot(Price ~ BuildingArea, data = train_data,
     main = "Model 'lm_housing'",
     col = "darkgray")

points(fitted_values ~ BuildingArea, 
       col = "purple",
       pch = 19,
       data = train_data)

abline(lm_housing, col = "black")

# Residuals
residuals <- resid(lm_housing)

length(residuals)
head(residuals)


set.seed(20) ## for reproducibility
id <- sample(x = 1:144, size = 5)
residuals[id]
fitted_values[id]


# Residuals plot
plot(Price ~ BuildingArea, data = train_data,
     main = "Model 'lm_housing'",
     col = "lightgray")

abline(lm_housing)

points(Price ~ BuildingArea, data = train_data[id, ],
       col = "red")

segments(x0 = train_data[id, "BuildingArea"], x1 = train_data[id, "BuildingArea"],
         y0 = fitted_values[id], y1 = train_data[id, "Price"],
         col = "blue")



# Create new data for prediction
new_data <- data.frame(
  BuildingArea = c(120, 200, 250),
  YearBuilt = c(1990, 2005, 2010),
  Rooms = c(3, 4, 5),
  Distance = c(10, 15, 20),
  Bathroom = c(2, 2.5, 3),
  Car = c(1, 2, 2),
  Landsize = c(500, 600, 700),
  Propertycount = c(5000, 6000, 7000))


# Predict using the linear model
predictions <- predict(lm_housing.4, newdata = new_data)
print(predictions)


# Display predictions
plot(Price ~ BuildingArea, 
     data = train_data,
     xlim = c(50, 600),
     ylim = c(900000, 1600000),
     main = "Predicted vs. Observed Prices")
abline(lm_housing)
##
points(x = new_data$BuildingArea,
       y = predictions, 
       col = "purple",
       pch = 19, cex = 1.5)

# Compute prediction confidence intervals
prediction_ci <- predict(lm_housing.4, newdata = new_data, interval = "prediction")

# Display the predictions and confidence intervals
print(data.frame(predictions, prediction_ci))


# Visualize target Price 
hist(train_data$Price, main = "Histogram of Target Price")

# Apply log transformation
train_data$log_Price <- log(train_data$Price)

# After transformation
hist(train_data$log_Price, main = "Histogram of Log-Transformed Target Price")


# Fit the GAM model
gam_model <- gam(log_Price  ~ s(BuildingArea) + s(YearBuilt) + s(Rooms) + s(Distance)+ Bathroom + Car + Landsize + Propertycount,
             data = train_data)

# Print the summary of the GAM model
summary(gam_model)

# Plot the GAM model

plot(gam_model, residuals = TRUE, pages = 1, shade = TRUE)
