# Load the required libraries
library(tidyverse)
library(caret)
library(neuralnet)
library(dplyr)

# Load the dataset
housing_data <- read.csv("Melbourne_housing_FULL.csv")

# Drop rows with missing data
housing_data <- na.omit(housing_data)

# Drop unnecessary columns
housing_data <- housing_data %>%
  select(-SellerG, -CouncilArea, -Address, -Postcode)


# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(housing_data$Price, p = 0.8, list = FALSE)
trainData <- housing_data[trainIndex, ]
testData <- housing_data[-trainIndex, ]

# Convert a character column to a factor
trainData$Suburb <- as.factor(trainData$Suburb)
trainData$Type <- as.factor(trainData$Type)
trainData$Method <- as.factor(trainData$Method)
trainData$Regionname <- as.factor(trainData$Regionname)

# Convert Distance and Propertycount to numeric
trainData$Distance <- as.numeric(trainData$Distance)
trainData$Propertycount <- as.numeric(trainData$Propertycount)

# Correct the column names in numericCols, if needed
numericCols <- c("Rooms", "Distance", "Bedroom2", "Landsize")

# Subset the numeric columns in trainData
numericTrainData <- trainData[, numericCols, drop = FALSE]

# Perform preprocessing and scaling
preprocessParams <- preProcess(numericTrainData, method = c("center", "scale"))
scaledNumericTrainData <- predict(preprocessParams, numericTrainData)

# Update the scaled numeric columns in trainData
trainData[, numericCols] <- scaledNumericTrainData

# Encode the categorical variables
trainData <- dummyVars(" ~ .", data = trainData) %>%
  predict(trainData) %>%
  as.data.frame()

# Fit the artificial neural network model
nnModel <- neuralnet(Price ~ Rooms + Distance + Bedroom2 + Landsize, data = trainData, hidden = c(5, 3), linear.output = TRUE, stepmax = 1e7)
plot(nnModel)

# Fit the artificial neural network model
nnModel <- neuralnet(Price ~ Rooms + Distance + Bedroom2 + Landsize, data = trainData, hidden = c(5, 3), linear.output = TRUE, stepmax = 1e7)

numericCols <- intersect(numericCols, colnames(testData))
# Preprocess the test data using the same preprocessing parameters
scaledNumericTestData <- predict(preprocessParams, testData[, numericCols])
tratestData[, numericCols] <- scaledNumericTestData
testData <- dummyVars(" ~ .", data = testData) %>%
  predict(testData) %>%
  as.data.frame()

# Prepare the input data for prediction on the test dataset
input_test_data <- testData[, colnames(trainData) %in% colnames(testData)]

# Predict the prices using the neural network model
predicted_prices <- compute(nnModel, input_test_data)$net.result

# Calculate the RMSE
rmse <- sqrt(mean((testData$Price - predicted_prices)^2))

# Print the RMSE
print(paste("RMSE:", rmse))

# Calculate the accuracy
accuracy <- sum(round(predicted_prices) == testData$Price) / nrow(testData) * 100

# Print the accuracy
print(paste("Accuracy:", accuracy, "%"))

# Save the RMSE and accuracy using RDS
model_evaluation <- list(RMSE = rmse, Accuracy = accuracy)
saveRDS(model_evaluation, file = "model_evaluation.rds")

# Now, let's proceed to answer the research questions using the trained neural network model.

# 1. What a home might worth? client or real estate agent based on distance, bedroom, rooms, type, landsize

# Example input values
input_data <- data.frame(Rooms = 3, Distance = 10, Bedroom2 = 2, Landsize = 500, Type_t = 1, Type_u = 0, Type_h = 0, Type_o = 0)

# Scale the input data using the preprocessing parameters
scaled_input_data <- predict(preprocessParams, input_data)

# Predict the home price using the neural network model
predicted_price <- compute(nnModel, scaled_input_data)$net.result

# Print the predicted price
predicted_price

# 2. Where can I afford to buy a house based on my budget: south, east, west, or north?

# Example input values
# Example input values
input_data <- data.frame(Rooms = 3, Distance = 10, Bedroom2 = 2, Landsize = 500)

# Scale the input data using the preprocessing parameters
scaled_input_data <- predict(preprocessParams, input_data)

# Define budget range
budget_range <- seq(500000, 2000000, by = 50000)

# Create an empty list to store affordable regions for each budget
affordable_regions <- list()

# Iterate over each budget
for (budget in budget_range) {
  # Predict the prices for different regions using the neural network model
  predicted_prices <- sapply(regions, function(region) {
    input_data$Regionname <- as.factor(region)
    scaled_input_data <- predict(preprocessParams, input_data)
    compute(nnModel, scaled_input_data)$net.result
  })
  
  # Compare predicted prices and choose the affordable region
  affordable_region <- regions[which(predicted_prices <= budget)]
  
  if (length(affordable_region) == 0) {
    affordable_region <- "None"
  }
  
  # Store the affordable regions for the current budget
  affordable_regions[[as.character(budget)]] <- affordable_region
}

# Print the affordable regions for each budget
for (budget in budget_range) {
  affordable_region <- affordable_regions[[as.character(budget)]]
  print(paste("Budget:", budget, "Affordable Region(s):", paste(affordable_region, collapse = ", ")))
}


# 3. Which type is being mostly sold?

# Predict property type using the neural network model
property_types <- c("Type_t", "Type_u", "Type_h", "Type_o")
predicted_counts <- sapply(property_types, function(property_type) {
  input_data <- data.frame(Rooms = 3, Distance = 10, Bedroom2 = 2, Landsize = 500, Type_t = 0, Type_u = 0, Type_h = 0, Type_o = 0)
  input_data[, property_type] <- 1
  scaled_input_data <- predict(preprocessParams, input_data)
  compute(nnModel, scaled_input_data)$net.result
})

# Find the most sold property type
most_sold_type <- property_types[which.max(predicted_counts)]

# Print the most sold type
most_sold_type

# 4. Which suburbs are the best to buy in?

# Example input values
input_data <- data.frame(Rooms = 3, Distance = 10, Bedroom2 = 2, Landsize = 500)

# Scale the input data using the preprocessing parameters
scaled_input_data <- predict(preprocessParams, input_data)

# Predict prices for different suburbs using the neural network model
suburbs <- unique(housing_data$Suburb)
predicted_prices <- sapply(suburbs, function(suburb) {
  input_data$Suburb <- as.factor(suburb)
  scaled_input_data <- predict(preprocessParams, input_data)
  compute(nnModel, scaled_input_data)$net.result
})

# Sort the suburbs based on predicted prices
sorted_suburbs <- suburbs[order(predicted_prices)]

# Print the best suburbs to buy in (e.g., top 5)
best_suburbs <- sorted_suburbs[1:5]
best_suburbs

# 5. Where's the expensive side of town?

# Predict prices for different regions using the neural network model
regions <- unique(housing_data$Regionname)
predicted_prices <- sapply(regions, function(region) {
  input_data <- data.frame(Rooms = 3, Distance = 10, Bedroom2 = 2, Landsize = 500, Regionname = as.factor(region))
  scaled_input_data <- predict(preprocessParams, input_data)
  compute(nnModel, scaled_input_data)$net.result
})

# Identify the region with the highest predicted prices
expensive_region <- regions[which.max(predicted_prices)]

# Print the expensive side of town
expensive_region

# 6. Where should I buy a 2 bedroom unit?

# Predict prices for 2 bedroom units in different suburbs using the neural network model
# Filter for 2 bedroom units
two_bedroom_data <- housing_data[housing_data$Rooms == 2, ]

if (nrow(two_bedroom_data) == 0) {
  # No 2-bedroom units found
  recommended_suburbs <- character(0)
} else {
  # Predict prices for 2 bedroom units in different suburbs using the neural network model
  suburbs <- unique(two_bedroom_data$Suburb)
  predicted_prices <- sapply(suburbs, function(suburb) {
    input_data <- data.frame(Rooms = 2, Distance = 10, Bedroom2 = 2, Landsize = 500, Suburb = as.factor(suburb))
    scaled_input_data <- predict(preprocessParams, input_data)
    compute(nnModel, scaled_input_data)$net.result
  })
  
  # Combine predicted prices with suburbs
  predicted_prices_df <- data.frame(Suburb = suburbs, PredictedPrice = predicted_prices)
  
  # Sort the 2 bedroom units based on predicted prices
  sorted_2bedroom_units <- predicted_prices_df[order(predicted_prices_df$PredictedPrice), ]
  
  # Print the recommended suburbs to buy a 2 bedroom unit (e.g., top 5)
  recommended_suburbs <- sorted_2bedroom_units$Suburb[1:5]
}

# Print the recommended suburbs
recommended_suburbs



