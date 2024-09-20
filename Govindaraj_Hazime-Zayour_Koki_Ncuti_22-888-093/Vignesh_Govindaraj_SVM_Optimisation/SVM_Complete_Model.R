library(ggplot2)
library(caret)
library(e1071)

dataset <- read.csv("melb_data.csv")
selected_cols <- c("Distance", "Bedroom2", "Rooms", "Type", "Landsize", "Price")
data <- dataset[selected_cols]

# Split the dataset into training and testing sets
set.seed(125)
trainIndex <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]



# Business Question :Considering the financial capability, which geographical area (south, east, west, or north) would be affordable for me to buy a house in?


# Select the relevant columns

data <- dataset[,c("Price", "Rooms", "Type", "Distance", "Landsize", "Regionname")]

# Preprocess the dataset if needed (e.g., handling missing values, encoding categorical variables)

# Split the dataset into training and testing sets
set.seed(125)
trainIndex <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Create the regression model
regModel <- lm(Price ~ ., data = trainData)

# Make predictions on the test set
predictions <- predict(regModel, newdata = testData)



# Visualize the output
output <- data.frame(ActualPrice = testData$Price, PredictedPrice = predictions, Regionname = testData$Regionname)

ggplot(output, aes(x = Regionname, y = ActualPrice, fill = PredictedPrice)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Affordability of Houses by Region") +
  xlab("Region") +
  ylab("Price") +
  theme_bw()


# Calculate regression evaluation metrics
mse <- mean((predictions - testData$Price)^2)
rmse <- sqrt(mse)
mae <- mean(abs(predictions - testData$Price))
r_squared <- 1 - sum((testData$Price - predictions)^2) / sum((testData$Price - mean(testData$Price))^2)

# Print the regression evaluation metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared:", r_squared, "\n")




# Business Question :Among different types of properties, which type is currently being sold the most in the real estate market?


# Select the relevant columns
data <- dataset[, c("Type", "Rooms", "Price", "Distance", "Bedroom2")]

# Convert Type to a factor
data$Type <- factor(data$Type)

# Preprocess the data if needed

# Split the dataset into training and testing sets
set.seed(125)
trainIndex <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Create the SVM classification model
svmModel <- svm(Type ~ ., data = trainData, type = "C-classification")

# Make predictions on the test set
predictions <- predict(svmModel, newdata = testData)

# Evaluate the model
accuracy <- sum(predictions == testData$Type) / length(predictions)

# Create a table of predicted counts for each house type
predictionTable <- table(predictions)

# Create a bar plot of the predicted house types
barplot(predictionTable, main = "Distribution of House Types", xlab = "House Type", ylab = "Count")

# Print the predicted house types and their corresponding counts
cat("Predicted House Types:\n")
print(predictionTable)


#Business Question : Which suburbs are considered the best options for purchasing a property? 


# Preprocess the data

data <- dataset[, c("Suburb", "Price")]  
data <- na.omit(data)  

# Convert Suburb to a factor
data$Suburb <- factor(data$Suburb)

# Split the data into training and testing sets
set.seed(125)
trainIndex <- sample(1:nrow(data), 0.7 * nrow(data))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train the SVM model
svmModel <- svm(Price ~ Suburb, data = trainData)

# Predict house prices for the test data
predictions <- predict(svmModel, newdata = testData)

# Create a data frame with predicted prices and suburbs
predictionData <- data.frame(Suburb = as.character(testData$Suburb), PredictedPrice = predictions)

# Print the predicted house prices for each suburb
cat("Predicted House Prices by Suburb:\n")
print(predictionData)

# Calculate the average predicted price for each suburb
averagePrices <- aggregate(PredictedPrice ~ Suburb, predictionData, mean)

# Sort suburbs based on average predicted price in ascending order
sortedSuburbs <- averagePrices[order(averagePrices$PredictedPrice), ]

# Print the ranking of suburbs
cat("\nSuburbs Ranked by Predicted House Prices:\n")
for (i in 1:nrow(sortedSuburbs)) {
  cat(i, ": ", sortedSuburbs$Suburb[i], "\n")
}

# Visualize the predicted house prices
library(ggplot2)
ggplot(predictionData, aes(x = Suburb, y = PredictedPrice)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Suburb", y = "Predicted Price", title = "Predicted House Prices by Suburb") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Business Question : In which part of town are the properties generally more expensive?


# Preprocess the data
data <- dataset[, c("Suburb", "Price", "Lattitude", "Longtitude")]  
data <- na.omit(data) 

# Split the data into training and testing sets
set.seed(125)
trainIndex <- sample(1:nrow(data), 0.7 * nrow(data))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train the SVM model
svmModel <- svm(Price ~ Lattitude + Longtitude, data = trainData)

# Predict house prices for the entire dataset
predictions <- predict(svmModel, newdata = data)

# Combine the predictions with the original data
data$PredictedPrice <- predictions

# Find the suburb with the highest predicted price
expensiveSuburb <- data[data$PredictedPrice == max(data$PredictedPrice), "Suburb"]

# Visualize the predicted prices on a map

ggplot(data, aes(x = Longtitude, y = Lattitude, color = PredictedPrice)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude", title = "Predicted House Prices by Location") +
  theme_bw() +
  scale_color_gradient(low = "blue", high = "red") +
  guides(color = guide_legend(title = "Predicted Price")) +
  geom_text(data = data[data$Suburb == expensiveSuburb, ],
            aes(label = Suburb), hjust = -0.1, vjust = 0.5, size = 3, color = "black", fontface = "bold")

# Print the predicted prices for each suburb
cat("Predicted House Prices by Suburb:\n")
print(data[, c("Suburb", "PredictedPrice")])

# Print the suburb with the highest predicted price
cat("\nSuburb with the Highest Predicted Price:\n")
cat(expensiveSuburb, "\n")



#Business Question : What are the recommended locations for buying a 2-bedroom unit?



# Preprocess the data
data <- dataset[, c("Suburb", "Price", "Bedroom2", "Lattitude", "Longtitude")]  # Select relevant columns
data <- na.omit(data)  
# Filter data for 2-bedroom units
twoBedroomData <- data[data$Bedroom2 == 2, ]

# Split the data into training and testing sets
set.seed(125)
trainIndex <- sample(1:nrow(twoBedroomData), 0.7 * nrow(twoBedroomData))
trainData <- twoBedroomData[trainIndex, ]
testData <- twoBedroomData[-trainIndex, ]

# Train the SVM model
svmModel <- svm(Price ~ Lattitude + Longtitude, data = trainData)

# Predict prices for the entire dataset
predictions <- predict(svmModel, newdata = twoBedroomData)

# Combine the predictions with the original data
twoBedroomData$PredictedPrice <- predictions

# Find the suburb with the lowest predicted price
cheapestSuburb <- twoBedroomData[twoBedroomData$PredictedPrice == min(twoBedroomData$PredictedPrice), "Suburb"]

# Visualize the predicted prices on a map
library(ggplot2)
ggplot(twoBedroomData, aes(x = Longtitude, y = Lattitude, color = PredictedPrice)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude", title = "Predicted Prices for 2-Bedroom Units") +
  theme_bw() +
  scale_color_gradient(low = "blue", high = "red") +
  guides(color = guide_legend(title = "Predicted Price")) +
  geom_text(data = twoBedroomData[twoBedroomData$Suburb == cheapestSuburb, ],
            aes(label = Suburb), hjust = -0.1, vjust = 0.5, size = 3, color = "black", fontface = "bold")

cat("Predicted Prices for 2-Bedroom Units by Suburb:\n")
print(twoBedroomData[, c("Suburb", "PredictedPrice")])

# Print the suburb with the lowest predicted price
cat("\nSuburb with the Lowest Predicted Price for 2-Bedroom Units:\n")
cat(cheapestSuburb, "\n")