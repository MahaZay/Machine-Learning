library(dplyr)
library(ggplot2)

# Load the dataset
dataset <- read.csv("melb_data.csv")

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
