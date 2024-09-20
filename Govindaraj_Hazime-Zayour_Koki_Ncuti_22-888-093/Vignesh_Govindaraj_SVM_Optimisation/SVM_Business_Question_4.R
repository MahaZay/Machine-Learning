library(e1071)

# Preprocess the data
dataset <- read.csv('melb_data.csv')
data <- dataset[, c("Suburb", "Price")]  # Select relevant columns
data <- na.omit(data)  # Remove rows with missing values

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
