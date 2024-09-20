
library(e1071)

dataset = read.csv("melb_data.csv")

# Preprocess the data
data <- dataset[, c("Suburb", "Price", "Bedroom2", "Lattitude", "Longtitude")]  # Select relevant columns
data <- na.omit(data)  # Remove rows with missing values

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