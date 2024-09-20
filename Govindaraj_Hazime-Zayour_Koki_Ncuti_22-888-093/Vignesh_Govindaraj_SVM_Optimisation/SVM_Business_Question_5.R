
library(e1071)
library(ggplot2)

dataset = read.csv("melb_data.csv")
# Preprocess the data
data <- dataset[, c("Suburb", "Price", "Lattitude", "Longtitude")]  # Select relevant columns
data <- na.omit(data)  # Remove rows with missing values

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
