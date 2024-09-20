library(e1071)

# Load the dataset
dataset <- read.csv("melb_data.csv")

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