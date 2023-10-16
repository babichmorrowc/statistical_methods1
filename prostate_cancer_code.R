# Load packages
library(readr)

# Source functions
source("least_squares_functions.R")

# Import data
prostate_data <- read_csv("prostate_data.csv")

# Split data into training and test data
train_data <- prostate_data[prostate_data$train == TRUE,]
test_data <- prostate_data[!prostate_data$train,]

# Model using all features -----------------------------------------------------
# Fit linear least squares on the training data
train_least_squares <- least_squares_solver(train_data[,1:8], train_data$lpsa)

# Apply the model to the testing data
preds <- least_squares_predict(test_data[,1:8], train_least_squares)
# Calculate cross-validation error
cross_val_error <- sum_squared_errors(preds, test_data$lpsa)
cross_val_error

# Model removing one feature ---------------------------------------------------
# Fit linear least squares on the training data
train_least_squares_drop1 <- least_squares_solver(train_data[,2:8], train_data$lpsa)
# Apply the model to the testing data
preds_drop1 <- least_squares_predict(test_data[,2:8], train_least_squares_drop1)
# Calculate cross-validation error
cross_val_error_drop1 <- sum_squared_errors(preds_drop1, test_data$lpsa)
cross_val_error_drop1

cross_val_data <- data.frame(feature_dropped = NA, cross_val_error = NA)
for (i in 1:8) {
  feature_i <- colnames(train_data)[i]
  train_data_drop1 <- train_data[,-i]
  test_data_drop1 <- test_data[,-i]
  # Fit linear least squares on the training data
  train_least_squares_drop1 <- least_squares_solver(train_data_drop1[,1:(ncol(train_data_drop1)-2)], train_data$lpsa)
  # Apply the model to the testing data
  preds_drop1 <- least_squares_predict(test_data_drop1[,1:(ncol(train_data_drop1)-2)], train_least_squares_drop1)
  # Calculate cross-validation error
  cross_val_error_drop1 <- sum_squared_errors(preds_drop1, test_data$lpsa)
  cross_val_data[i,] <- c(feature_i, cross_val_error_drop1)
}
View(cross_val_data)

# Compare to lm results --------------------------------------------------------
lm_model <- lm(lpsa ~ ., data = train_data[,1:9])
summary(lm_model)
predict(lm_model, test_data)
