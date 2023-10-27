library(pracma)

# Generate data ----------------------------------------------------------------
set.seed(48)
# x_i uniformly generated (0,2)
x <- runif(200, min = 0, max = 2)
epsilon <- rnorm(200, mean = 0, sd = 0.64)
y <- exp(1.5*x - 1) + epsilon
generated_data <- data.frame(x_val = x, y_val = y)
plot(y ~ x, data = generated_data)

# Calculate CV error with different values of lambda ---------------------------
# Use 5-fold cross-validation
# Randomly shuffle the data
generated_data <-  generated_data[sample(nrow(generated_data)),]
folds <- cut(seq(1,nrow(generated_data)), breaks = 5, labels = FALSE)
# Set up feature space
lambdas <- logspace(-3, 1, 10)
degrees <- 4:6
# Empty dataframe to store cross-validation error
cross_val_results <- data.frame(lambda = NA, b = NA, cross_val_error = NA)
# Loop over all combinations of lambda and b
for (l in 1:length(lambdas)) {
  for (b in degrees) {
    cv_error <- c()
    for(i in 1:5) {
      test_data <- generated_data[which(folds == i),]
      train_data <- generated_data[which(folds != i),]
      fit_i <- least_squares_solver(train_data$x_val, train_data$y_val, lambda = lambdas[l], b = b)
      pred_i <- least_squares_predict(test_data$x_val, fit_i, b = b)
      error_i <- sum_squared_errors(pred_i, test_data$y_val)
      cv_error <- c(cv_error, error_i)
    }
    overall_error <- mean(cv_error)
    cross_val_results <- rbind(cross_val_results, c(lambdas[l], b, overall_error))
  }
}

# Find the values of lambda and b with the lowest CV error
cross_val_results[cross_val_results$cross_val_error == min(cross_val_results$cross_val_error, na.rm = T),]

# Calculate the predictive probability distribution ----------------------------


