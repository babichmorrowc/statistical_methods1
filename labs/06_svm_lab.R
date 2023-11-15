library(ggplot2)
library(dplyr)
source("perceptron_functions.R")

# Set dimensions
d <- 2
n <- 200
# Make random data
offset <- 4
# Create matrices X_pos and X_neg
X_p <- matrix(rnorm(d*n) + offset, nrow = d)
X_n <- matrix(rnorm(d*n) - offset, nrow = d)
input_X <- cbind(X_p, X_n)
input_y <- c(rep(1, ncol(X_p)), rep(-1, ncol(X_n)))
input_Xy <- rbind(input_X, input_y)
# Shuffle the input data so we don't have all positive x all negative classes
random_cols <- sample(ncol(input_Xy))
input_shuffled <- input_Xy[,random_cols]
X_data <- input_shuffled[1:d,]
y_data <- input_shuffled[d+1,]

simp_coef <- simplitron(X = X_data, y = y_data, max_iteration = 50, eta_0 = 1, w_0 = rnorm(d + 1))
svm_coef_c0.1 <- svm_perceptron(X = X_data, y = y_data, max_iteration = 50, eta_0 = 1, w_0 = rnorm(d + 1), c = 0.1)
svm_coef_c0.01 <- svm_perceptron(X = X_data, y = y_data, max_iteration = 50, eta_0 = 1, w_0 = rnorm(d + 1), c = 0.01)
svm_coef_c0.001 <- svm_perceptron(X = X_data, y = y_data, max_iteration = 50, eta_0 = 1, w_0 = rnorm(d + 1), c = 0.001)

ggplot() +
  geom_point(aes(x = input_Xy[1,], y = input_Xy[2,], color = as.factor(input_Xy[3,]))) +
  geom_abline(slope = -svm_coef_c0.1[1] / svm_coef_c0.1[2], intercept = -svm_coef_c0.1[3] / svm_coef_c0.1[2], color = "darkblue") +
  geom_abline(slope = -svm_coef_c0.01[1] / svm_coef_c0.01[2], intercept = -svm_coef_c0.01[3] / svm_coef_c0.01[2], color = "blue") +
  geom_abline(slope = -svm_coef_c0.001[1] / svm_coef_c0.001[2], intercept = -svm_coef_c0.001[3] / svm_coef_c0.001[2], color = "lightblue") +
  geom_abline(slope = -simp_coef[1] / simp_coef[2], intercept = -simp_coef[3] / simp_coef[2], color = "red") +
  labs(x = "Dim 1", y = "Dim 2", color = "y")
