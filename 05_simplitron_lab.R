library(ggplot2)
library(dplyr)
source("perceptron_functions.R")

# Simplitron on random dataset -------------------------------------------------
# Set dimensions
d <- 2
n <- 200
#### With loop ####
# Create matrices X_pos and X_neg
X_pos <- matrix(rnorm(d*n) + 3, nrow = d)
X_pos <- rbind(X_pos, rep(1, n))
X_neg <- matrix(rnorm(d*n) - 3, nrow = d)
X_neg <- rbind(X_neg, rep(1, n))
# Set number of iterations
max_iteration <- 50
# Initialize eta_0
eta_0 <- 1
# Initialize random w
w_0 <- rnorm(d + 1)

w <- w_0
for (iter in 1:max_iteration) {
  # Different options for updating eta:
  eta <- eta_0 / iter
  # eta <- eta_0 / sqrt(iter)
  # eta <- eta_0 / iter^2
  
  for (i in 1:n) {
    # Check positive
    if(1 * w %*% X_pos[,i] <= 0) {
      w <- w + eta * 1 * X_pos[,i]
    }
    # Check negative
    if(-1 * w %*% X_neg[,i] <= 0) {
      w <- w + eta * -1 * X_neg[,i]
    }
  }
}

ggplot() +
  geom_point(aes(x = X_pos[1,], y = X_pos[2,]), color = "green") +
  geom_point(aes(x = X_neg[1,], y = X_neg[2,]), color = "red") +
  geom_abline(slope = -w[1] / w[2], intercept = -w[3] / w[2]) +
  labs(x = "Dim 1", y = "Dim 2")

#### With function ####
# Create matrices X_pos and X_neg
X_p <- matrix(rnorm(d*n) + 3, nrow = d)
X_n <- matrix(rnorm(d*n) - 3, nrow = d)
input_X <- cbind(X_p, X_n)
input_y <- c(rep(1, ncol(X_p)), rep(-1, ncol(X_n)))
input_Xy <- rbind(input_X, input_y)
# Shuffle the input data so we don't have all positive x all negative classes
random_cols <- sample(ncol(input_Xy))
input_shuffled <- input_Xy[,random_cols]
X_data <- input_shuffled[1:d,]
y_data <- input_shuffled[d+1,]

simp_coef <- simplitron(X = X_data, y = y_data, max_iteration = 50, eta_0 = 1, w_0 = rnorm(d + 1))

ggplot() +
  geom_point(aes(x = input_Xy[1,], y = input_Xy[2,], color = input_Xy[3,])) +
  geom_abline(slope = -simp_coef[1] / simp_coef[2], intercept = -simp_coef[3] / simp_coef[2]) +
  labs(x = "Dim 1", y = "Dim 2")





                 
                 