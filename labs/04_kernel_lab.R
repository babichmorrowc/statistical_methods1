# Load packages
library(readr)

# Source functions
source("least_squares_functions.R")
source("kernel_least_squares_functions.R")

# Import data
prostate_data <- read_csv("prostate_data.csv")

# Split data into training and test data
train_data <- prostate_data[prostate_data$train == TRUE,]
test_data <- prostate_data[!prostate_data$train,]

# Linear kernel ----------------------------------------------------------------
linear_kernel <- poly_kernel_solver(input_variables = train_data[,1:8],
                                    output_variable = train_data$lpsa,
                                    lambda = 0.1,
                                    b = 1)
preds <- linear_kernel(test_data[,1:8])
# Calculate cross-validation error
cross_val_error <- sum_squared_errors(preds, test_data$lpsa)
cross_val_error

# Polynomial kernel ------------------------------------------------------------
poly_kernel <- poly_kernel_solver(input_variables = train_data[,1:8],
                                    output_variable = train_data$lpsa,
                                    lambda = 0.8,
                                    b = 2)
poly_preds <- poly_kernel(test_data[,1:8])
# Calculate cross-validation error
poly_cross_val_error <- sum_squared_errors(poly_preds, test_data$lpsa)
poly_cross_val_error



