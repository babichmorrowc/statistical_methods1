# Function to calculate the coefficients w_LS
least_squares_solver <- function(input_variables, output_variable, lambda = 0, b = 1) {
  # Set up variables
  y <- output_variable
  phi_X <- t(as.matrix(input_variables^b)) # convert each x_i into column vector
  power <- b - 1
  while(power > 0) {
    phi_X <- rbind(phi_X, t(as.matrix(input_variables^power)))
    power <- power - 1
  }
  # Add intercept
  phi_X <- rbind(phi_X, rep(1, ncol(phi_X)))
  
  # Compute w_LS
  w_LS <- solve(phi_X %*% t(phi_X) + lambda*diag(nrow(phi_X))) %*% phi_X %*% as.matrix(y)
  return(w_LS)
}

# Function to apply coefficients to a new set of data
least_squares_predict <- function(input_variables, coefficients, b = 1) {
  # Set up variables
  phi_X <- t(as.matrix(input_variables^b)) # convert each x_i into column vector
  power <- b - 1
  while(power > 0) {
    phi_X <- rbind(phi_X, t(as.matrix(input_variables^power)))
    power <- power - 1
  }
  # Add intercept
  phi_X <- rbind(phi_X, rep(1, ncol(phi_X)))
  
  preds <- t(as.matrix(coefficients)) %*% phi_X
  return(preds)
}

# Function to calculate sum of squared errors
sum_squared_errors <- function(prediction, actual) {
  squared_error <- (actual - prediction)^2
  return(sum(squared_error))
}
