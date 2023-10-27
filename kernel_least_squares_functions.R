# Polynomial kernel ------------------------------------------------------------
poly_kernel_solver <- function(input_variables, output_variable, lambda = 0.1, b = 1) {
  # We must have lambda > 0
  if(lambda <= 0) {
    stop("Lambda must be greater than 0 in order to use the Woodbury identity!")
  }
  # Set up variables
  y <- output_variable
  X <- t(as.matrix(input_variables)) # convert each x_i into column vector
  # Add intercept
  X <- rbind(X, rep(1, ncol(X))) # d+1 x n matrix
  
  # Compute the polynomial kernel matrix (n x n)
  K <- (t(X) %*% X + 1)^b
  
  # Compute (K + lambda I)^-1 y^T
  K_lambdaI_yT <- solve(K + lambda*diag(ncol(X))) %*% as.matrix(y)
  
  # Create function for making predictions
  predict_poly_kernel <- function(new_input) {
    x_i <- as.matrix(t(new_input))
    # Add intercept
    x_i <- rbind(x_i, rep(1, ncol(x_i))) # d+1 x 1 matrix
    k <- (t(x_i) %*% X + 1)^b
    return(k %*% K_lambdaI_yT)
  }
  
  return(predict_poly_kernel)
}

# RBF kernel -------------------------------------------------------------------
rbf_kernel_solver <- function(input_variables, output_variable, lambda = 0.1) {
  # We must have lambda > 0
  if(lambda <= 0) {
    stop("Lambda must be greater than 0 in order to use the Woodbury identity!")
  }
  # Set up variables
  y <- output_variable
  X <- t(as.matrix(input_variables)) # convert each x_i into column vector
  # Add intercept
  X <- rbind(X, rep(1, ncol(X))) # d+1 x n matrix
  
  # Compute the polynomial kernel matrix (n x n)
  # NEED TO CHANGE
  K <- (t(X) %*% X + 1)^b
  
  # Compute (K + lambda I)^-1 y^T
  K_lambdaI_yT <- solve(K + lambda*diag(ncol(X))) %*% as.matrix(y)
  
  # Create function for making predictions
  predict_poly_kernel <- function(new_input) {
    x_i <- as.matrix(t(new_input))
    # Add intercept
    x_i <- rbind(x_i, rep(1, ncol(x_i))) # d+1 x 1 matrix
    # NEED TO CHANGE
    k <- (t(x_i) %*% X + 1)^b
    return(k %*% K_lambdaI_yT)
  }
  
  return(predict_poly_kernel)
}

