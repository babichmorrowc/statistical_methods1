simplitron <- function(X, y, max_iteration, eta_0, w_0, eta_fn = "linear") {
  if(!(eta_fn %in% c("linear", "quadratic", "sqrt"))) {
    stop("eta_fn must be one of linear, quadratic, or sqrt")
  }
  
  n <- ncol(X)
  d <- nrow(X)
  # Add row of 1s to X
  X <- rbind(X, rep(1,n))
  
  w <- w_0
  for (iter in 1:max_iteration) {
    eta <- case_when(
      eta_fn == "linear" ~ eta_0 / iter,
      eta_fn == "quadratic" ~ eta_0 / iter^2,
      eta_fn == "sqrt" ~ eta_0 / sqrt(iter)
    )
    
    for (i in 1:n) {
      if(y[i] * w %*% X_pos[,i] <= 0) {
        w <- w + eta * 1 * X_pos[,i]
      }
    }
  }
  
  return(w)
}
