library(matrixcalc)
library(misc3d)

# Generate samples from a multi-variate normal distribution --------------------
set.seed(1)
# Set dimension
d <- 3
# Generate mean mu
# mu <- runif(n = d, min = 0, max = 1)
mu <- matrix(c(1,2,3), ncol = 1)
# Generate covariance matrix sigma
# A <- matrix(runif(d^2), ncol = 10)
# Sigma <- t(A) %*% A            
Sigma <- matrix(c(2,-1,0,-1,2,-1,0,-1,2), ncol = 3)
is.positive.definite(Sigma) # Check if positive definite
# Set n samples to generate
n <- 6

# Generate n_samples samples of dimension d with mean mu and covariance matrix Sigma:
mvn_generator(dimension = d, mu = mu, Sigma = Sigma, n_samples = n)

# Run to try to mimic example plot in lab
plot_mu <- matrix(c(2,1), ncol = 1)
plot_Sigma <- matrix(c(1,0.5,0.5,1), ncol = 2)
sample <- mvn_generator(dimension = 2, mu = plot_mu, Sigma = plot_Sigma, n_samples = 10000)
sample_t <- t(sample)
sample_t_dataframe <- as.data.frame(sample_t)
sample_t_dataframe$color <- "black"
plot(sample_t[,2] ~ sample_t[,1])

# Confidence intervals ---------------------------------------------------------
# s_function <- function(x) t(x - plot_mu) %*% solve(plot_Sigma) %*% (x - plot_mu)
ellipsem(mu = plot_mu, amat = solve(plot_Sigma), c2 = 6, col = "red")

count_6 <- 0
for (i in 1:nrow(sample_t)) {
  s_value <- t(sample_t[i,] - plot_mu) %*% solve(plot_Sigma) %*% (sample_t[i,] - plot_mu)
  if(s_value < 6) {
    count_6 <- count_6 + 1
    sample_t_dataframe$color[i] <- "red"
  }
}
count_6
count_6 / nrow(sample_t)
# Should be ~ 0.95 !

plot(sample_t_dataframe$V2 ~ sample_t_dataframe$V1, col = sample_t_dataframe$color)
ellipsem(mu = plot_mu, amat = solve(plot_Sigma), c2 = 6, col = "green")





