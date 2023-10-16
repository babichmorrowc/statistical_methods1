# Multivariate normal sample generating function -------------------------------
mvn_generator <- function(dimension, mu, Sigma, n_samples) {
  if(!is.positive.definite(Sigma)) {
    stop("Sigma is not positive definite!")
  }
  
  # Find the eigendecomposition of Sigma inverse
  ev_Sigma_inv <- eigen(solve(Sigma))
  U <- ev_Sigma_inv$vectors
  eigenvalues <- ev_Sigma_inv$values
  D <- diag(eigenvalues)
  if(!all.equal(U %*% D %*% solve(U), solve(Sigma))) {
    stop("Something has gone wrong in your eigendecomposition!")
  }
  
  x_samples <- matrix(0, nrow = dimension, ncol = n_samples)
  for (i in 1:n_samples) {
    y_i <- rnorm(n = dimension, mean = rep(0, dimension), sd = sqrt(1/eigenvalues))
    x_i <- U %*% y_i + mu
    x_samples[,i] <- x_i
  }
  
  return(x_samples)
  
}

# Ellipse functions ------------------------------------------------------------
# Functions from https://ms.mcmaster.ca/peter/s4c03/s4c03_0506/classnotes/DrawingEllipsesinR.pdf
ellipsem <- function (mu, amat, c2, npoints = 100, showcentre = T, ...)
{
  if (all(dim(amat) == c(2, 2))) {
    eamat <- eigen(amat)
    hlen <- sqrt(c2/eamat$val)
    theta <- angle(eamat$vec[1, 1], eamat$vec[2, 1])
    ellipse(hlen[1], hlen[2], theta, mu[1], mu[2], npoints = npoints,
            ...)
    if (showcentre)
      points(mu[1], mu[2], pch = 3)
  }
  invisible()
}

ellipse <- function (hlaxa = 1, hlaxb = 1, theta = 0, xc = 0, yc = 0, newplot = F,
                     npoints = 100, ...)
{
  a <- seq(0, 2 * pi, length = npoints + 1)
  x <- hlaxa * cos(a)
  y <- hlaxb * sin(a)
  alpha <- angle(x, y)
  rad <- sqrt(x^2 + y^2)
  xp <- rad * cos(alpha + theta) + xc
  yp <- rad * sin(alpha + theta) + yc
  if (newplot)
    plot(xp, yp, type = "l", ...)
  else lines(xp, yp, ...)
  invisible()
}

angle <- function (x, y)
{
  angle2 <- function(xy) {
    x <- xy[1]
    y <- xy[2]
    if (x > 0) {
      atan(y/x)
    }
    else {
      if (x < 0 & y != 0) {
        atan(y/x) + sign(y) * pi
      }
      else {
        if (x < 0 & y == 0) {
          pi
        }
        else {
          if (y != 0) {
            (sign(y) * pi)/2
          }
          else {
            NA
          }
        }
      }
    }
  }
  apply(cbind(x, y), 1, angle2)
}


