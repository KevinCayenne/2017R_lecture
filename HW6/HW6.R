library(scatterplot3d)

newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  x <- x0       # 先把 x assign 為起始值 x0
  fx <- ftn(x)
  iter <- 0
  while ((abs(fx[1]) > tol) & (iter < max.iter)) {
    x <- x - fx[1]/fx[2]
    fx <- ftn(x)
    iter <- iter + 1
    cat("At iteration", iter, "value of x is:", x, "\n")
  }
  if (abs(fx[1]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else {
    cat("Algorithm converged\n")
    return(x)
  }
}

ftn <- function(x) {
  fx <- -10+36/x
  dfx <- -36/(x^2)
  return(c(fx,dfx))
}

newtonraphson(ftn, 3, 1e-06)