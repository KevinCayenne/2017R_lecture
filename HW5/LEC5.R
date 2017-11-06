library(binom)

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
  fx <- x^2-x-12
  dfx <- 2*x-1
  return(c(fx, dfx))
}

newtonraphson(ftn, 2, 1e-06)

########################################

p <- seq(0,1,0.01)
fp <- -0.025
for (k in 0:3){
  fp <- fp + choose(20,k)*(p^k)*(1-p)^(20-k)
}
plot(p,fp)

##############

ftn7 <- function(p) {
  fp <- (-0.025)
  dfp <- 0
  for(k in 0:3){
    fp <- fp+choose(20,k)*(p^k)*((1-p)^(20-k))
    dfp <- dfp+choose(20,k)*((k*p^(k-1))*((1-p)^(20-k))-(p^k)*((20-k)*(1-p)^(19-k)))
  }
  return(c(fp, dfp))
}
newtonraphson(ftn7, 0.1, 1e-06)