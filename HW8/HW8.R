setwd("c:/Users/acer/Desktop/R²Î­pHW")

newton <- function(f3, x0, tol = 1e-9, n.max = 100) {
  x <- x0
  f3.x <- f3(x)
  n <- 0
  while ((max(abs(f3.x[[1]])) > tol) & (n < n.max)) {
    x <- x - solve(f3.x[[2]]) %*% f3.x[[1]]
    f3.x <- f3(x)
    n <- n + 1
  }
  if (n == n.max) {
    cat('newton failed to converge\n')
  } else {
    return(x)
  }
}

f6 <- function(betacoef) {
  seizure <- read.csv("seizure.csv")
  
  Y <- seizure$y                                                                             
  X <- cbind(rep(1,length(Y)),seizure$trt,seizure$age)
  u <- exp(X %*% betacoef) * seizure$ltime
  gradient <-  t(X) %*% (Y-u)
  hessian <- -t(X) %*% diag(c(u),length(Y)) %*% X
  logLikelihood <- sum(-u+Y*log(u)-log(factorial(Y)))
  return(list(gradient, hessian, logLikelihood))
}
newton(f6,c(0,0,0))

MLE <- newton(f6,c(0,0,0))

mm <- glm(seizure$y~seizure$trt + seizure$age ,offset = log(seizure$ltime), family=poisson)

f6(MLE)[[1]]
solve(-f6(MLE)[[2]])  
vcov(mm)
f6(MLE)[[3]]
logLik(mm)  
