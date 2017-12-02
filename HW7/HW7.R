setwd("c:/Users/acer/Desktop/R²Î­pHW")
admit <- read.table("admit.txt", header=T)

glm <- glm(admit$admit~admit$gpa+admit$gre,family=binomial)

### Ex20 (1):
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

f5 <- function(betacoef) {
  admit <- read.table('admit.txt',header=T)
  Y <- admit$admit                                                                             
  X <- cbind(rep(1,length(Y)),admit$gpa,admit$gre)
  pi1 <- exp(X %*% betacoef)/(1+exp(X %*% betacoef))
  gradient <-  t(X) %*% (Y-pi1)
  hessian <- -t(X) %*% diag(c(pi1*(1-pi1)),length(Y)) %*% X
  logLikelihood <- sum(Y*log(pi1/(1-pi1))+log(1-pi1))
  return(list(gradient, hessian, logLikelihood))
}

MLE <- newton(f5,c(0,0,0))
admit <- read.table('admit.txt',header=T)
mm <- glm(admit$admit~admit$gpa+admit$gre,family=binomial) 

f5(MLE)[[1]]
solve(-f5(MLE)[[2]])  
vcov(mm)
f5(MLE)[[3]]
logLik(mm)  
