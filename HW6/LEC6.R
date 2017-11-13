rm(list = ls())
p <- seq(0,1,0.001)
fp <- (p^3)*((1-p)^17)   
plot(p,fp,ylab='Likelihood')
abline(v=3/20)


lam <- seq(0,10,0.001)

X <- c(3,1,2,0,6,8,4,5,7,0) 
fp2 <- Lik(X,p)
plot(p,fp2,col=2)  

Lik <- function(X, l){
  likelihood <- 1
  for (i in 1:length(X)){
    if (X[i] == 1){
      likelihood <- likelihood * (exp(-l)*)
    } else {
      likelihood <- likelihood * (1 - p)
    }
  }
  return(likelihood)
}

### Ex 18  (1):
rm(list = ls())
lambda <- seq(0,10,0.01)

Lik <- function(X, lambda){
  likelihood <- 1
  for (i in 1:length(X)){
    likelihood <- likelihood * ( ((exp(-lambda))*(lambda^X[i]))/(factorial(X[i])) )
  }
  return(likelihood)
}
X <- c(3,1,2,0,6,8,4,5,7,0)
fp2 <- Lik(X,lambda)
plot(lambda,fp2,ylab='Likelihood')
abline(v=mean(X))


###
Lik <- function(X, lambda){
  likelihood <- 1
  for (i in 1:length(X)){
    likelihood <- likelihood + log( ((exp(-lambda))*(lambda^X[i]))/(factorial(X[i])) )
  }
  return(likelihood)
}

X <- c(3,1,2,0,6,8,4,5,7,0)
fp2 <- Lik(X,lambda)
plot(lambda,fp2,ylab='Likelihood')
abline(v=mean(X))

###

rm(list = ls())

logLikFun <- function(datapoint, param){
  mu <- param[1]
  sigma <- param[2]
  return(sum(dnorm(datapoint, mean = mu, sd = sigma, log = TRUE)))   ### log Likelihood of normal distribution
}

mu <- seq(70,75,0.01)
sigma <- seq(11,15,0.01)
LL <- matrix(NA,length(mu)*length(sigma),3)
X <- c(21.5,16,19,20,21.5,18.6,19.3,21.1,23.5,21.3)

cc <- 1
for(i in 1:length(mu)){
  for(j in 1:length(sigma)){
    LL[cc,] <- c(mu[i],sigma[j],logLikFun(X, c(mu[i],sigma[j])))
    cc <- cc+1  
  }
}
library(scatterplot3d)    ### 需先安裝程式套件scatterplot3d
scatterplot3d(LL[,1],LL[,2],LL[,3],xlab="mu",ylab="sigma",zlab="log Likelihood", highlight.3d=TRUE)
