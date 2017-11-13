
## �m��17-1�G�G���������A�Y�b100�����礤���\20���G
## (1)	�ШD�X���\���v(���]��p)��95% exact confidence interval�C

p <- seq(0,1,0.01)
times <- 100
sec <- 20
fp <- (-0.975)
for(k in 0:(sec-1)){
  fp <- fp+choose(times,k)*(p^k)*((1-p)^(times-k))
}
plot(p,fp)                                                     


ftn7 <- function(p) {
  times <- 100
  sec <- 20
  fp <- (-0.975)
  dfp <- 0
  for(k in 0:(sec-1)){
    fp <- fp+choose(times,k)*(p^k)*((1-p)^(times-k))
    dfp <- dfp+choose(times,k)*((k*p^(k-1))*((1-p)^(times-k))-(p^k)*((times-k)*(1-p)^(times-1-k)))
  }                                                      
  return(c(fp, dfp))
}
newtonraphson(ftn7, 0.1, 1e-06)

######

p <- seq(0,1,0.01)
times <- 100
sec <- 20
fp <- (-0.025)
for(k in 0:sec){
  fp <- fp+choose(times,k)*(p^k)*((1-p)^(times-k))
}
plot(p,fp)                                                     


ftn8 <- function(p) {
  times <- 100
  sec <- 20
  fp <- (-0.025)
  dfp <- 0
  for(k in 0:sec){
    fp <- fp+choose(times,k)*(p^k)*((1-p)^(times-k))
    dfp <- dfp+choose(times,k)*((k*p^(k-1))*((1-p)^(times-k))-(p^k)*((times-k)*(1-p)^(times-1-k)))
  }                                                      
  return(c(fp, dfp))
}
newtonraphson(ftn8, 0.2, 1e-06)

## (2)	�ХH���������w�z�D���D��95% confidence interval�A�ä����P(1)�p�D���G�C

phat <- 20/100
phat-qnorm(0.975)*sqrt(phat*(1-phat)/20)
phat+qnorm(0.975)*sqrt(phat*(1-phat)/20)

## (3)	�ХH���������w�z�D�m��17��95% confidence interval�A�ä����P95% exact confidence interval�A���@�誺���G�����i�H�H

phat <- 3/20
phat-qnorm(0.975)*sqrt(phat*(1-phat)/20)
phat+qnorm(0.975)*sqrt(phat*(1-phat)/20)