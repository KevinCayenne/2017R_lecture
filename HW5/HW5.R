
## 練習17-1：二項分布中，若在100次試驗中成功20次：
## (1)	請求出成功機率(假設為p)的95% exact confidence interval。

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

## (2)	請以中央極限定理求本題的95% confidence interval，並比較其與(1)小題結果。

phat <- 20/100
phat-qnorm(0.975)*sqrt(phat*(1-phat)/100)
phat+qnorm(0.975)*sqrt(phat*(1-phat)/100)

## (3)	請以中央極限定理求練習17的95% confidence interval，並比較其與95% exact confidence interval，哪一方的結果較為可信？

phat <- 3/20
phat-qnorm(0.975)*sqrt(phat*(1-phat)/20)
phat+qnorm(0.975)*sqrt(phat*(1-phat)/20)