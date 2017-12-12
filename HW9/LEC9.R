set.seed(7)   # 設定好種子數為7
rnorm(10)   # 從標準常態分布裡隨機抽出10個實現值

rm(list = ls())
mu <- 7
sigma <- 2
n <- 100
no.rep <- 1000

l95 <- rep(NA,no.rep)
r95 <- rep(NA,no.rep)
l99 <- rep(NA,no.rep)
r99 <- rep(NA,no.rep)

for(i in 1:no.rep){
  print(i)
  set.seed(i)
  x <- rnorm(n,mu,sigma)
  l95[i] <- mean(x)-qnorm(0.975)*sqrt(sigma^2/n)
  r95[i] <- mean(x)+qnorm(0.975)*sqrt(sigma^2/n)
  l99[i] <- mean(x)-qnorm(0.995)*sqrt(sigma^2/n)
  r99[i] <- mean(x)+qnorm(0.995)*sqrt(sigma^2/n)
}
mean((l95<=mu) & (mu<=r95))   # 檢查覆蓋率(coverage)
mean((l99<=mu) & (mu<=r99)) 

###

lt95 <- rep(NA,no.rep)
rt95 <- rep(NA,no.rep)
lt99 <- rep(NA,no.rep)
rt99 <- rep(NA,no.rep)
for(i in 1:no.rep){
  print(i)
  set.seed(i)
  x <- rnorm(n,mu,sigma)
  lt95[i] <- mean(x)-qt(0.975,n-1)*sqrt(var(x)/n)
  rt95[i] <- mean(x)+qt(0.975,n-1)*sqrt(var(x)/n)
  lt99[i] <- mean(x)-qt(0.995,n-1)*sqrt(var(x)/n)
  rt99[i] <- mean(x)+qt(0.995,n-1)*sqrt(var(x)/n)
}
mean((lt95<=mu) & (mu<=rt95))   # 檢查覆蓋率(coverage)
mean((lt99<=mu) & (mu<=rt99))

mean(r95-l95)   # 情況一的信賴區間長度 (z test)
mean(rt95-lt95)  # 情況二的信賴區間長度 (t test)
mean(r99-l99)
mean(rt99-lt99)

### Ex 21-1:

rm(list = ls())
mu <- 7
sigma <- 2
n <- seq(5,100,5)
no.rep <- 1000

coverage <- matrix(NA,length(n),2)

for(j in 1:length(n)){
  l95 <- rep(NA,no.rep)
  r95 <- rep(NA,no.rep)
  l99 <- rep(NA,no.rep)
  r99 <- rep(NA,no.rep)
  for(i in 1:no.rep){
    print(i)
    set.seed(i)
    x <- rnorm(n[j],mu,sigma)
    l95[i] <- mean(x)-qnorm(0.975)*sqrt(var(x)/n[j])
    r95[i] <- mean(x)+qnorm(0.975)*sqrt(var(x)/n[j])
    l99[i] <- mean(x)-qnorm(0.995)*sqrt(var(x)/n[j])
    r99[i] <- mean(x)+qnorm(0.995)*sqrt(var(x)/n[j])
  }
  coverage[j,1] <- mean((l95<=mu) & (mu<=r95))   # 檢查覆蓋率(coverage)
  coverage[j,2] <- mean((l99<=mu) & (mu<=r99))
}
matplot(n,coverage,type="l",lty=c(1,2),col=c(1,2),lwd=2)
abline(h=0.95)
abline(h=0.99)

####






