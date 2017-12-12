p <- 0.15
n <- 20
no.rep <- 1000

lt95 <- rep(NA,no.rep)
rt95 <- rep(NA,no.rep)
lt99 <- rep(NA,no.rep)
rt99 <- rep(NA,no.rep)

for(i in 1:no.rep){
  print(i)
  set.seed(i)
  x <- rbinom(n,2,p)
  l95[i] <- mean(x)-qnorm(0.975)*sqrt(p*(1-p)/n)
  r95[i] <- mean(x)+qnorm(0.975)*sqrt(p*(1-p)/n)
  l99[i] <- mean(x)-qnorm(0.995)*sqrt(p*(1-p)/n)
  r99[i] <- mean(x)+qnorm(0.995)*sqrt(p*(1-p)/n)
}
# ÀË¬dÂÐ»\²v(coverage)
mean((l95<=p) & (p<=r95))   
mean((l99<=p) & (p<=r99)) 

mean(r95-l95)
mean(r99-l99)