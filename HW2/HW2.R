## 當母體來自於自由度1的卡方分布時，該如何更改程式以示範中央極限定理？

par(mfrow = c(3,2))
CLT <- function(n){
  N <- 10000
  score <- rchisq(N,1)
  hist(score)
  mean.score <- c()
  for(i in 1:1000){
    mean.score[i] <- mean(score[sample(1:N, size=n)]) 
  }
  hist(mean.score)
  return( c(mean(mean.score), var(mean.score)) )
}
CLT(5)
CLT(50)
CLT(100)

## 請自行編寫一個計算簡單線性迴歸係數估計值的函數，並以課程網頁上seizure.csv的y當成反應變數，
## ltime當成解釋變數，求迴歸係數估計值(含截距項與斜率項)。

setwd('C:/Users/acer/Desktop/R統計HW/HW2')
table <- read.csv("seizure.csv")

regtry <- function(y, x){
  b.one <- (sum(y*x)-(sum(x)*sum(y)/length(x)))/(sum(x^2)-(sum(x)^2/length(x)))
  b.zero <- mean(y) - b.one*mean(x)
  result <- data.frame("Intercept" = b.zero, "b1" = b.one)
  return(result)
}

regtry(table$y,table$ltime)

  
