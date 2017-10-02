## ������Ӧ۩�ۥѫ�1���d������ɡA�Ӧp����{���H�ܽd���������w�z�H

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

## �Цۦ�s�g�@�ӭp��²��u�ʰj�k�Y�Ʀ��p�Ȫ���ơA�åH�ҵ{�����Wseizure.csv��y���������ܼơA
## ltime���������ܼơA�D�j�k�Y�Ʀ��p��(�t�I�Z���P�ײv��)�C

setwd('C:/Users/acer/Desktop/R�έpHW/HW2')
table <- read.csv("seizure.csv")

regtry <- function(y, x){
  b.one <- (sum(y*x)-(sum(x)*sum(y)/length(x)))/(sum(x^2)-(sum(x)^2/length(x)))
  b.zero <- mean(y) - b.one*mean(x)
  result <- data.frame("Intercept" = b.zero, "b1" = b.one)
  return(result)
}

regtry(table$y,table$ltime)

  