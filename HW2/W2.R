
setwd("C:/Users/acer/Desktop/R²Î­pHW/HW1")
Seizure1 <- read.csv('seizure.csv')
Seizure2 <- read.table('Seizure.txt')

x <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)

red <- function(Money){
    ANS <- c()  
    ANS[1] <- sum(>Money, na.rm = T)
    ANS[2] <- x[3] && x[8] > Money  #and
    ANS[3]<- x[3] | x[8] > Money  #or
    ANS[4]<- xor(x[3]>Money, x[8]>Money) #xor
    return(ANS)
}

M <- matrix(NA,20,4)

rownum = 0
for (i in seq(1000,20000,by=1000)){
  rownum = rownum+1
  for (num in 1:4) {
    M[rownum,num] <- red(i)[num]
  }
}

money.vec <- seq(1000,20000,1000)
ANS.matrix <- matrix(NA, length(money.vec)){
  ANS.matrix[i,] <- red(money.vec[i])
}

sample(50, size = 12, replace = T)

(x <- c(sort(sample(1:20, 9)), NA))
(y <- c(sort(sample(3:23, 7)), NA))
union(x, y)
intersect(x, y)
setdiff(x, y)
setdiff(y, x)
setequal(x, y)
is.element(x,y) # = x%in%y

x%in%y

boy <- seq(1,50,2)
girl <- seq(2,50,2)
mid <- c(11,16,23,31,36,47,50)
fin <- c(3,9,16,20,27,31,36,49,50)
intersect(intersect(mid, fin),boy)
intersect(intersect(mid, fin),girl)
intersect(setdiff(mid, fin),boy)
intersect(setdiff(fin, mid),girl)

rm(list = ls())

par(mfrow = c(3,2))
CLT <- function(n){
  N <- 10000
  score <- rnorm(N,75,5)
  hist(score, xlim=c(50,100), breaks=seq(50,100,1))
  mean.score <- c()
  for(i in 1:1000){
    mean.score[i] <- mean(score[sample(1:N, size=n)]) 
  }
  hist(mean.score, xlim=c(50,100), breaks=seq(50,100,1))
  return( c(mean(mean.score), var(mean.score)) )
}

CLT(n=5)
CLT(n=10)
CLT(n=20)

25/5
25/10
25/20




