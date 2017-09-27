list(x=seq(1,10,1), y=LETTERS[1:10])

x <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)  # 壓歲錢
if(mean(x, na.rm=T)>5000){
  rich <- 1
} else{
  rich <- 0
}
rich

rm(list = ls())

x <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)  # 壓歲錢
if(mean(x, na.rm=T)>5000){
  rich <- 1
} 
else{
  rich <- 0
}
rich

rich <- as.numeric(mean(x, na.rm=T)>5000) 


rm(list = ls())
nn <- seq(1,100,1)
ss <- 0
i <- 1
while(i<=length(nn)){
  ss <- ss+nn[i]
 i = i + 1
}
ss

done <- FALSE
count <- 1
x <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000) 
while(!done){
  if (is.na(x[count])==FALSE){
    count = count + 1
  } else {
    done <- TRUE
    print(count)
  }
}

num = 2
pnum = 3
sp = 
pnum.list <- c()

while(num <= pnum){
  if(pnum%%num == 0){
    break
  } else {
    num = num + 1
    if(num == pnum){
      pnum.list(sp) <-
    }
  }
  
}

x <- "Citroen SM"
y <- "Jaguar XK150"
z <- "Ford Falcon GT-HO"
(wish.list <- paste(x, y, z, sep = ", "))

x <- 7
n <- 5
# display powers
cat("Powers of", x, "\n")        # cat displays concatenated character
cat("exponent result\n\n")      # \n for a newline
result <- 1
for (i in 1:n) {
  result <- result * x
  cat(format(i, width = 8),format(result, width = 10),"\n", sep = "")
}

setwd("C:/Users/acer/Desktop/R統計HW")
source("powers.R")

SS1 <- read.csv("seizure.csv")
SS2 <- read.table("seizure.txt")

scan(file = "seizure.txt", what = double(), n = -1, sep = "", skip = 0, quiet = FALSE)
scan(file = "seizure.csv", what = double(), n = -1, sep = ",", skip = 1, quiet = FALSE)



bmi <- read.csv("BMIrepeated.csv")
x <- seq(0,9,3)
y <- cbind(bmi$BMI0, bmi$BMI1, bmi$BMI2, bmi$BMI3)

par(mfrow = c(1,2))
plot(x,y[1,],type="b",lwd=1,col=1,lty=1,pch=1,ylim=c(15,50),axes = FALSE,xlab="months",ylab="BMI",main="Placebo group")
axis(1, at = x, labels = seq(0,9,3))
axis(2)

for(subj in 2:10){
  lines(x,y[subj,],lty=1,lwd=1,col=subj,type="b",pch=subj)
}
legend("topright",bty="n", c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10"),lty=1,col=(1:10),lwd=1,pch=(1:10))

