
###### 請將Seizure1的資料整理成Seizure2的排列方式。(本題限用for迴圈回答) ####

setwd("c:/Users/acer/Desktop/R統計HW/")
sei <- read.csv("seizure.csv")
Seizure1 <- read.csv('seizure.csv')
Seizure2 <- read.table("seizure.txt")

tryS2 = matrix(NA,(length(Seizure1$y)/5),6)
for(v in 1:length(Seizure1$y)){
    tryS2[v,1] <- Seizure1$trt[((v-1)*5+1)]
  for(i in 2:6){
    tryS2[v,i] <- Seizure1$y[(v-1)*5+(i-1)]
  }
}
colnames(tryS2) <- paste("V", 1:6, sep="")
rownames(tryS2) <- seq(1,(length(Seizure1$y)/5),1)
tryS2

###### 請將Seizure2的資料整理成Seizure1的排列方式。(本題限用for迴圈回答) ####





