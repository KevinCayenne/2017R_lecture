
###### �бNSeizure1����ƾ�z��Seizure2���ƦC�覡�C(���D����for�j��^��) ####

setwd("c:/Users/acer/Desktop/R�έpHW/")
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

###### �бNSeizure2����ƾ�z��Seizure1���ƦC�覡�C(���D����for�j��^��) ####

head(Seizure1)
head(Seizure2)

tryS1 <- matrix(NA,dim(Seizure2)[1]*5,2)
for(v in 1:dim(Seizure2)[1]){
  for(n in 2:6){
    tryS1[(v-1)*5+(n-1),1] <- Seizure2[v,n]
    tryS1[(v-1)*5+(n-1),2] <- Seizure2[v,1]
  }
}
colnames(tryS1) <- c("y","trt")
rownames(tryS1) <- seq(1,dim(Seizure2)[1]*5,1)
tryS1
  
##### Ex S1:  �Ф��O�ϥ�for��while�j��ӭp��factorial(10)�����G�C####
  
fac <- 10
for(fn in 1:fac){
  ifelse(fn == 1, multi <- fn, multi <- multi*fn)
}
multi

count <- 1
while(count <= fac){
  ifelse(count == 1, multiw <- count, multiw <- multiw*count)
  count = count + 1
}
multiw

# Ex S2:  �]�� A�B B�B C �T�ڬW�l�AA�W�l�W��20�Ӫ��|�j�p���@�����Ŷ�L�A�Ѥj�Ӥp�|��b�@�_�A####
       #  �p�U��(�H3�Ӷ�L����)�ҥܡC�C���h�@�Ӷ�L�A�b�h���L�{���A���|�j�����i��b���|�p���W���A
       #  �T�ڬW�l���i���L�C�հݳֻ̤ݭn�h�h�֦��A���N�o�Ƕ�L�qA�W�h��C�W�H�ХH���j�禡�Ӧ^���C






# Ex S3:  �Y�ǥ̮ͨy���(���C�Q��)�L�~�⪺���]�`�B�p�U�A�Шϥ�apply���O�D�X�C�@�C�B####
        # �C�@�檺����ơB�̤j�ȡB�̤p�ȡANA�̲��L���p�C 
  
x <- matrix(c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000,
              3600, 4500, 10000, 8500, 3000, 10000, 1000, NA, 1200, 10000,
              3800, 5500, 9000, 6000, 6600, 3000, 9600, 6500, 8200, 8000,
              5000, 6600, 13000, 4500, 5000, NA, 10600, 9500, 7600, 6000,
              6600, 8000, 17000, 3000, 7000, 1000, 12600, 8500, 6000, NA),5,10, byrow = TRUE)
apply(x,1, median, na.rm = T)
apply(x,2, median, na.rm = T)

arg <- c("median", "max", "min")
mod <- c(1,2)

for(i in mod){
  for(argg in arg){
    print(apply(x,i, argg, na.rm = T))
  }
}