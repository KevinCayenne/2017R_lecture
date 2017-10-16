library(UsingR)

setwd("c:/Users/acer/Desktop")
A <- read.csv("try1.csv")

plot(A$Delivery, A$F.ej., type="b")
lines(A$Delivery.time, A$F.ej.)

h <- hist(A$Delivery.time, freq = FALSE, xlim=c(0,70), ylim=c(0,0.03), breaks=12)
h$counts <- A$fj
plot(h, xlim = c(0,70))
lines((A$Delivery.time+2.5), A$fj)

ggplot(A, aes(x=A$Delivery.time, y=A$fj, ..density..)) + 
        geom_bar(stat = "identity") + 
        xlim(c(0,70)) + geom_line()

v <- rep(A$Delivery.time, A$nj)
hist(v, xlim = c(0,70), ylim = c(0,0.07), freq =FALSE)
lines(density(v-2.5, bw =2.33, na.rm = T))

## 2.4
cp <- data.frame(central.park)
attach(cp)
cp
na.omit(cp)

## 2.8
n <- data.frame(npdb)
attach(n)
sort(tapply(n$amount, n$state, sum), decreasing = T)

## 2.9
table(npdb$ID)
# ID是匿名後的參賽者編號，這個動作是將不同ID按照順序排列並計算其數量

## 2.16
#1
mean(rivers<500)
#2
mean(rivers<mean(rivers))
#3
sumR <- summary(rivers)
sumR[5]

## 2.17
#1
mean(nym.2002$time/60 < 3)
#2 #3
quantile(nym.2002$time, c(.9, .75, .1))
#4  
hist(nym.2002$time, breaks = 20, xlim = c(0,700))

