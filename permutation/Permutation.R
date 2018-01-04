rm(list = ls())
Gene1 <- read.table('c:/Users/acer/Desktop/R統計HW/Gene2.txt',header=F)
pvalue <- c()

for(kk in 2:ncol(Gene1)){
  pvalue[kk-1] <- summary(glm(Gene1[,1]~Gene1[,kk],family=binomial))$coef[2,4]
}

minP <- min(pvalue)

# Permutation
n.per <- 1000  # number of permutation = 1,000
n <- nrow(Gene1)  # the sample size
minP.per <- c()
for(pp in 1:n.per){
  Y.per <- Gene1[sample(1: n, n, replace=FALSE),1]  # 1~n 裡取n個，抽出後不放回
  pvalue.per <- c()
  for(kk in 2:ncol(Gene1)){
    pvalue.per[kk-1] <- summary(glm(Y.per~Gene1[,kk],family=binomial))$coef[2,4]
  }
  minP.per[pp] <- min(pvalue.per)
}

(sum(minP.per<=minP)+1)/(n.per+1)
hist(minP.per)

####

rm(list = ls())                                                                                                            
betawatchpc <- c(0,0.25,0.5)
sig <- seq(0.01,0.05,0.01)
n <- 50
Y <- c()
no.rep <- 100
rej.rate <- matrix(NA,length(betawatchpc),length(sig))
rej.rate.per <- matrix(NA,length(betawatchpc),length(sig))

for(betaloop in 1:length(betawatchpc)){
  
  pvalue <- c()
  permutationPvalue <- c()
  
  for(i in 1:no.rep){
    set.seed(i)
    age <- rnorm(n,35,5)
    watchpc <- rnorm(n,4,1)
    x.beta <- 7-0.05*age+betawatchpc[betaloop]*watchpc
    random.error <- rnorm(n,0,1)
    
    Y <- x.beta+random.error
    
    pvalue[i] <- summary(lm(Y~age+watchpc))$coef[3,4]
    
    ### Permutation
    n.per <- 1000   # number of permutation = 1,000
    pvalue.per <- c()
  
    for(pp in 1:n.per){
      Y.per <- Y[sample(1: n, n, replace=FALSE)]     # 1~n 裡取n個，抽出後不放回 
      pvalue.per[pp] <- summary(lm(Y.per~age+watchpc))$coef[3,4]
    }
    
    permutationPvalue[i] <- (sum(pvalue.per<=pvalue[i])+1)/(n.per+1) 
  }

    for(k in 1:length(sig)){
      rej.rate[betaloop,k] <- sum(pvalue<sig[k])/no.rep
      rej.rate.per[betaloop,k] <- sum(permutationPvalue<sig[k])/no.rep
    }
  
}
rej.rate
rej.rate.per

par(mfrow = c(1,2))

matplot(sig,t(rej.rate),col=c(1:length(betawatchpc)),pch=c(1:length(betawatchpc)),lty=c(1:length(betawatchpc)),type="b",frame=F,xlab="Significance level",ylab="Rejection rate",ylim=c(0,1),lwd=2,main="Wald test")
abline(a=0,b=1,col=8)
legend(0.03,rej.rate[1,4]+0.07,expression(paste(beta,'=0')),bty="n")
legend(0.03,rej.rate[2,4]+0.07,expression(paste(beta,'=0.25')),bty="n")
legend(0.03,rej.rate[3,4]+0.07,expression(paste(beta,'=0.5')),bty="n")

matplot(sig,t(rej.rate.per),col=c(1:length(betawatchpc)),pch=c(1:length(betawatchpc)),lty=c(1:length(betawatchpc)),type="b",frame=F,xlab="Significance level",ylab="Rejection rate",ylim=c(0,1),lwd=2,main="Permutation test")
abline(a=0,b=1,col=8)
legend(0.03,rej.rate.per[1,4]+0.07,expression(paste(beta,'=0')),bty="n")
legend(0.03,rej.rate.per[2,4]+0.07,expression(paste(beta,'=0.25')),bty="n")
legend(0.03,rej.rate.per[3,4]+0.07,expression(paste(beta,'=0.5')),bty="n")
