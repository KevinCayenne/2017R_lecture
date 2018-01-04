rm(list = ls())
betagpa <- c(0,0.75,1.5)
sig <- seq(0.01,0.05,0.01)
n <- 200
Y <- c()
no.rep <- 100
tol = 1e-4
n.max = 100
rej.rate <- matrix(NA,length(betagpa),length(sig))
rej.rate.per <- matrix(NA,length(betagpa),length(sig))

for(betaloop in 1:length(betagpa)){
  
  pvalueglm <- c()
  permutationPvalue <- c()
  
  for(i in 1:no.rep){
    set.seed(i)
    gpa <- rnorm(n,3.1,0.3)
    gre <- rnorm(n,580,80)
    x.beta <- -6+betagpa[betaloop]*gpa+0.005*gre
    pi.admit <- exp(x.beta)/(1+exp(x.beta))
    
    for(j in 1:n){
      Y[j] <- sample(c(0,1),1,c(1-pi.admit[j],pi.admit[j]),replace=F) 
    }
    
    pvalueglm[i] <- summary(glm(Y~gpa+gre,family=binomial))$coef[2,4]
    
    ### Permutation
    n.per <- 1000   # number of permutation = 1,000
    pvalue.per <- c()
    
    for(pp in 1:n.per){
      Y.per <- Y[sample(1: n, n, replace=FALSE)]     # 1~n 裡取n個，抽出後不放回 
      pvalue.per[pp] <- summary(glm(Y.per~gpa+gre,family=binomial))$coef[2,4]
    }
    permutationPvalue[i] <- (sum(pvalue.per<=pvalueglm[i])+1)/(n.per+1) 
  }
  
  for(k in 1:length(sig)){
    rej.rate[betaloop,k] <- sum(pvalueglm<sig[k])/no.rep
    rej.rate.per[betaloop,k] <- sum(permutationPvalue<sig[k])/no.rep
  }
  
}
rej.rate
rej.rate.per

par(mfrow = c(1,2))

matplot(sig,t(rej.rate),col=c(1:length(betagpa)),pch=c(1:length(betagpa)),lty=c(1:length(betagpa)),type="b",frame=F,xlab="Significance level",ylab="Rejection rate",ylim=c(0,1),lwd=2,main="Wald test")
abline(a=0,b=1,col=8)
legend(0.03,rej.rate[1,4]+0.07,expression(paste(beta,'=0')),bty="n")
legend(0.03,rej.rate[2,4]+0.07,expression(paste(beta,'=0.25')),bty="n")
legend(0.03,rej.rate[3,4]+0.07,expression(paste(beta,'=0.5')),bty="n")

matplot(sig,t(rej.rate.per),col=c(1:length(betagpa)),pch=c(1:length(betagpa)),lty=c(1:length(betagpa)),type="b",frame=F,xlab="Significance level",ylab="Rejection rate",ylim=c(0,1),lwd=2,main="Permutation test")
abline(a=0,b=1,col=8)
legend(0.03,rej.rate.per[1,4]+0.07,expression(paste(beta,'=0')),bty="n")
legend(0.03,rej.rate.per[2,4]+0.07,expression(paste(beta,'=0.25')),bty="n")
legend(0.03,rej.rate.per[3,4]+0.07,expression(paste(beta,'=0.5')),bty="n")
    