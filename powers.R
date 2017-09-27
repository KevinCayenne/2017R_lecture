
###################################################################################
# If you use this code to analyze data, please cite the following paper:  
# Lin W-Y, Chen WJ, Liu C-M, Hwu H-G, McCarroll SA, Glatt SJ, Tsuang MT (2017). Adaptive combination of Bayes factors as a powerful method for the joint analysis of rare and common variants. Scientific Reports, in press.
# Any questions or comments, please contact: Wan-Yu Lin, linwy@ntu.edu.tw, Institute of Epidemiology and Preventive Medicine, National Taiwan University College of Public Health
# Thank you.
###################################################################################


library(gtx)
library(MASS)
library(corpcor)
ADABFunre <- function(Y, Copy, Type="D", Cov=NULL){
  Y <- as.matrix(Y,ncol=1)     
  Copy <- as.matrix(Copy)
  MAF = apply(Copy, 2 , function(x) (0.5*mean(x, na.rm = T)))
  Copy[,which(MAF>0.5)] <- 2-Copy[,which(MAF>0.5)]  # Let Copy be the matrix of the number of minor alleles.  
  MAF = apply(Copy, 2 , function(x) (0.5*mean(x, na.rm = T)))
  keepvar <- which(MAF > 0.0001)
  Copy <- Copy[,keepvar]
  G <- Copy - rep(1, nrow(Copy)) %*% t(colMeans(Copy,na.rm=T))  
  k <- ncol(G)
  if(Type=="D"){
    bbb <- apply(G,2,function(x){coef(glm(Y~x,family=binomial()))[2]})
    bbbse <- apply(G,2,function(x){summary(glm(Y~x,family=binomial()))$coef[2,2]})
    if(!is.null(Cov)){
      Cov <- as.matrix(Cov)
      bbb <- apply(G,2,function(x){coef(glm(Y~x+Cov,family=binomial()))[2]})
      bbbse <- apply(G,2,function(x){summary(glm(Y~x+Cov,family=binomial()))$coef[2,2]})
    }
  }
  
  if(Type=="C"){
    bbb <- apply(G,2,function(x){coef(lm(Y~x))[2]})
    bbbse <- apply(G,2,function(x){summary(lm(Y~x))$coef[2,2]})
    if(!is.null(Cov)){
      Cov <- as.matrix(Cov)
      bbb <- apply(G,2,function(x){coef(lm(Y~x+Cov))[2]})
      bbbse <- apply(G,2,function(x){summary(lm(Y~x+Cov))$coef[2,2]})
    }      
  }
  
  fun1 <- function(xx){
    ABF <- abf.Wakefield(xx, bbbse, 0.2)
    ABF[!is.finite(ABF)] <- NA
    return(cumsum(sort(log(ABF),decreasing=T)))
  }
  ABFo <- abf.Wakefield(bbb, bbbse, 0.2)
  ABFo[!is.finite(ABFo)] <- NA
  obsABF <- cumsum(sort(log(ABFo),decreasing=T)) 
  
  covG <- cor(G, use="pairwise.complete.obs") * (matrix(bbbse,ncol=1) %*% matrix(bbbse,nrow=1))
  if(is.positive.definite(covG)==FALSE){covG <- make.positive.definite(covG, tol=1e-3)}
  n.res <- 100
  
  bbb_null <- mvrnorm(n = n.res, rep(0,ncol(G)), covG)
  ABF_null <- t(apply(bbb_null,1,fun1))
  ABFp <- rbind(obsABF,ABF_null)
  Pmat <- (nrow(ABFp)-apply(ABFp,2,rank)+1)/nrow(ABFp)
  Pmin <- apply(Pmat,1,min)
  p.rej <- sum(Pmin[1]>Pmin[2:length(Pmin)])
  p.ADABF <- p.rej / n.res
  
  if(all(p.ADABF>=0.1)){
    pvalue <- p.ADABF
  } else {
    n.res <- 1000
    bbb_null <- mvrnorm(n = n.res, rep(0,ncol(G)), covG)
    ABF_null <- t(apply(bbb_null,1,fun1))
    ABFp <- rbind(obsABF,ABF_null)
    Pmat <- (nrow(ABFp)-apply(ABFp,2,rank)+1)/nrow(ABFp)
    Pmin <- apply(Pmat,1,min)
    p.rej <- sum(Pmin[1]>Pmin[2:length(Pmin)])
    p.ADABF <- p.rej / n.res
    
    if(all(p.ADABF>=0.01)){
      pvalue <- p.ADABF
    } else {
      n.res <- 10000
      bbb_null <- mvrnorm(n = n.res, rep(0,ncol(G)), covG)
      ABF_null <- t(apply(bbb_null,1,fun1))
      ABFp <- rbind(obsABF,ABF_null)
      Pmat <- (nrow(ABFp)-apply(ABFp,2,rank)+1)/nrow(ABFp)
      Pmin <- apply(Pmat,1,min)
      p.rej <- sum(Pmin[1]>Pmin[2:length(Pmin)])
      p.ADABF <- p.rej / n.res
      
      if(all(p.ADABF>=0.001)){
        pvalue <- p.ADABF
      } else {
        n.res <- 100000
        bbb_null <- mvrnorm(n = n.res, rep(0,ncol(G)), covG)
        ABF_null <- t(apply(bbb_null,1,fun1))
        ABFp <- rbind(obsABF,ABF_null)
        Pmat <- (nrow(ABFp)-apply(ABFp,2,rank)+1)/nrow(ABFp)
        Pmin <- apply(Pmat,1,min)
        p.rej <- sum(Pmin[1]>Pmin[2:length(Pmin)])
        p.ADABF <- p.rej / n.res
        
        if(all(p.ADABF>=0.0001)){
          pvalue <- p.ADABF
        } else {
          n.res <- 1000000
          bbb_null <- mvrnorm(n = n.res, rep(0,ncol(G)), covG)
          ABF_null <- t(apply(bbb_null,1,fun1))
          ABFp <- rbind(obsABF,ABF_null)
          Pmat <- (nrow(ABFp)-apply(ABFp,2,rank)+1)/nrow(ABFp)
          Pmin <- apply(Pmat,1,min)
          p.rej <- sum(Pmin[1]>Pmin[2:length(Pmin)])
          p.ADABF <- p.rej / n.res
          
          if(all(p.ADABF>=0.00001)){
            pvalue <- p.ADABF
          } else {
            n.res <- 10000000
            bbb_null <- mvrnorm(n = n.res, rep(0,ncol(G)), covG)
            ABF_null <- t(apply(bbb_null,1,fun1))
            ABFp <- rbind(obsABF,ABF_null)
            Pmat <- (nrow(ABFp)-apply(ABFp,2,rank)+1)/nrow(ABFp)
            Pmin <- apply(Pmat,1,min)
            p.rej <- sum(Pmin[1]>Pmin[2:length(Pmin)])
            p.ADABF <- p.rej / n.res
            
            pvalue <- p.ADABF
          }
        }
      }
    }
  }
  return(pvalue)   
}




