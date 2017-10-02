nfact2 <- function(n) {
  # calculate n factorial
  if (n == 1) {
    cat("called nfact2(1)\n")
    return(1)
  } else {
    cat("called nfact2(", n, ")\n", sep = "")
    return(n*nfact2(n-1))
  }
}

nfact2(6)
nfact2(10)


lm.obj <- lm(sei$y ~ sei$trt + sei$age + sei$ltime)

A <- matrix(c(3,5,-1,2,-1,3,4,2,-3),3,3, byrow = T)
b <- c(10 , 9 ,-1)
solve(A,b)


#######

seqtry <- function(n) {
  S <- c(seq(1,n-1,1),seq(n,1,-1))
  return(S)
}

seqtry(10)
seqtry(20)