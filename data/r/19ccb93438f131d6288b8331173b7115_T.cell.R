data <- read.csv("T.cell.csv",header=F)
names(data) <- c("y1","y2","y3","grp")
str(data)


library(reshape2)
library(plyr)
library(doBy)
data <- melt(data)
data <- data[, c("grp","value")]
data <- na.omit(data)
data <- orderBy( ~ grp, data)
data$value <- as.numeric(data$value)

data$x <- sqrt(data$value)

# summaryBy( value ~ grp, data = data, FUN = length)

npem.ll <- function(data, par){
  lamda <- par[1:4]
  beta  <- par[5:6]
  sigma <- par[7]
  sigma
  
}
mcmc.npem <-
  function(data,start=c(0.5,1.5,2.5,3.5,10,5,1),n=c(24,24,24,22),
           nstep = 1000,tol=1e-10,prnt=FALSE)
  {
    
    # npem package: does EM for these data
    # I use it here in order to calculate the log likelihood
    #      (npem.ll)
#     require(npem)
    
    gp <- length(n)
    N <- sum(n)
    output <- matrix(nrow=nstep+1,ncol=length(start)+1)
    output[1,] <- c(start,npem.ll(data,start))
    wh <- rep(1:4,n)
    
    for(i in 1:nstep) {
      
      # sample k's
      lam <- rep(start[1:gp],n)
      maxk <- qpois(1-tol*100,lam)
      k <- apply(cbind(data,lam,maxk),1,
                 function(x,absig,i) {
                   a <- dpois(0:x[3],x[2]+tol)*
                     dnorm(x[1],absig[1]+absig[2]*(0:x[3]),absig[3])
                   print(c( x,i) )
                   sample(0:x[3],1,prob=a)
                 }, start[-(1:gp)],i)
      
      # sample lambdas
      start[1:gp] <- rgamma(gp,tapply(k,wh,sum)+1, scale = 1/(n+1))
      
      # lin reg
      a <- summary(lm(data ~ k))
      betahat <- a$coef[,1]
      sig <- a$sigma
      Vhat <- a$cov
      
      # simulate sigma
      sig <- sig*sqrt((N-2)/rchisq(1,N-2))
      
      # simulate a,b
      beta <- rnorm(2) %*% chol(Vhat * sig) + betahat
      
      start[-(1:gp)] <- c(beta,sig)
      
      output[i+1,] <- c(start,npem.ll(data,start))
      
      if(prnt) print(round(output[i+1,],2))
      
    }
    print(i)
    output
  }
data
res <- mcmc.npem(data$x, nstep = 10000)

res1 <- res[5000:10000,]
summary(res1)
plot(res1[,6],res1[,7], pch = 19,type = "l")