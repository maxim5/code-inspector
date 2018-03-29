# data.table example

library(data.table)

# Loading data
# peerI is an 10x4537 indentification matrix containing unique firm indentifiers
# corrI is a 3749x4539 matrix of peer correlations corresponding to the firm indentifiers in peerI (309 Mb)

peerI <- read.csv("...", header=T, sep=",")
corrI <- read.csv("...", header=T, sep=",")
peerI <- peerI[,-c(1,5)]

# Removing bad identifiers
peerI <- subset(peerI, peerI$ISIN != "CA45324F2098")
peerI <- subset(peerI, peerI$ISIN != "RU000A0JNUD0") 



# These loops create variables for a panel regression. Therefore they must be stacked on top of each other using rbind
# Only one variable from the panel is included in this example
#===============

corr <- NULL;pb <- txtProgressBar(min = 0, max = 4535, style = 3)
reg <- matrix(data = NA, nrow = 3749, ncol = 1, byrow = FALSE, dimnames = NULL)


# Using data.frame() & colnames()
ptm <- proc.time()
for(i in 1:4535){
  reg <- data.frame(unlist(corrI[(as.character(peerI[i,1]))])) 
  colnames(reg)[1] <- "CorrRawret"
  corr <- rbind(corr, reg)
  
  Sys.sleep(0.1)
  # update progress bar
  setTxtProgressBar(pb, i)
  close(pb)
}
proc.time() - ptm


#           == TIME ==
#    user     system     elapsed 
# 12426.802  1569.623   14501.812 

#===============

corr1 <- NULL;pb <- txtProgressBar(min = 0, max = 4535, style = 3)
reg <- matrix(data = NA, nrow = 3749, ncol = 1, byrow = FALSE, dimnames = NULL)

# Using data.table() & setnames()
ptm <- proc.time()
for(i in 1:4535){
  reg <- data.table(unlist(corrI[(as.character(peerI[i,1]))])) 
  setnames(reg, "CorrRawret")
  corr1 <- rbind(corr1, reg)
  
  Sys.sleep(0.1)
  # update progress bar
  setTxtProgressBar(pb, i)
  close(pb)
}
proc.time() - ptm

#           == TIME =
#   user   system  elapsed 
# 642.689  470.685 1615.708 





