#' @title DecompContinuous Numeric approximation of Continuous Decomposition
#' 
#' @description Approximation of the continuous decomposition strategy proposed by Horiuchi et. al. (2008) for 
#' changes in \code{y} over time or some other dimension assuming gradual changes in the variables that determine
#' \code{y} between the two observation points. This is basically an R translation of the example MatLab program
#' provided by the authors in the supplementary material to the article, which as of this writing, was available
#' at: \url{http://www.demog.berkeley.edu/~jrw/Papers/decomp.suppl.pdf}. The only major difference is 
#' in the way function variables must be specified. The MatLab program asks for 
#' covariates in a single vector. This R implementation asks for covariates in a matrix, with columns specifying 
#' variables, and rows specifying age (or other) classes.
#' 
#' @param func A function specified by the user. This must take \code{rates1} or \code{rates2} as its argument, 
#' and return the value of the function, \code{y}, when evaluated for these rates. The function can also contain 
#' other arguments as parameters not to be decomposed.
#' @param rates1 These are the covariate matrices to be passed on as arguments to \code{func()}. 
#' Columns separate variables and rows separate ages.
#' @param rates2 just like \code{rates1} but for a different population, or a different time point.
#' @param N The number of intervals to integrate over. If \code{rates1} are observations from 2005 and 
#' \code{rates2} are observations from 2006 an \code{N} of 20 would imply a delta of 1/20 of a year for each 
#' integration step. Higher \code{N} should be asymptotically better, but may take longer to compute. In general,
#' there are decreasing returns to higher \code{N}.
#' @param ... Optional items to pass on to \code{func()} but not to be decomposed.
#' 
#' @details The decomposition works by assuming a linear change in all covariates between time 1 and time 2 (or
#' population 1 and population 2). At each small time step approaching time 2 (the size of which is the inverse 
#' of \code{N}) each covariate is moved forward along its linear trajectory. One at a time, each covariate (of 
#' which there are ages*variables of) is switched out twice, once for its value at 1/2N forward and once for its 
#' value at 1/2N backward in time. The difference between \code{func()} evaluated with these two rate matrices 
#' is the change in \code{y} attributable to that particular covariate and that particular time step. 
#' Summing over all N time steps, we get the contribution to the difference of each covariate, 
#' \code{effectmat}. The sum of \code{effectmat} should come very close to \code{func(rates2)-func(rates1)}. 
#' The error decreases with larger \code{N}, but there is not much point in having an \code{N} higher 
#' than 100, and 20 is usually sufficient. This ought to be able to handle a very wide variety of functions.
#' 
#' @return returns \code{effectmat}, a matrix of the variable effects that is organized in the same way as rates1
#' and rates2. Variables across columns and ages down rows. \code{sum(effectmat)} ought to approximate 
#' \code{func(rates2)-func(rates1)}.
#' 
#' @references Horiuchi, Wilmoth and Pletcher (2008) A Decomposition Method Based on a Model of Continuous
#' Change. Demography. Vol. 45, (4) pp 785-801
#' 
#' @seealso See Also as \link{DecompContinuousOrig}, which is the exact R implementation of the Matlab
#'  code provided by Horiuchi et. al. (2008), and a bit more flexible.
#' 
#' @examples 
#' \dontrun{
#' library(DecompHoriuchi)
#' data(rates1)
#' data(rates2)
#' # DecompContinuous()
#' Epsilon <- DecompContinuous(R0,rates1,rates2,N=20,pfem=.49)
#' # how to interpret?:
#' # the difference in R0 given these rates is:
#' R0(rates2,.49)-R0(rates1,.49)
#' ' # which is also the SUM of Epsilon:
#' sum(Epsilon)
#' # and when we look at the individual components of Epsilon, we see their contributions to the change:
#' # easiest to call barplot()
#'
#' # 1) flip the matrix on its side (wide)
#' Epsilon <- t(Epsilon)
#' # 2) separate negatives and positive:
#' EPSpos <- Epsilon * .5*(sign(Epsilon)+1)        
#' EPSneg <- Epsilon * .5*abs(sign(Epsilon)-1)
#' # 3) call barplot
#' barplot(EPSpos,width=rep(1,ncol(Epsilon)),space=0,ylim=range(Epsilon),main="A fake decomposition of R0",
#' col=c("yellow","green"),axisnames=F,xlim=c(0,90), ylab="contrib to change in R0",cex.axis=.7)
#' barplot(EPSneg,width=rep(1,ncol(Epsilon)),add=T,space=0,col=c("yellow","green"),axes=F,axisnames=F)
#' segments(seq(from=0,to=90,by=10),0,seq(from=0,to=90,by=10),-.027,lty=2,col="grey")
#' text(seq(from=0,to=90,by=10),-.027,seq(from=0,to=90,by=10),pos=1,xpd=T)
#' legend("bottomright",fill=c("yellow","green"),legend=c("contrib from change in Lx",
#' "contrib from change in #' Fx"),title="age specific contrib of changes in Fx and Lx",bg="white") }
#' 
#' @export  

DecompContinuous <-
function(func,rates1,rates2,N,...){
	# number of interval jumps   
	L 			<- nrow(rates1) # number of ages
	P 			<- ncol(rates1) # number of factors
	d 			<- (rates2-rates1)
	deltascale 	<- .5:(N-.5)/N
	effectmat 	<- rates1*0
	for (k in 1:N){ # over intervals
		# this part implements the assumption that the other rates (all ages and factors)
		# are also changing linearly in sync
		ratesprop <- rates1 + d * deltascale[k]
		for (i in 1:P){ # across effects
			deltaiak     	<- 0
			deltaia     	<- rep(0,L)
			for (a in 1:L){ # down ages
				# now, we select a single element, [a,i] and increment it forward by .5 delta
				# and also bring it backward by .5 delta
				ratesminus        	<-     ratesplus        <- ratesprop
				ratesplus[a,i]     	<-     ratesplus[a,i] + (d[a, i] / (2 * N))
				ratesminus[a,i] 	<-     ratesminus[a,i] - (d[a, i] / (2 * N))
				# the difference between the funciton evaluated with these rates is the change in y
				# due to this age (La), factor (Pi) and interval (Nk)
				deltaiak         	<-     func(ratesplus,...) - func(ratesminus,...)
				# for this factor and interval, we define a vector of these mini-deltas over age
				deltaia[a]         	<-     deltaiak
			}
			# and when age is done looping we add it to the effects matrix in the appropriate column
			# the the effects matrix sums over all intervals (split according to N- bigger N = finer)
			effectmat[,i] <- effectmat[,i] + deltaia
		}
	}
	return(effectmat)
}

#' @title DecompHoriuchi 2 functions to approximate the continuous decomposition method proposed by Horiuchi et. al. (2008).
#' 
#' @description This package contains two functions for continuous decomposition. One, a direct R 
#'  of the Matlab code offered by Horiuchi et al (2008) (\code{DecompContinuousOrig()}), and another
#'  a slightly friendlier but less flexible implementation (\code{DecompContinuous()}).
#' 
#' @references Horiuchi, Wilmoth and Pletcher (2008) A Decomposition Method Based on a Model of 
#' Continuous Change. Demography. Vol. 45, (4) pp 785-801
#' Also see the supplementary materials at 
#' \url{http://www.demog.berkeley.edu/~jrw/Papers/decomp.suppl.pdf}, 
#' which includes the MatLab program from the authors.
#' 
#' @examples 
#' \dontrun{
#' library(DecompHoriuchi)
#' data(rates1)
#' data(rates2)
#' ################################################################
#' # DecompContinuous()
#' Epsilon <- DecompContinuous(R0,rates1,rates2,N=20,pfem=.49)
#' # how to interpret?:
#' # the difference in R0 given these rates is:
#' R0(rates2,.49)-R0(rates1,.49)
#' # which is also the SUM of Epsilon:
#' sum(Epsilon)
#' # and when we look at the individual components of Epsilon, we see their contributions to the change:
#' # easiest to call barplot()
#' 
#' # 1) flip the matrix on its side (wide)
#' Epsilon <- t(Epsilon)
#' # 2) separate negatives and positive:
#' EPSpos <- Epsilon * .5*(sign(Epsilon)+1)        
#' EPSneg <- Epsilon * .5*abs(sign(Epsilon)-1)
#' # 3) call barplot
#' barplot(EPSpos,width=rep(1,ncol(Epsilon)),space=0,ylim=range(Epsilon),main="A fake decomposition of R0",
#' col=c("yellow","green"),axisnames=F,xlim=c(0,90),ylab="contrib to change in R0",cex.axis=.7)
#' barplot(EPSneg,width=rep(1,ncol(Epsilon)),add=T,space=0,col=c("yellow","green"),axes=F,axisnames=F)
#' segments(seq(from=0,to=90,by=10),0,seq(from=0,to=90,by=10),-.027,lty=2,col="grey")
#' text(seq(from=0,to=90,by=10),-.027,seq(from=0,to=90,by=10),pos=1,xpd=T)
#' legend("bottomright",fill=c("yellow","green"),legend=c("contrib from change in Lx",
#' "contrib from change in Fx"),title="age specific contrib of changes in Fx and Lx",bg="white")
#' ####################################################
#' # DecompContinuousOrig()
#' # put rates into vectors,
#' rates1 <- c(rates1)
#' rates2 <- c(rates2)
#' # here we call R0vec instead of R0:
#' A <- DecompContinuousOrig(func=R0vec,rates1=rates1,rates2=rates2,N=10,pfem=.49)
#' 
#' # reorder A into a matrix (sideways):
#' A <- t(matrix(A,ncol=2))
#' # call barplot() twice, once for positive values and again for negative values
#' Apos <- A * .5*(sign(A)+1)      
#' Aneg <- A * .5*abs(sign(A)-1)   
#' barplot(Apos,width=rep(1,length(A)/2),space=0,ylim=range(A),main="A fake decomposition of R0",
#' col=c("yellow","green"),axisnames=F,xlim=c(0,90),ylab="contrib to change in R0",cex.axis=.7)
#' barplot(Aneg,width=rep(1,length(A)/2),add=T,space=0,col=c("yellow","green"),axes=F,axisnames=F)
#' segments(seq(from=0,to=90,by=10),0,seq(from=0,to=90,by=10),-.027,lty=2,col="grey")
#' text(seq(from=0,to=90,by=10),-.027,seq(from=0,to=90,by=10),pos=1,xpd=T)
#' legend("bottomright",fill=c("yellow","green"),legend=c("contrib from change in Lx",
#' "contrib from change in Fx"),title="age specific contrib of changes in Fx and Lx",bg="white")
#' # (same plot)}
#' 
#' @docType package
#' 
#' @name DecompHoriuchi
NULL

#' @title Fake data generated for example.
#' @description
#' The first column, Lx, is a discrete survival function for time point 1. The second column are age 
#' specific fertility rates.
#' 
#' @examples 
#' \dontrun{
#' library(DecompHoriuchi)
#' data(rates1)
#' data(rates2)
#' # nothing fancy
#' # compare Lx
#' plot(rates1[,1],type='l',col="blue")
#' lines(rates2[,1],col="green")
#' # compare Fx
#' plot(rates1[,2],type='l',col="blue")
#' lines(rates2[,2],col="green") }
#' 
#' @keywords datasets
#' 
#' @name rates1 
#' @docType data
NULL

#' @title Fake data generated for example.
#' @description
#' The first column, Lx, is a discrete survival function for time point 1. The second column are age 
#' specific fertility rates.
#' 
#' @examples 
#' \dontrun{
#' library(DecompHoriuchi)
#' data(rates1)
#' data(rates2)
#' # nothing fancy
#' # compare Lx
#' plot(rates1[,1],type='l',col="blue")
#' lines(rates2[,1],col="green")
#' # compare Fx
#' plot(rates1[,2],type='l',col="blue")
#' lines(rates2[,2],col="green") }
#' 
#' @keywords datasets
#' @name rates2 
#' @docType data 
NULL
