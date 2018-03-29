setValidity("BeadStudioSetList", function(object){
	nms <- ls(assayData(object))
	if(!all(c("baf", "lrr") %in% nms)){
		msg <- "baf and lrr are required elements of the assayData"
		return(msg)
	}
	if(length(object) > 0){
		msg <- validAssayDataDims(assayData(object))
		if(!all(msg == TRUE)) return(msg)
		elt <- (ls(assayDataList(object)))[[1]]
		b <- assayDataList(object)[[elt]]
		if(length(chromosome(object)) != length(b)){
			return("chromosome slot must be the same length as the length of the list for each assayData element")
		}
	}
	if(!identical(sampleNames(object), sampleNames(phenoData(object)))){
		stop("sampleNames of 'BeadStudioSetList' object must be the same as the sampleNames of the phenoData")
	}
	if(length(featureDataList(object)) != length(chromosome(object))){
		return("each chromosome should have an element in the featureDataList")
	}
	if(length(featureDataList(object)) > 0){
		featureDataClasses <- sapply(featureDataList(object), class)
		if(!unique(featureDataClasses) == "GenomeAnnotatedDataFrame"){
			return("featureDataList must be comprised of GenomeAnnotatedDataFrame(s)")
		}
	}
})

#' Defunct functions/classes/methods in the VanillaICE package
#'
#' The function, class, or data object you asked is defunct.
#'
#' @name HmmOptionList-class
#' @name HmmOptionList
#' @aliases HmmOptionList
#' @keywords internal
#' @rdname Defunct
#' @export
HmmOptionList <- function(...) .Defunct(msg="HmmOptionList class is defunct")

#' @rdname Defunct
#' @name hmm.setup
#' @aliases hmm.setup
hmm.setup <- function(...) .Defunct("hmm.setup function is defunct.")




#' @param object see \code{showMethods(hmm)} for a listing of classes for which this method is defined
#' @rdname Deprecated
#' @export
#' @aliases hmm,BafLrrSetList-method hmm,BeadStudioSet-method hmm,BeadStudioSetList-method hmm,SnpSet2-method hmm,oligoSetList-method hmm,oligoSnpSet-method
setGeneric("hmm", function(object, ...) standardGeneric("hmm"))

setGeneric("gtEmission", function(object, hmm.params, gt.conf, is.snp, cdfName, ...) standardGeneric("gtEmission"))
setGeneric("nStates", function(object) standardGeneric("nStates"))
setGeneric("normalStateIndex", function(object) standardGeneric("normalStateIndex"))
setGeneric("backwardVariable", function(object) standardGeneric("backwardVariable"))
setGeneric("forwardVariable", function(object) standardGeneric("forwardVariable"))
setGeneric("transitionProb", function(object) standardGeneric("transitionProb"))
setGeneric("viterbiStatePath", function(object) standardGeneric("viterbiStatePath"))
setGeneric("scaleFactor", function(object) standardGeneric("scaleFactor"))
setGeneric("fit", function(object) standardGeneric("fit"))
setGeneric("initialProb", function(object) standardGeneric("initialProb"))
setGeneric("normal2altered", function(object) standardGeneric("normal2altered"))
setGeneric("altered2normal", function(object) standardGeneric("altered2normal"))
setGeneric("altered2altered", function(object) standardGeneric("altered2altered"))
setGeneric("delta", function(object) standardGeneric("delta"))
setGeneric("pAA", function(object) standardGeneric("pAA"))


#' Constraints for updating the means of the copy number states
#'
#' @param mu numeric vector of means
#' @return numeric vector of means
#' @rdname constraints
#' @export
#' @keywords manip
constrainMu2 <- function(mu){
  mu[3] <- ifelse(mu[3] < -0.2, -0.2, mu[3])
  mu[3] <- ifelse(mu[3] > 0.2, 0.2, mu[3])
  d <- mu[3]-mu[2]
  if(d < 0.4)
    mu[2] <- mu[3]-0.4
  d <- mu[2]-mu[1]
  if(d < 1)
    mu[1] <- mu[2]-1
  d <- mu[4]-mu[3]
  if(d < 0.4)
    mu[4] <- mu[3] + 0.4
  d <- mu[5]-mu[4]
  if(d < 0.25)
    mu[5] <- mu[4] + 0.25
  return(mu)
}

#' Constraints for updating the standard deviations of the BAFs
#'
#' @param sigma numeric vector of standard deviations (non-negative)
#' @return constrained standard deviations
#' @rdname constraints
#' @export
#' @keywords manip
constrainSd2 <- function(sigma){
  sigma[1] <- 3*sigma[3]
  sigma[2:5] <- sigma[3]
  sigma[6] <- max(sigma[6], sigma[3])
  return(sigma)
}

#' Deprecated function in VanillaICE
#'
#' The function read.bsfiles  has been deprecated.  This function
#' is provided only for compatability with older versions and will be
#' defunct at the next release.
#'
#' Use the replacement function fread instead.
#'
#' @aliases read.bsfiles
#' @keywords internal
#' @rdname read.bsfiles
#' @export
read.bsfiles <- function(...) {
  .Deprecated("fread")
  fread(...)
}

format_genotypes <- function(allele1, allele2){
  gt <- paste(allele1, allele2, sep="")
  as.integer(factor(gt, levels=c("AA", "AB", "BB")))
}

## ad-hoc.  Do we want to put priors on the means?
constrainMu <- function(mu, is.log){
  if(is.log){
    is.ratio <- all.equal(mu[3], 0, tolerance=0.2) == TRUE
    if(is.ratio){
      mu[3] <- ifelse(mu[3] < -0.2, -0.2, mu[3])
      mu[3] <- ifelse(mu[3] > 0.2, 0.2, mu[3])
      mu[1] <- ifelse(mu[1] > -1, -1, mu[1])
      mu[2] <- ifelse(mu[2] > -0.25, -0.25, mu[2])
      mu[4] <- ifelse(mu[4] < 0.25, 0.25, mu[4])
      mu[4] <- ifelse(mu[4] > 0.6, 0.6, mu[4])
      mu[5] <- ifelse(mu[5] < 0.65, 0.65, mu[5])
    } else {
      mu[3] <- ifelse(mu[3] < 0.75, 0.75, mu[3])
      mu[3] <- ifelse(mu[3] > 1.25, 1.25, mu[3])
      mu[1] <- ifelse(mu[1] > -0.5, -0.5, mu[1])
      mu[2] <- ifelse(mu[2] > 0.7, 0.7, mu[2])
      mu[4] <- ifelse(mu[4] < 1.25, 1.25, mu[4])
      mu[4] <- ifelse(mu[4] > 2, 2, mu[4])
      mu[5] <- ifelse(mu[5] < 2, 2, mu[5])
    }
  } else {
    mu[1] <- ifelse(mu[1] > 0.8, 0.8, mu[1])
    mu[2] <- ifelse(mu[2] > 1.7, 1.7, mu[2])
    mu[2] <- ifelse(mu[2] < 0.9, 0.9, mu[2])
    mu[3] <- ifelse(mu[3] > 2.2, 2.2, mu[3])
    mu[3] <- ifelse(mu[3] < 1.8, 1.8, mu[3])
    mu[4] <- ifelse(mu[4] < 2.3, 2.3, mu[4])
    mu[4] <- ifelse(mu[4] > 3, 3, mu[4])
    mu[5] <- ifelse(mu[5] < 3, 3, mu[5])
  }
  return(mu)
}

constrainSd <- function(sigma){
  sigma[1] <- min(3*sigma[3], max(sigma[1], sigma[3]))
  sigma[6] <- min(3*sigma[3], max(sigma[6], sigma[3]))
  sigma[5] <- min(3*sigma[3], max(sigma[5], sigma[3]))
  sigma[2:4] <- sigma[3]
  return(sigma)
}


computeTransitionProb <- function(x, TAUP, S, tauMAX=1-5e-6){
	p <- exp(-2*diff(x)/TAUP)
	minimum <- 1-1/((S-1)) + 0.01
	p <- pmax(p, minimum)
	p <- pmin(p, tauMAX)
	return(as.matrix(p))
}

#' Restricted range for CN values
#'
#' @param is.log whether copy number estimates are on log scale
#' @return numeric vector giving the lower and upper values of the restricted range
#' @rdname constraints
#' @export
#' @examples
#' copyNumberLimits(is.log=TRUE)
#' copyNumberLimits(is.log=FALSE)
#' @keywords manip
copyNumberLimits <- function(is.log) if(is.log) return(c(-2.5, 3)) else return(c(0,10))

thresholdCopyNumber <- function(object, limits){
  object <- pmin(object, limits[2])
  object <- pmax(object, limits[1])
  object
}

centerCopyNumber <- function(object, is.snp){
  snp.index <- which(is.snp)
  mu.snp <- apply(object[snp.index, , drop=FALSE], 2, median, na.rm=TRUE)
  object[snp.index, ] <- sweep(object[snp.index, , drop=FALSE],  2, mu.snp)
  if(any(!is.snp)){
    np.index <- which(!is.snp)
    mu.np <- apply(object[np.index, , drop=FALSE], 2, median, na.rm=TRUE)
    object[np.index, ] <- sweep(object[np.index, , drop=FALSE], 2, mu.np)
  }
  object
}

guessCnScale <- function(x){
  is.log <- if(all(x >= 0, na.rm=TRUE)) FALSE else TRUE
  is.log
}

copyNumberStates <- function(normalCn) {
  is.ratio <- all.equal(normalCn, 1, tolerance=0.2) == TRUE
  if(is.ratio){
    return(c(0, 1/2, 1, 1, 3/2, 4/2))
  } else{
    is.absolute <- all.equal(normalCn, 2, tolerance=0.2)==TRUE
    if(is.absolute){
      return(c(0, 1, 2, 2, 3, 4))
    } else{
      stop("median copy number is not near 1 or 2")
    }
  }
}




makeNonDecreasing <- function(x){
  d <- diff(x)
  if(all(d >= 0)) return(x)
  index <- which(d < 0)
  if(index[1] == 1){
    x[1] <- x[2]
    index <- index[-1]
    if(length(index) ==0)
      return(x)
  }
  l <- length(index)
  if(l == length(x)-1){
    i <- index[l]
    x[length(x)] <- x[length(x)-1]
    index <- index[-l]
    if(length(index) == 0) return(x)
  }
  x[index+1] <- x[index]
  return(x)
}

getPrB <- function(pB=NULL){
  ##n <- length(x)
  if(is.null(pB)){
    pB <- 0.5
  }
  pA <- 1-pB
  pAA <- pA^2
  pAB <- 2*pA*pB
  pBB <- 1-pAA-pAB
  pAAA <- pA^3
  pAAB <- 3*pA^2*pB
  pABB <- 3*pA*pB^2
  pBBB <- 1-pAAA-pAAB-pABB
  pAAAA <- pA^4
  pAAAB <- 4*pA^3*pB
  pAABB <- 6*pA^2*pB^2
  pABBB <- 4*pA*pB^3
  pBBBB <- pB^4
  p <- matrix(c(pB,
                pA,
                pAA,
                pAB,
                pBB,
                pAAA,
                pAAB,
                pABB,
                pBBB,
                pAAAA,
                pAAAB,
                pAABB,
                pABBB,
                pBBBB),
              byrow=FALSE, nrow=length(pB), ncol=14)
  colnames(p) <- c("B", "A", "AA", "AB", "BB",
                   "AAA", "AAB", "ABB", "BBB",
                   "AAAA", "AAAB", "AABB", "ABBB", "BBBB")
  return(p)
}

generatorFunG <- function(r, gt, is.snp, cnStates,
			  normalIndex, TAUP, limits, center,
			  position, is.log, computeLLR, chrom, verbose=FALSE){
	S <- length(cnStates)
	nc <- ncol(r)
	nr <- nrow(r)
	np.index <- which(!is.snp)
	CHR <- paste("chr", oligoClasses::integer2chromosome(chrom), sep="")

	## params for copy number
	sds <- apply(r, 2, mad, na.rm=TRUE)
	if(center) r <- centerCopyNumber(r, is.snp) + cnStates[normalIndex]
	r <- thresholdCopyNumber(r, limits=limits)

	initialCnParams <- function(j){
		mus <- cnStates
		sigmas <- rep(sds[j], S)
		##p <- matrix(c(0.99,  0.01), S, 2, byrow=TRUE)
		p <- 0.01
		paramsCN <- list(mu=mus, sigmas=sigmas, p=p)
	}

	g2 <- g1 <- matrix(NA, nr, S)
	d1 <- matrix(NA, nr, S)
	emitr <- matrix(NA, nr, S)
	cn.dunif <- dunif(r, limits[1], limits[2])
	Gtotal <- matrix(NA, nr, 4)

	## might try dt here instead
	mydnorm <- function(x){
		function(mean, sd){
			mus <- matrix(mean, nr, S, byrow=TRUE)
			sds <- matrix(sd, nr, S, byrow=TRUE)
			dnorm(x, mus, sds)
		}
	}

	updateCnEmission <- function(cn, params, j){
		mus <- params[["mu"]]
		sds <- params[["sigmas"]]
		p <- params[["p"]]
		q <- 1-p
		normalDens <- mydnorm(cn)
		d <- normalDens(mus, sds)
		emitr <- q*d + p*cn.dunif[, j]
		emitr.na <- is.na(emitr)
		hasna <- any(emitr.na)
		if(hasna) emitr[emitr.na] <- 1
		return(emitr)
	}
	cn.sd.new <- rep(NA, S)
	updateCnParams <- function(cn, params, fv, bv, j) {
		mu <- params[["mu"]]
		sigma <- params[["sigmas"]]
		p <- params[["p"]]; q <- 1-p
		min.sd <- sds[j]/2

		h <- matrix(fv*bv, nr, S)
		m <- rowSums(h, na.rm=TRUE)
		h <- h/m

		d.unif <- cn.dunif[, j]
		d <- q*dnorm(cn, mean=matrix(mu, nr, S, byrow=TRUE), sd=matrix(sigma, nr, S, byrow=TRUE)) + p*d.unif
		d1 <- d/(d+d.unif)

		g1 <- h * d1
		g2 <- h * (1-d1)

		totalh <- apply(g1, 2, sum, na.rm=TRUE)
		mu.new <- colSums(g1*cn, na.rm=TRUE)/totalh
		mu.new[-4] <- constrainMu(mu.new[-4], is.log)
		mu.new[4] <- mu.new[normalIndex]
		mu.new <- makeNonDecreasing(mu.new)

		## For loop
		for(s in seq_len(S))  cn.sd.new[s] <- sqrt(sum(g1[, s]*(cn-mu.new[s])^2, na.rm=TRUE)/totalh[s])
		cn.sd.new <- pmax(cn.sd.new, min.sd)
		cn.sd.new <- constrainSd(cn.sd.new)
		params[["mu"]] <- mu.new
		params[["sigmas"]] <- cn.sd.new

		## update p as the proportion of values not fit by any of the states
		total.g2 <- apply(g2, 2, sum, na.rm=TRUE)
		denom.eq52 <- apply(g1 + g2, 2, sum, na.rm=TRUE)
		p <- min(total.g2/denom.eq52)  ## altered
		params[["p"]] <- p
		params
	}
	#
	transitionPr <- function(TAUP, tauMAX=1-5e-8){
		##
		## probability that state at marker t+1 is the same as
		## at marker t
		##
		d <- diff(position)
		p <- exp(-2*d/TAUP)
		minimum <- 1-1/((S-1)) + 0.01
		p <- pmax(p, minimum)
		p <- pmin(p, tauMAX)
		if(any(d < 0)) p[d < 0] <- 0.5 ## different chromosomes
		return(as.matrix(p))
	}
	tau <- transitionPr(TAUP)
	arm <- integer(nr)
	statePath <- integer(nr)
	scaleFactor <- rep(0, nr)
	bv <- fv <- matrix(0.0, nr*S, 1L) ## forward and backward variables
	initialProb <- rep(1/S, S)

	fitViterbi <- function(emit){
		emit <- as.matrix(as.numeric(emit))
		res <- .C("viterbi2", emit=emit, pi=initialProb,
			  tau=tau,
			  arm=arm, S=S, nr=nr, statePath=statePath,
			  fv=fv, bv=bv, 3L,
			  scaleFactor=scaleFactor)[c("statePath", "fv", "bv", "scaleFactor")]
		statePath <- res[["statePath"]]
		sf <- res[["scaleFactor"]]
		fv <- res[["fv"]]
		bv <- res[["bv"]]
		res <- list(statePath=statePath, fv=fv, bv=bv, sf=sf)
	}

	## computeLLR
	CHR <- paste("chr", integer2chromosome(chrom), sep="")
	toGRanges <- function(statePath, j){
		id <- colnames(r)[j]
		rl <- Rle(statePath)
		starts <- position[start(rl)]
		ends <- position[end(rl)]
		states <- statePath[start(rl)]
		GRanges(CHR, IRanges(starts, ends), numberProbes=width(rl), state=states, sample=id)
	}
	##
	if(computeLLR){
		log.initial <- log(initialProb)
		tauC <- 1-tau ## probability that state t+1 != state t
		## divy up between the number of different states
		## tauC2 <- tauC/(S-1)
		##lP.N2N <- log(1-(tauC*(S-1))) ##probability normal -> normal
		lP.N2N <- log(tau)
		lP.N2A <- log(tauC) ##probability normal -> altered
		##P.A2A <- sapply(1-(tauC*(1+(S-2))), function(x) max(x, 0.01))
		##lP.A2A <- log(P.A2A) ## probability altered to same altered state
		lP.A2A <- lP.N2N
		lP.A2N <- lP.N2A ##probability altered -> normal
		lP.A2Astar <- lP.N2A ## probability altered -> different altered state
		## featureRanges
		if(length(CHR)==1){
			fr <- GRanges(rep(CHR, length(position)), IRanges(position, width=1))
		} else fr <- GRanges(CHR, IRanges(position, width=1))
	}
	##
	computeLogLikRatio <- function(gr, emit){
		gr2 <- gr
		alt.index <- which(state(gr) != normalIndex & numberProbes(gr) > 1)
		if(length(alt.index)==0) return(rep(0.0, length(gr)))
		gr <- gr[alt.index, ]
		log.emission <- log(emit)
		L <- length(gr)
		LLR <- rep(NA,  L)
		olaps <- findOverlaps(gr, fr)
		index <- subjectHits(olaps)
		indexList <- split(index, queryHits(olaps))
		starts <- sapply(indexList, min)
		ends <- sapply(indexList, max)
		statePath <- as.integer(state(gr))
		T <- length(statePath)
		rangeLogLik <- function(from, to, thisState){
			index <- seq(from, to)
			index2 <- index[-1]## t=2, ...., t*
			if(from == 1){
				if(to < T){
					logLik.vit <- log.initial[thisState] + log.emission[from, thisState]  + sum(lP.A2A[index2] + log.emission[index2, thisState]) + lP.A2N[to+1] + log.emission[to+1, normalIndex]
					logLik.null <- log.initial[normalIndex] + log.emission[from, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex]) + lP.N2N[to+1] + log.emission[to+1, normalIndex]
				} else {
					logLik.vit <- log.initial[thisState] + log.emission[from, thisState]  + sum(lP.A2A[index2] + log.emission[index2, thisState]) ##+ lP.A2N[last.index+1] + log.emission[last.index+1, normalIndex]
					logLik.null <- log.initial[normalIndex] + log.emission[from, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex]) ##+ lP.N2N[last.index+1] + log.emission[last.index+1, normalIndex]
				}
			} else { ## first index > 1
				index2 <- index[-1]
				if(to < T){
					logLik.vit <- lP.N2A[from] +
						sum(lP.A2A[index2]) +
							lP.A2N[to+1] +
								log.emission[from, thisState] +
									sum(log.emission[index2, thisState]) +
										log.emission[to+1, normalIndex]
					logLik.null <-
						sum(lP.N2N[index]) +
							lP.N2N[to+1]  +
								sum(log.emission[index, normalIndex]) +
									log.emission[to+1, normalIndex]
				} else {
					logLik.vit <- lP.N2A[from] + log.emission[from, thisState] + sum(lP.A2A[index2] + log.emission[index2, thisState])
					logLik.null <- lP.N2N[from] + log.emission[from, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex])
				}
			}
			LLR <- logLik.vit - logLik.null
			return(LLR)
		}
		states <- state(gr)
		for(k in seq_len(L)) LLR[k] <- rangeLogLik(from=starts[k], to=ends[k], thisState=states[k])
		res <- rep(0.0, length(gr2))
		res[alt.index] <- LLR
		return(res)
	}
	list(initialCnParams=initialCnParams,
	     updateCnEmission=updateCnEmission,
	     updateCnParams=updateCnParams,
	     trPr=transitionPr,
	     fitViterbi=fitViterbi,
	     toGRanges=toGRanges,
	     computeLogLikRatio=computeLogLikRatio)
}

validChromosomeIndex <- function(object){
	index <- which(chromosome(object) <= 24 & !is.na(chromosome(object)) & !is.na(position(object)))
	if(length(index) < 1){
		stop("Chromosome must be 1-24 and physical position \n
                      can not be missing.  See chromosome() and position().\n
                      See integer2chromosome(1:24) for integer codes used by\n
                      VanillaICE.")
	}
	return(index)
}

makeMusBafNondecreasing <- function(mus){
  nms <- names(mus)
  index <- match(c("A", "AAAB", "AAB", "AB", "ABB", "ABBB", "B"), names(mus))
  mus2 <- mus[index]
  mus2[2] <- if(mus2[2] > mus2[3]) mus2[3] else mus2[2]
  mus2[3] <- if(mus2[3] > mus2[4]) mus2[4] else mus2[3]
  mus2[5] <- if(mus2[5] < mus2[4]) mus2[4] else mus2[5]
  mus2[6] <- if(mus2[6] < mus2[5]) mus2[5] else mus2[6]
  mus2
}

.getArm <- function(chrom, pos, genome){
  if(is.integer(chrom)) chrom <- paste("chr", integer2chromosome(chrom), sep="")
  path.gap <- system.file("extdata", package="SNPchip")
  gaps <- readRDS(list.files(path.gap, pattern=paste("gap_", genome, ".rda", sep=""), full.names=TRUE))
  centromere.starts <- start(gaps)
  centromere.ends <- end(gaps)
  names(centromere.ends) <- names(centromere.starts) <- seqnames(gaps)
  centromere.starts <- centromere.starts[chrom]
  centromere.ends <- centromere.ends[chrom]
  chr.arm <- arm <- rep(NA, length(pos))
  arm[pos <= centromere.starts] <- "p"
  arm[pos >= centromere.ends] <- "q"
  ##arm <- ifelse(pos <= centromere.starts, "p", "q")
  chr.arm[!is.na(arm)] <- paste(chrom[!is.na(arm)], arm[!is.na(arm)], sep="")
  chr.arm
}
generatorViterbiBeadStudioSet <- function(R, B, chr, center,
					  snp.index, anyNP,
					  cnStates){
  chr <- chr
  R <- R
  B <- B
  toMatrix <- function(j){
    r <- R[, j, drop=FALSE]/100
    b <- B[, j, drop=FALSE]/1000
    if(anyNP) b <- b[snp.index, , drop=FALSE]
    rownames(r) <- rownames(b) <- NULL
    if(center){
      if(all(as.integer(chr) <= 22)){
        meds <- colMedians(r, na.rm=TRUE)
        r2 <- sweep(r, 2, meds)
        r <- r2+cnStates[3]
      } else{
        autosome.index <- which(chr <= 22)
        rauto <- r[autosome.index, ]
        meds <- colMedians(rauto, na.rm=TRUE)
        rauto <- sweep(rauto, 2, meds)
        rauto <- rauto+cnStates[3]
        r[autosome.index, ] <- rauto
      }
    }
    close(R)
    close(B)
    list(r=r, b=b)
  }
  return(toMatrix)
}

generatorViterbiCnGt <- function(R, G, chr, center,
				 snp.index, anyNP,
				 cnStates){
	chr <- chr
	R <- R
	G <- G
	toMatrix <- function(j){
		r <- R[, j, drop=FALSE]/100
		g <- G[, j, drop=FALSE]
		if(anyNP) g <- g[snp.index, , drop=FALSE]
		rownames(r) <- rownames(g) <- NULL
		if(center){
			if(all(as.integer(chr) <= 22)){
				meds <- colMedians(r, na.rm=TRUE)
				r2 <- sweep(r, 2, meds)
				r <- r2+cnStates[3]
			} else{
				autosome.index <- which(chr <= 22)
				rauto <- r[autosome.index, ]
				meds <- colMedians(rauto, na.rm=TRUE)
				rauto <- sweep(rauto, 2, meds)
				rauto <- rauto+cnStates[3]
				r[autosome.index, ] <- rauto
			}
		}
		close(R)
		close(G)
		list(r=r, g=g)
	}
	return(toMatrix)
}

generatorViterbiSnpSet <- function(G, chr, isff){
	chr <- chr
	G <- G
	toMatrix <- function(j){
		g <- G[, j, drop=FALSE]
		if(isff) close(G)
		return(g)
	}
	return(toMatrix)
}

generatorViterbiSnpSetIce <- function(G, GP, chr, isff){
	chr <- chr
	G <- G
	GP <- GP
	toMatrix <- function(j){
		g <- G[, j, drop=FALSE]
		gp <- GP[, j, drop=FALSE]
		gp <- i2p(gp)
		if(isff) {
			close(G)
			close(GP)
		}
		return(list(g=g, gt.conf=gp))
	}
	return(toMatrix)
}


generatorGRanges <- function(chrom, position, build, ids, TAUP, tauMAX){
  S <- 6
  CHR <- paste("chr", oligoClasses::integer2chromosome(chrom), sep="")
  chrarm <- .getArm(as.integer(chrom), position, build)
  chrarm <- factor(chrarm, unique(chrarm))
  sl <- getSequenceLengths(build)
  sl <- sl[unique(CHR)]
  ##
  toGRanges <- function(statePath, id){##, j){
    sp <- split(statePath, chrarm)
    rlist <- lapply(sp, Rle)
    pos <- split(position, chrarm)
    x <- x2 <- x1 <- rl <- states <- NULL
    starts <- foreach(rl=rlist, x=pos) %do% x[start(rl)]
    ends <- foreach(rl=rlist, x=pos) %do% x[end(rl)]
    statelist <- foreach(states=sp, rl=rlist) %do% states[start(rl)]
    chrlist <- split(CHR, chrarm)
    chrl <- foreach(chrom=chrlist, rl=rlist) %do% chrom[start(rl)]
    gr <- foreach(states=statelist,
                  x1=starts,
                  x2=ends,
                  rl=rlist,
                  chrom=chrl) %do% {
                    GRanges(chrom, IRanges(x1, x2),
                            numberProbes=width(rl),
                            sample=id,
                            state=states,
                            seqlengths=sl)
                  }
    gr <- unlist(GRangesList(gr))
  }
  if(missing(tauMAX)) tauMAX <- 1-5e-8
  d <- diff(position)
  p <- exp(-2*d/TAUP)
  minimum <- 1-1/((S-1)) + 0.01
  p <- pmax(p, minimum)
  p <- pmin(p, tauMAX)
  ##
  ## 1-(1-tau)*(S-1)*c1, c1=1.  can't be less than 0.8
  ##
  if(any(d < 0)) p[d < 0] <- 0.8 ## different chromosomes
  tau <- p; rm(p)

  initialProb <- rep(1/S, S)
  ##log.initial <- log(initialProb)
  tauC <- 1-tau ## probability that state t+1 != state t
  lP.N2N <- log(tau)
  lP.N2A <- log(tauC) ##probability normal -> altered
  lP.A2A <- lP.N2N
  lP.A2N <- lP.N2A ##probability altered -> normal
  lP.A2Astar <- lP.N2A ## probability altered -> different altered state
  if(length(CHR)==1){
    fr <- GRanges(rep(CHR, length(position)), IRanges(position, width=1))
  } else fr <- GRanges(CHR, IRanges(position, width=1))
  transitionProbs <- list(lP.N2N=lP.N2N,
                          lP.N2A=lP.N2A,
                          lP.A2A=lP.A2A,
                          lP.A2N=lP.A2N,
                          lP.A2Astar=lP.A2Astar,
                          fr=fr)
  funs <- list(toGRanges=toGRanges,
               tau=tau,
               transitionProbs=transitionProbs)
  return(funs)
}

generatorViterbi <- function(rlist, blist, chr, center, snp.index, anyNP){
	chr <- chr
	rlist <- rlist
	blist <- blist
	toMatrix <- function(j){
		rl <- lapply(rlist, function(x, j) x[, j, drop=FALSE], j=j)
		bl <- lapply(blist, function(x, j) x[, j, drop=FALSE], j=j)
		r <- do.call("rbind", rl)/100
		b <- do.call("rbind", bl)/1000
		if(anyNP) b <- b[snp.index, , drop=FALSE]
		rownames(r) <- rownames(b) <- NULL
		if(center){
			if(all(as.integer(chr) <= 22)){
				meds <- colMedians(r, na.rm=TRUE)
				r <- sweep(r, 2, meds)
			} else{
				autosome.index <- which(chr <= 22)
				rauto <- r[autosome.index, , drop=FALSE]
				meds <- colMedians(rauto, na.rm=TRUE)
				r[autosome.index, ] <- sweep(rauto, 2, meds)
			}
		}
		sapply(rlist, close)
		sapply(blist, close)
		list(r=r, b=b)
	}
	return(toMatrix)
}

generatorFun <- function(r, b, gt, snp.index, cnStates,
			 normalIndex, tau, limits, center,
			 prOutlierBAF, p.hom, is.log,
			 computeLLR, verbose=FALSE,
			 transitionProbs){
  S <- length(cnStates)
  nc <- ncol(r)
  ## remove nonpolymorphic markers
  ## b <- b[snp.index, , drop=FALSE]
  nr <- nrow(r)
  nb <- nrow(b)
  ##np.index <- which(!is.snp)
  names(prOutlierBAF)[1] <- "prOutlier"
  ##CHR <- paste("chr", oligoClasses::integer2chromosome(chrom), sep="")
  ##
  ## params for copy number
  sds <- rep(NA, ncol(r))
  for(j in seq_along(sds)){
    i <- r[, j] != 0
    sds[j] <- sd(r[i,j], na.rm=TRUE)
  }
  ##sds <- apply(r, 2, mad, na.rm=TRUE)
  ##if(center) r <- centerCopyNumber(r, is.snp) + cnStates[normalIndex]
  initialCnParams <- function(j){
    mus <- cnStates
    sigmas <- rep(sds[j], S)
    ## only works if the chromosome is not a 2-copy gain
    ## iHets <- which(b[, j] > 0.45 & b[, j] < 0.55)
    ## pHets <- length(iHets)/nr
    ## if(pHets > 0.1) mus[c(3,4)] <- median(r[iHets], na.rm=TRUE)
    ##mus[5] <- mus[3] + 2*sds[j]
    ##mus[6] <- mus[3] + 3*sds[j]
    ##p <- matrix(c(0.99,  0.01), S, 2, byrow=TRUE)
    p <- 0.01
    paramsCN <- list(mu=mus, sigmas=sigmas, p=p)
  }
  ## params for BAF
  allele.prob <- getPrB()
  bindex <- b[, 1] > 0 & b[, 1] < 1
  initialBafParams <- function(){
    musBAF <- c(0, 0.1, 1/3, 0.5, 2/3, 0.9, 1)
    ## we have already removed the nonpolymorphic markers
    index <- b[, 1] < 0.25
    sdA <- sd(b[index & bindex, 1], na.rm=TRUE)
    sdsBAF <- c(sdA, rep(sdA*1.5, 5), sdA)
    names(musBAF) <- names(sdsBAF) <- c("A", "AAAB", "AAB", "AB", "ABB", "ABBB", "B")
    paramsBAF <- list(mus=musBAF, sigmas=sdsBAF, prOutlier=prOutlierBAF)
  }
  g2 <- g1 <- matrix(NA, nb, S)
  d1 <- matrix(NA, nb, S)
  isHom <- b < 0.02 | b > 0.98
  indexHom <- list()
  for(j in seq_len(nc)) indexHom[[j]] <- which(isHom[, j])
  emitr <- matrix(NA, nr, S)
  emitb <- matrix(NA, nb, S)
  cn.dunif <- dunif(r, limits[1], limits[2])
  Gtotal <- matrix(NA, nb, 4)
  updateBafEmission <- function(bf, params, j){
    mus <- params[["mus"]]
    sds <- params[["sigmas"]]
    ##prOutlier <- params$prOutlier$initial
    prOutlier <- params$prOutlier$prOutlier
    computeEmitPr <- function(dlist, prOutlier, allele.prob){
      sumd <- 0
      q <- 1-prOutlier
      for(i in seq_along(dlist)){
        sumd <- sumd + dlist[[i]] * q*allele.prob[i]
      }
      ##p <- matrix(prOutlier, nc, nr, byrow=TRUE)
      t(sumd+prOutlier)
    }
    trNormal <- trnorm(bf)
    ##
    ##---------------------------------------------------------------------------
    ## copy number 1
    dA <- trNormal(mus["A"], sds["A"])
    dB <- trNormal(mus["B"], sds["B"])
    emitb[, 2] <- computeEmitPr(list(dA,dB), prOutlier, allele.prob[, c("A", "B")])
    emitb[, 4] <- emitb[, 2]
    ##
    ##
    ##---------------------------------------------------------------------------
    ## copy number 2
    dAB <- trNormal(mus["AB"], sds["AB"])
    emitb[, 3] <- computeEmitPr(list(dA, dAB, dB), prOutlier=prOutlier, allele.prob=allele.prob[, c("AA", "AB", "BB")])
    ##
    ##---------------------------------------------------------------------------
    ## copy number 3
    dAAB <- trNormal(mus["AAB"], sds["AAB"])
    dABB <- trNormal(mus["ABB"], sds["ABB"])
    emitb[, 5] <- computeEmitPr(list(dA, dAAB, dABB, dB), prOutlier, allele.prob[, c("AAA", "AAB", "ABB", "BBB")])
    ##
    ##---------------------------------------------------------------------------
    ## copy number 4
    dAAAB <- trNormal(mus["AAAB"], sds["AAAB"])
    dABBB <- trNormal(mus["ABBB"], sds["ABBB"])
    emitb[, 6] <- computeEmitPr(list(dA, dAAAB, dAB, dABBB, dB), prOutlier, allele.prob[, c("AAAA", "AAAB", "AABB", "ABBB", "BBBB")])
    ##
    ##
    ## small p.hom makes homozygous genotypes less informative
    ##
    if(p.hom < 1){
      i <- indexHom[[j]]
      if(length(i) > 0) emitb[i, c(3, 5, 6)] <- (1-p.hom)*emitb[i, 2] + p.hom*emitb[i, c(3, 5, 6)]
    }
    ##if(length(np.index) > 0) emitb[np.index, ] <- 1
    nas <- anyMissing(emitb)
    if(nas) emitb[is.na(emitb)] <- 1
    ##index <- which(rowSums(is.infinite(emitb)) > 0)
    return(emitb)
  }
  ## might try dt here instead
  mydnorm <- function(x){
    function(mean, sd){
      mus <- matrix(mean, nr, S, byrow=TRUE)
      sds <- matrix(sd, nr, S, byrow=TRUE)
      dnorm(x, mus, sds)
    }
  }
  updateCnEmission <- function(cn, params, j){
    mus <- params[["mu"]]
    sds <- params[["sigmas"]]
    p <- params[["p"]]
    q <- 1-p
    normalDens <- mydnorm(cn)
    d <- normalDens(mus, sds)
    emitr <- q*d + p*cn.dunif[, j]
    emitr.na <- is.na(emitr)
    hasna <- any(emitr.na)
    if(hasna) emitr[emitr.na] <- 1
    return(emitr)
  }
  theoreticalMeans <- initialBafParams()$mus
  names(theoreticalMeans) <- c("A", "AAAB", "AAB", "AB", "ABB", "ABBB", "B")
  ##strictHets <- apply(b > 0.4 & b < 0.6, 2, mean, na.rm=TRUE) < 0.05
  ##fewHets <- apply(b > 0.05 & b < 0.45 | b > 0.55 & b < 0.95, 2, mean, na.rm=TRUE) < 0.05
  updateBafParams <- function(bf, params, h, j) {
    bindex <- bf > 0 & bf < 1
    weightedMoments <- function(w, homozygousA=FALSE, homozygousB=FALSE) {
      if(sum(w > 0.5, na.rm=TRUE) >= 10){
        ##w[w < 0.01 | is.na(w)] <- 0
        w[is.na(w)] <- 0
        w <- w[bindex]
        bf <- bf[bindex]
        sum.w <- sum(w)
        if(homozygousA) mu <- 0
        if(homozygousB) mu <- 1
        if(!homozygousA & !homozygousB)
          mu <- sum(w*bf, na.rm=TRUE)/sum.w
        sigma <- sqrt((sum(w*(bf-mu)^2, na.rm=TRUE))/sum.w)
        return(c(mu, sigma))
      } else c(NA, NA)
    }
    mus <- params[["mus"]]
    if(verbose) print(mus)
    sigmas <- params[["sigmas"]]
    pOut <- params[["prOutlier"]]
    pOutlierMax <- pOut[["max"]]
    pOutlierMaxROH <- pOut[["maxROH"]]
    p <- pOut[["prOutlier"]]
    p <- pmin(p, pOutlierMax)
    p <- 1-p ## probability not an outlier
    m <- rowSums(h, na.rm=TRUE)
    h <- h/m
    calculateGamma <- function(d, h, allele.prob){
      sumd <- 0
      ## conditional on not an outlier
      G <- length(d)
      ## integrate over G possible genotypes
      ## (marginal probability for CN given not an outlier)
      for(j in seq_len(G)){
        d[[j]] <- allele.prob[j]*d[[j]]
        sumd <- sumd+d[[j]]
      }
      ## integrate out the outlier indicator to get the marginal probability for the copy number
      ## (marginal probability for CN)
      ## denom <- p*sumd + (1-p)
      lapply(d, function(d) d*h/sumd)
    }
    updateBMoments <- function(gammas, nms){
      moments <- matrix(NA, length(nms), 2)
      for(i in seq_len(length(nms))){
        homozygousA <- nms[i] == "A"
        homozygousB <- nms[i] == "B"
        moments[i, ] <- weightedMoments(gammas[[i]],
                                        homozygousA=homozygousA,
                                        homozygousB=homozygousB)
      }
      ##			if(nms==c("A", "B")){
      ##				moments <- matrix(NA, 2, 2)
      ##				moments[1, ] <- weightedMoments(gammas, homozygousA=TRUE)
      ##				moments[2, ] <- weightedMoments(gammas, homozygousB=TRUE)
      ##			} else {
      ##				moments <- t(sapply(gammas, weightedMoments))
      ##			}
      rownames(moments) <- nms
      moments
    }
    nms <- names(mus)
    trNormal <- trnorm(bf)
    ##
    ## For cn = 0, BAF~Unif(0,1)
    gamma0 <- dunif(bf, 0,1) * h[, 1]
    gammaTotal <- function(g) {
      sumg <- 0
      for(i in seq_len(length(g))) sumg <- sumg+g[[i]]
      sumg
    }
    ## BAF means for cn 1
    dA <- trNormal(mus["A"], sigmas["A"])
    dB <- trNormal(mus["B"], sigmas["B"])
    gamma1 <- calculateGamma(list(dA, dB), h[, 2], allele.prob[, c("A", "B")])
    ## these estimates will be unreliable if there is weak
    ## evidence of hemizygous deletion as the moment estimatore includes
    ## the forward/backward probabilities
    m1 <- updateBMoments(gamma1, nms=c("A", "B"))
    ##
    ## BAF means for cn 2
    dAB <- trNormal(mus["AB"], sigmas["AB"])
    gamma2 <- calculateGamma(list(dA, dAB, dB), h[, 3], allele.prob[, c("AA", "AB", "BB")])
    m2 <- updateBMoments(gamma2, nms=c("A", "AB", "B"))##["AB", , drop=FALSE]
    ##
    ## BAF means for cn 3
    dAAB <- trNormal(mus["AAB"], sigmas["AAB"])
    dABB <- trNormal(mus["ABB"], sigmas["ABB"])
    gamma3 <- calculateGamma(list(dA, dAAB, dABB, dB), h[, 5], allele.prob[, c("AAA", "AAB", "ABB", "BBB")])
    m3 <- updateBMoments(gamma3, nms=c("A", "AAB", "ABB", "B"))##[c("AAB", "ABB"), ]
    ##
    ## Update mixture components.
    dAAAB <- trNormal(mus["AAAB"], sigmas["AAAB"])
    dABBB <- trNormal(mus["ABBB"], sigmas["ABBB"])
    gamma4 <- calculateGamma(list(dA, dAAAB, dAB, dABBB, dB), h[, 6], allele.prob[,c("AAAA", "AAAB", "AABB", "ABBB", "BBBB")])
    m4 <- updateBMoments(gamma4, nms=c("A", "AAAB", "AB", "ABBB", "B"))##[c("AAAB", "ABBB"), ]
    ##
    ## use fv/bv to find which state is likely to give the best estimate of the means/variances for A, and B
    totalh <- apply(h, 2, sum, na.rm=TRUE)
    imax <- which.max(totalh)
    mA_B <- switch(paste("state", imax, sep=""),
                   state1=m1,#?? homozygous null
                   state2=m1,
                   state3=m2,
                   state4=m1,## use means of A and B for copy-neutral ROH
                   state5=m3,
                   state6=m4)
    Gtotal[, 1] <- gammaTotal(gamma1) ## A or B
    Gtotal[, 2] <- gammaTotal(gamma2) ## AB
    Gtotal[, 3] <- gammaTotal(gamma3) ## AAB, ABB
    Gtotal[, 4] <- gammaTotal(gamma4) ## AAAB, ABBB
    ##mus.new <- c(mA_B[c("A", "B"),1], m2["AB", 1], m3[c("AAB", "ABB"), 1], m4[c("AAAB", "ABBB"), 1])
    mus.new <- c(0, 1, m2["AB", 1], m3[c("AAB", "ABB"), 1], m4[c("AAAB", "ABBB"), 1])
    names(mus.new)[1:3] <- c("A", "B", "AB")
    if(any(is.na(mus.new))){
      mus.new <- mus.new[match(names(theoreticalMeans),names(mus.new))]
      mus.new[is.na(mus.new)] <- theoreticalMeans[is.na(mus.new)]
    }
    mus.new <- makeMusBafNondecreasing(mus.new)
    if(mus.new["AB"] < 0.45 | mus.new["AB"] > 0.55) mus.new["AB"] <- params$mus[["AB"]]
    ##mus.new <- constrainMuBaf(mus.new)
    ## sd for hets. For hom null, hemizygous null, and regions of homozygosity, use the prior
    if(imax %in% c(3,5,6)){
      sAB <- switch(paste("state", imax, sep=""),
                    state3=m2["AB", 2],
                    state5=mean(c(m3["AAB", 2], m3["ABB", 2])),
                    state6=mean(c(m4["AAAB", 2], m4["AB", 2], m4["ABBB", 2])))
    } else sAB <- params$sigmas[["AB"]]
    ## if there are few hets, the sd estimate for AB BAFs will be unreliable
    sigmas.new <- rep(sAB, 7)
    names(sigmas.new) <- names(params$sigmas)
    ##sigmas.new <- c(mA_B[c("A", "B"),2], m2["AB", 2], m3[c("AAB", "ABB"), 2], m4[c("AAAB", "ABBB"), 2])
    sigmas.new[c("A", "B")] <- mA_B[c("A", "B"),2]
    if(any(is.na(sigmas.new))){
      sigmas.new <- sigmas.new[match(names(params$sigmas), names(sigmas.new))]
      sigmas.new[is.na(sigmas.new)] <- params$sigmas[is.na(sigmas.new)]
    }
    sigmas.new[c("A", "B")] <- pmax(sigmas.new[c("A", "B")], 0.001)
    ##names(sigmas.new)[3] <- "AB"
    ## do not allow any of the sd estimates to be less than the sd for the homozygous genotypes
    ##
    ## proportion outliers (how to account for homozygous deletions?)
    ##    -- exclude consecutive outliers
    has.nas <- anyMissing(Gtotal)
    if(has.nas) {
      nas <- is.na(Gtotal[,1])
      Gtotal <- Gtotal[-which(nas), ]
    }
    rmax <- tryCatch(rowMax(Gtotal), error=function(e) NULL)
    if(!is.null(rmax)){
      isout <- rmax < 0.9
      p <- mean(diff(isout) != 0)
    } else p <- pOut[["prOutlier"]]
    params[["mus"]] <- mus.new
    params[["sigmas"]] <- sigmas.new
    pOut[["prOutlier"]] <- min(p, 0.001)
    params[["prOutlier"]] <- pOut
    return(params)
  }
  cn.sd.new <- rep(NA, S)
  updateCnParams <- function(cn, params, h, j) {
    lindex <- cn != 0
    mu <- params[["mu"]]
    if(verbose) print(mu)
    sigma <- params[["sigmas"]]
    p <- params[["p"]]; q <- 1-p
    min.sd <- sds[j]/2
    ##h <- matrix(fv*bv, nr, S)
    ##h <- matrix(h, nr, S)
    m <- rowSums(h, na.rm=TRUE)
    h <- h/m
    d.unif <- cn.dunif[, j]
    d <- q*dnorm(cn, mean=matrix(mu, nr, S, byrow=TRUE), sd=matrix(sigma, nr, S, byrow=TRUE)) + p*d.unif
    d.total <- d+d.unif
    d1 <- d/(d+d.unif)
    d2 <- 1-d1
    ##pout <- mean(rowMax(d1) < 0.5) ## uniform distribution has higher probability than any of the cn states
    g1 <- h * d1  ## emission from observed states
    g1[g1 < 0.01] <- 0
    ##g2 <- 1-g1
    ##g2 <- h * (1-d1) ## emission from outlier state
    estimable <- apply(g1 > 0.5, 2, sum, na.rm=TRUE) >= 10
    if(!any(estimable)) {
      mu[c(3,4)] <- median(cn, na.rm=TRUE)
      mu[5] <- mu[3] + sds[j]
      mu[6] <- mu[3] + 2*sds[j]
      mu[2] <- mu[3] - 1*sds[j]
      mu[1] <- params$mu[1]
    }
    totalh <- apply(g1, 2, sum, na.rm=TRUE)
    ##round(total.g1,3)
    ## if total.g1 is small, then there's little data to estimate the means.
    mu.new <- colSums(g1*cn, na.rm=TRUE)/totalh
    if(any(!estimable))
      mu.new[!estimable] <- params[["mu"]][!estimable]
    imax <- which.max(totalh)
    if(imax == 4){
      mu.new[3] <- mu.new[4]
    } else mu.new[4] <- mu.new[3]
    ##
    ##
    ## without this constraint, the means for the gain
    ## states will creep towards normal copy number
    ##
    ##
    mu.new <- makeNonDecreasing(mu.new)
    one.sd <- params$sigmas[3]
    nsd <- 1
    diploid <- max(totalh[3], totalh[4])
    if(diploid > totalh[2]){
      mu.new[2] <- min(mu.new[3]-one.sd, mu.new[2])
    } else{
      mu.new[3] <- max(mu.new[2]+one.sd, mu.new[3])
    }
    if(diploid > totalh[5]){
      mu.new[5] <- max(mu.new[3]+nsd*one.sd, mu.new[5])
      mu.new[6] <- max(mu.new[5]+nsd*one.sd, mu.new[6])
    } else{
      mu.new[3] <- min(mu.new[5]-nsd*one.sd, mu.new[3])
      mu.new[6] <- max(mu.new[5]+nsd*one.sd, mu.new[6])
    }
    mu.new[1] <- min(mu.new[2] - one.sd, mu.new[1])
    mu.new[4] <- mu.new[3]
    ##if(fewHets[j]) mu.new[c(5,6)] <- params[["mu"]][c(5,6)]
    ## For loop
    for(s in seq_len(S)) {
      cn.sd.new[s] <- sqrt(sum(g1[, s]*(cn-mu.new[s])^2, na.rm=TRUE)/totalh[s])
    }
    ## assume sds are the same for non-null copy number states.
    ## use the state that has the highest overall probability for estimating the sd.
    cn.sd.new[2:6] <- cn.sd.new[imax]
    if(any(is.na(cn.sd.new))) {
      naindex <- which(is.na(cn.sd.new))
      cn.sd.new[naindex] <- params$sigmas[naindex]
    }
    cn.sd.new <- pmax(cn.sd.new, min.sd)
    ##if(any(!estimable))
    ##	cn.sd.new[!estimable] <- params$sigmas[!estimable]
    params[["mu"]] <- mu.new
    params[["sigmas"]] <- cn.sd.new
    ##total.g2 <- apply(g2, 2, sum, na.rm=TRUE)
    ##denom.eq52 <- apply(g1 + g2, 2, sum, na.rm=TRUE)
    p <- mean(rowSums(g1, na.rm=TRUE) < 0.1)
    ##p <- min(total.g2/denom.eq52)  ## altered states will have higher values of total.g2/denom.eq52.
    params[["p"]] <- min(p, 0.1)
    return(params)
  }
  ##tau <- transitionPr(TAUP)
  arm <- integer(nr)
  statePath <- integer(nr)
  scaleFactor <- rep(0, nr)
  bv <- fv <- matrix(0.0, nr*S, 1L)
  initialProb <- rep(1/S, S)
  fitViterbi <- function(emit){
    emit <- as.matrix(as.numeric(emit))
    res <- .C("viterbi2",
              emit=emit,
              pi=initialProb,
              tau=tau,
              arm=arm,
              S=S,
              nr=nr,
              statePath=statePath,
              fv=fv,
              bv=bv,
              3L,
              scaleFactor=scaleFactor)[c("statePath", "fv", "bv", "scaleFactor")]
    statePath <- res[["statePath"]]
    sf <- res[["scaleFactor"]]
    fv <- res[["fv"]]
    bv <- res[["bv"]]
    res <- list(statePath=statePath, fv=fv, bv=bv, sf=sf)
  }
	## this should not be within the for loop.
	## computeLLR
##	CHR <- paste("chr", oligoClasses::integer2chromosome(chrom), sep="")
##	chrarm <- oligoClasses:::.getArm(as.integer(chrom), position, build)
##	chrarm <- factor(chrarm, unique(chrarm))
##	toGRanges <- function(statePath, j){
##		id <- colnames(r)[j]
##		if(is.null(id)) id <- paste("sample", length(j), sep="")
##		sp <- split(statePath, chrarm)
##		rlist <- lapply(sp, Rle)
##		pos <- split(position, chrarm)
##		##rl <- Rle(statePath)
##		starts <- foreach(rl=rlist, x=pos) %do% x[start(rl)]
##		ends <- foreach(rl=rlist, x=pos) %do% x[end(rl)]
##		##starts <- position[start(rl)]
##		##ends <- position[end(rl)]
##		statelist <- foreach(states=sp, rl=rlist) %do% states[start(rl)]
##		chrlist <- split(CHR, chrarm)
##		chrl <- foreach(chrom=chrlist, rl=rlist) %do% chrom[start(rl)]
##		gr <- foreach(states=statelist,
##			      x1=starts,
##			      x2=ends,
##			      rl=rlist,
##			      chrom=chrl) %do% {
##				      GRanges(chrom, IRanges(x1, x2),
##					      numberProbes=width(rl), sample=id,
##					      state=states)
##			      }
##		gr <- unlist(GRangesList(gr))
##		##states <- statePath[start(rl)]
####		if(!is.null(id)){
####			gr <- GRanges(CHR, IRanges(starts, ends), numberProbes=width(rl), state=states, sample=id)
####		} else {
####			gr <- GRanges(CHR, IRanges(starts, ends), numberProbes=width(rl), state=states)
####		}
##	}
  log.initial <- log(rep(1/S, S))

  computeLogLikRatio <- function(gr, emit){
    lP.N2N <- transitionProbs$lP.N2N
    lP.N2A <- transitionProbs$lP.N2A
    lP.A2A <- transitionProbs$lP.A2A
    lP.A2N <- transitionProbs$lP.A2N
    lP.A2Astar <- transitionProbs$lP.A2Astar
    fr <- transitionProbs$fr
    gr2 <- gr
    alt.index <- which(state(gr) != normalIndex & numberProbes(gr) > 1)
    if(length(alt.index)==0) return(rep(0.0, length(gr)))
    gr <- gr[alt.index, ]
    log.emission <- log(emit)
    L <- length(gr)
    LLR <- rep(NA,  L)
    olaps <- findOverlaps(gr, fr)
    index <- subjectHits(olaps)
    indexList <- split(index, queryHits(olaps))
    starts <- sapply(indexList, min)
    ends <- sapply(indexList, max)
    statePath <- as.integer(state(gr))
    T <- length(statePath)
    rangeLogLik <- function(from, to, thisState){
      index <- seq(from, to)
      index2 <- index[-1]## t=2, ...., t*
      if(from == 1){
        if(to < T){
          logLik.vit <- log.initial[thisState] + log.emission[from, thisState]  + sum(lP.A2A[index2] + log.emission[index2, thisState]) + lP.A2N[to+1] + log.emission[to+1, normalIndex]
          logLik.null <- log.initial[normalIndex] + log.emission[from, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex]) + lP.N2N[to+1] + log.emission[to+1, normalIndex]
        } else {
          logLik.vit <- log.initial[thisState] + log.emission[from, thisState]  + sum(lP.A2A[index2] + log.emission[index2, thisState]) ##+ lP.A2N[last.index+1] + log.emission[last.index+1, normalIndex]
          logLik.null <- log.initial[normalIndex] + log.emission[from, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex]) ##+ lP.N2N[last.index+1] + log.emission[last.index+1, normalIndex]
        }
      } else { ## first index > 1
        index2 <- index[-1]
        if(to < T){
          logLik.vit <- lP.N2A[from] +
            sum(lP.A2A[index2]) +
              lP.A2N[to+1] +
                log.emission[from, thisState] +
                  sum(log.emission[index2, thisState]) +
                    log.emission[to+1, normalIndex]
          logLik.null <-
            sum(lP.N2N[index]) +
              lP.N2N[to+1]  +
                sum(log.emission[index, normalIndex]) +
                  log.emission[to+1, normalIndex]
        } else {
          logLik.vit <- lP.N2A[from] + log.emission[from, thisState] + sum(lP.A2A[index2] + log.emission[index2, thisState])
          logLik.null <- lP.N2N[from] + log.emission[from, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex])
        }
      }
      result <- logLik.vit - logLik.null
      return(result)
    }
    states <- state(gr)
    for(k in seq_len(L)) LLR[k] <- rangeLogLik(from=starts[k], to=ends[k], thisState=states[k])
    res <- rep(0.0, length(gr2))
    res[alt.index] <- LLR
    return(res)
  }
  list(initialCnParams=initialCnParams,
       initialBafParams=initialBafParams,
       updateBafEmission=updateBafEmission,
       updateCnEmission=updateCnEmission,
       updateBafParams=updateBafParams,
       updateCnParams=updateCnParams,
       ##trPr=transitionPr,
       fitViterbi=fitViterbi,
       ##toGRanges=toGRanges,
       computeLogLikRatio=computeLogLikRatio)
}

generatorFunG2 <- function(r, cnStates,
			   normalIndex, tau, limits,
			   p.hom, is.log,
			   computeLLR, verbose=FALSE,
			   transitionProbs){
	## why are we passing g?
	S <- length(cnStates)
	nc <- ncol(r)
	nr <- nrow(r)

	## params for copy number
	sds <- rep(NA, ncol(r))
	for(j in seq_along(sds)){
		i <- r[, j] != 0
		sds[j] <- sd(r[i,j], na.rm=TRUE)
	}

	initialCnParams <- function(j){
		mus <- cnStates
		sigmas <- rep(sds[j], S)
		p <- 0.01
		paramsCN <- list(mu=mus, sigmas=sigmas, p=p)
	}

	g2 <- g1 <- matrix(NA, nr, S)
	d1 <- matrix(NA, nr, S)
	emitr <- matrix(NA, nr, S)
	cn.dunif <- dunif(r, limits[1], limits[2])
	Gtotal <- matrix(NA, nr, 4)

	## might try dt here instead
	mydnorm <- function(x){
		function(mean, sd){
			mus <- matrix(mean, nr, S, byrow=TRUE)
			sds <- matrix(sd, nr, S, byrow=TRUE)
			dnorm(x, mus, sds)
		}
	}

	updateCnEmission <- function(cn, params, j){
		mus <- params[["mu"]]
		sds <- params[["sigmas"]]
		p <- params[["p"]]
		q <- 1-p
		normalDens <- mydnorm(cn)
		d <- normalDens(mus, sds)
		emitr <- q*d + p*cn.dunif[, j]
		emitr.na <- is.na(emitr)
		hasna <- any(emitr.na)
		if(hasna) emitr[emitr.na] <- 1
		return(emitr)
	}
	cn.sd.new <- rep(NA, S)
	updateCnParams <- function(cn, params, fv, bv, j) {
		mu <- params[["mu"]]
		sigma <- params[["sigmas"]]
		p <- params[["p"]]; q <- 1-p
		min.sd <- sds[j]/2

		h <- matrix(fv*bv, nr, S)
		m <- rowSums(h, na.rm=TRUE)
		h <- h/m

		d.unif <- cn.dunif[, j]
		d <- q*dnorm(cn, mean=matrix(mu, nr, S, byrow=TRUE), sd=matrix(sigma, nr, S, byrow=TRUE)) + p*d.unif
		d1 <- d/(d+d.unif)

		g1 <- h * d1
		g2 <- h * (1-d1)

		totalh <- apply(g1, 2, sum, na.rm=TRUE)
		mu.new <- colSums(g1*cn, na.rm=TRUE)/totalh
		mu.new[-4] <- constrainMu(mu.new[-4], is.log)
		mu.new[4] <- mu.new[normalIndex]
		mu.new <- makeNonDecreasing(mu.new)

		## For loop
		for(s in seq_len(S))  cn.sd.new[s] <- sqrt(sum(g1[, s]*(cn-mu.new[s])^2, na.rm=TRUE)/totalh[s])
		cn.sd.new <- pmax(cn.sd.new, min.sd)
		cn.sd.new <- constrainSd(cn.sd.new)
		params[["mu"]] <- mu.new
		params[["sigmas"]] <- cn.sd.new

		## update p as the proportion of values not fit by any of the states
		total.g2 <- apply(g2, 2, sum, na.rm=TRUE)
		denom.eq52 <- apply(g1 + g2, 2, sum, na.rm=TRUE)
		p <- min(total.g2/denom.eq52)  ## altered
		params[["p"]] <- p
		params
	}
	#
	arm <- integer(nr)
	statePath <- integer(nr)
	scaleFactor <- rep(0, nr)
	bv <- fv <- matrix(0.0, nr*S, 1L)
	initialProb <- rep(1/S, S)

	fitViterbi <- function(emit){
		emit <- as.matrix(as.numeric(emit))
		res <- .C("viterbi2", emit=emit, pi=initialProb,
			  tau=tau,
			  arm=arm, S=S, nr=nr, statePath=statePath,
			  fv=fv, bv=bv, 3L,
			  scaleFactor=scaleFactor)[c("statePath", "fv", "bv", "scaleFactor")]
		statePath <- res[["statePath"]]
		sf <- res[["scaleFactor"]]
		fv <- res[["fv"]]
		bv <- res[["bv"]]
		res <- list(statePath=statePath, fv=fv, bv=bv, sf=sf)
	}
	log.initial <- log(rep(1/S, S))
	computeLogLikRatio <- function(gr, emit){
		lP.N2N <- transitionProbs$lP.N2N
		lP.N2A <- transitionProbs$lP.N2A
		lP.A2A <- transitionProbs$lP.A2A
		lP.A2N <- transitionProbs$lP.A2N
		lP.A2Astar <- transitionProbs$lP.A2Astar
		fr <- transitionProbs$fr
		gr2 <- gr
		alt.index <- which(state(gr) != normalIndex & numberProbes(gr) > 1)
		if(length(alt.index)==0) return(rep(0.0, length(gr)))
		gr <- gr[alt.index, ]
		log.emission <- log(emit)
		L <- length(gr)
		LLR <- rep(NA,  L)
		olaps <- findOverlaps(gr, fr)
		index <- subjectHits(olaps)
		indexList <- split(index, queryHits(olaps))
		starts <- sapply(indexList, min)
		ends <- sapply(indexList, max)
		statePath <- as.integer(state(gr))
		T <- length(statePath)
		rangeLogLik <- function(from, to, thisState){
			index <- seq(from, to)
			index2 <- index[-1]## t=2, ...., t*
			if(from == 1){
				if(to < T){
					logLik.vit <- log.initial[thisState] + log.emission[from, thisState]  + sum(lP.A2A[index2] + log.emission[index2, thisState]) + lP.A2N[to+1] + log.emission[to+1, normalIndex]
					logLik.null <- log.initial[normalIndex] + log.emission[from, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex]) + lP.N2N[to+1] + log.emission[to+1, normalIndex]
				} else {
					logLik.vit <- log.initial[thisState] + log.emission[from, thisState]  + sum(lP.A2A[index2] + log.emission[index2, thisState]) ##+ lP.A2N[last.index+1] + log.emission[last.index+1, normalIndex]
					logLik.null <- log.initial[normalIndex] + log.emission[from, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex]) ##+ lP.N2N[last.index+1] + log.emission[last.index+1, normalIndex]
				}
			} else { ## first index > 1
				index2 <- index[-1]
				if(to < T){
					logLik.vit <- lP.N2A[from] +
						sum(lP.A2A[index2]) +
							lP.A2N[to+1] +
								log.emission[from, thisState] +
									sum(log.emission[index2, thisState]) +
										log.emission[to+1, normalIndex]
					logLik.null <-
						sum(lP.N2N[index]) +
							lP.N2N[to+1]  +
								sum(log.emission[index, normalIndex]) +
									log.emission[to+1, normalIndex]
				} else {
					logLik.vit <- lP.N2A[from] + log.emission[from, thisState] + sum(lP.A2A[index2] + log.emission[index2, thisState])
					logLik.null <- lP.N2N[from] + log.emission[from, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex])
				}
			}
			LLR <- logLik.vit - logLik.null
			return(LLR)
		}
		states <- state(gr)
		for(k in seq_len(L)) LLR[k] <- rangeLogLik(from=starts[k], to=ends[k], thisState=states[k])
		res <- rep(0.0, length(gr2))
		res[alt.index] <- LLR
		return(res)
	}
	list(initialCnParams=initialCnParams,
	     updateCnEmission=updateCnEmission,
	     updateCnParams=updateCnParams,
	     fitViterbi=fitViterbi,
	     computeLogLikRatio=computeLogLikRatio)
}

generatorFunSnpSet <- function(g, normalIndex, tau,
			       computeLLR, verbose=FALSE,
			       transitionProbs,
			       S){
	## why are we passing g?
	nc <- ncol(g)
	nr <- nrow(g)

	#
	arm <- integer(nr)
	statePath <- integer(nr)
	scaleFactor <- rep(0, nr)
	bv <- fv <- matrix(0.0, nr*S, 1L)
	initialProb <- rep(1/S, S)

	fitViterbi <- function(emit){
		emit <- as.matrix(as.numeric(emit))
		res <- .C("viterbi2", emit=emit, pi=initialProb,
			  tau=tau,
			  arm=arm, S=S, nr=nr, statePath=statePath,
			  fv=fv, bv=bv, 3L,
			  scaleFactor=scaleFactor)[c("statePath", "fv", "bv", "scaleFactor")]
		statePath <- res[["statePath"]]
		sf <- res[["scaleFactor"]]
		fv <- res[["fv"]]
		bv <- res[["bv"]]
		res <- list(statePath=statePath, fv=fv, bv=bv, sf=sf)
	}
	log.initial <- log(rep(1/S, S))
	computeLogLikRatio <- function(gr, emit){
		lP.N2N <- transitionProbs$lP.N2N
		lP.N2A <- transitionProbs$lP.N2A
		lP.A2A <- transitionProbs$lP.A2A
		lP.A2N <- transitionProbs$lP.A2N
		lP.A2Astar <- transitionProbs$lP.A2Astar
		fr <- transitionProbs$fr
		gr2 <- gr
		alt.index <- which(state(gr) != normalIndex & numberProbes(gr) > 1)
		if(length(alt.index)==0) return(rep(0.0, length(gr)))
		gr <- gr[alt.index, ]
		log.emission <- log(emit)
		L <- length(gr)
		LLR <- rep(NA,  L)
		olaps <- findOverlaps(gr, fr)
		index <- subjectHits(olaps)
		indexList <- split(index, queryHits(olaps))
		starts <- sapply(indexList, min)
		ends <- sapply(indexList, max)
		statePath <- as.integer(state(gr))
		T <- length(statePath)
		rangeLogLik <- function(from, to, thisState){
			index <- seq(from, to)
			index2 <- index[-1]## t=2, ...., t*
			if(from == 1){
				if(to < T){
					logLik.vit <- log.initial[thisState] + log.emission[from, thisState]  + sum(lP.A2A[index2] + log.emission[index2, thisState]) + lP.A2N[to+1] + log.emission[to+1, normalIndex]
					logLik.null <- log.initial[normalIndex] + log.emission[from, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex]) + lP.N2N[to+1] + log.emission[to+1, normalIndex]
				} else {
					logLik.vit <- log.initial[thisState] + log.emission[from, thisState]  + sum(lP.A2A[index2] + log.emission[index2, thisState]) ##+ lP.A2N[last.index+1] + log.emission[last.index+1, normalIndex]
					logLik.null <- log.initial[normalIndex] + log.emission[from, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex]) ##+ lP.N2N[last.index+1] + log.emission[last.index+1, normalIndex]
				}
			} else { ## first index > 1
				index2 <- index[-1]
				if(to < T){
					logLik.vit <- lP.N2A[from] +
						sum(lP.A2A[index2]) +
							lP.A2N[to+1] +
								log.emission[from, thisState] +
									sum(log.emission[index2, thisState]) +
										log.emission[to+1, normalIndex]
					logLik.null <-
						sum(lP.N2N[index]) +
							lP.N2N[to+1]  +
								sum(log.emission[index, normalIndex]) +
									log.emission[to+1, normalIndex]
				} else {
					logLik.vit <- lP.N2A[from] + log.emission[from, thisState] + sum(lP.A2A[index2] + log.emission[index2, thisState])
					logLik.null <- lP.N2N[from] + log.emission[from, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex])
				}
			}
			LLR <- logLik.vit - logLik.null
			return(LLR)
		}
		states <- state(gr)
		for(k in seq_len(L)) LLR[k] <- rangeLogLik(from=starts[k], to=ends[k], thisState=states[k])
		res <- rep(0.0, length(gr2))
		res[alt.index] <- LLR
		return(res)
	}
	list(fitViterbi=fitViterbi,
	     computeLogLikRatio=computeLogLikRatio)
}

setMethod("hmm", signature(object="oligoSnpSet"),
	  function(object, ...){
            ##.Deprecated("hmm2")
            if("baf" %in% assayDataElementNames(object)){
              return(hmmBafLrrSet2(object, ...))
            } else{
              ## baf not in assay data
              ## - use genotypes
              return(hmmOligoSnpSet2(object, ...))
            }
	  })

setMethod("hmm", signature(object="BeadStudioSet"),
	  function(object, ...){
            ##.Deprecated("hmm2")
            hmmBafLrrSet2(object, ...)
	  })

setMethod("hmm", signature(object="SnpSet2"),
	  function(object, ICE=FALSE, ...){
            ##.Deprecated("hmm2")
		  if(!ICE){
			  hmmSnpSet2(object, ...)
		  } else hmmSnpSetIce(object, ...)
	  })

setMethod("hmm", signature(object="BeadStudioSetList"),
	  function(object, ...){
            ##.Deprecated("hmm2")
            ##hmmBeadStudioSetList(object, ...)
            hmmBafLrrSetList2(object, ...)
	  })

setMethod("hmm", signature(object="BafLrrSetList"),
	  function(object, ...){
            ##.Deprecated("hmm2")
            hmmBafLrrSetList2(object, ...)
	  })

setMethod("hmm", signature(object="oligoSetList"),
	  function(object, ...){
            ##.Deprecated("hmm2")
            nms <- ls(assayData(object))
            if("baf" %in% nms){
              hmmBafLrrSetList2(object, ...)
            } else {
              stop("assay data element 'baf' required")
            }##hmmOligoSetList(object, ...)
	  })

hmmOligoSnpSet2 <- function(object, sampleIds, TAUP=1e10, tauMAX,
			    cnStates=c(0, 1, 2, 2, 3, 4),
			    is.log=FALSE,
			    ...){
  pkgs <- c("GenomicRanges", "VanillaICE", "oligoClasses", "matrixStats")
  if(isPackageLoaded("ff")) pkgs <- c("ff", pkgs)
  if(missing(sampleIds)) sampleIds <- sampleNames(object)
  index.samples <- match(sampleIds, sampleNames(object))
  chunks <- splitIndicesByLength(index.samples, ocSamples())
  r <- copyNumber(object)
  g <- calls(object)
  pos <- position(object)
  chr <- chromosome(object)
  grFun <- generatorGRanges(chr, pos, genomeBuild(object), TAUP=TAUP, tauMAX=tauMAX)
  is.snp <- isSnp(object)
  snp.index <- which(is.snp)
  anyNP <- any(!is.snp)
  ##if(anyNP) np.index <- which(!is.snp) else np.index <- integer()
  center <- TRUE
  pkgs <- c("oligoClasses", "VanillaICE")
  isff <- is(g, "ff")
  if(isff) pkgs <- c("ff", pkgs)
  if(is.log) limits <- c(-4,3) else limits <- c(0, 5)
  matrixFun <- generatorViterbiCnGt(r, g, chr, center=TRUE,
                                    snp.index=snp.index,
                                    anyNP=anyNP,
                                    cnStates=cnStates)
  rm(pos, chr, r, g); gc()
  i <- NULL
  results <- foreach(i=chunks, .packages=pkgs) %dopar% {
    viterbiWrapperG2(index.samples=i,
                     is.snp=is.snp,
                     anyNP=anyNP,
                     is.log=is.log,
                     limits=limits,
                     cnStates=cnStates,
                     grFun=grFun,
                     matrixFun=matrixFun, ...)
  }
  grl <- GRangesList(results)
  metadata(grl) <- list(genome=genomeBuild(object))
  grl
}

hmmBafLrrSet2 <- function(object, sampleIds, TAUP=1e10, tauMAX,
			  cnStates=c(-2, -0.4, 0, 0, 0.4, 1),
			  is.log=TRUE,
			  ...){
  pkgs <- c("GenomicRanges", "VanillaICE", "oligoClasses", "matrixStats")
  if(isPackageLoaded("ff")) pkgs <- c("ff", pkgs)
  if(missing(sampleIds)) sampleIds <- sampleNames(object)
  index.samples <- match(sampleIds, sampleNames(object))
  chunks <- splitIndicesByLength(index.samples, ocSamples())
  r <- copyNumber(object)
  b <- baf(object)
  pos <- position(object)
  chr <- chromosome(object)
  grFun <- generatorGRanges(chr, pos, genomeBuild(object), TAUP=TAUP, tauMAX=tauMAX)
  is.snp <- isSnp(object)
  snp.index <- which(is.snp)
  anyNP <- any(!is.snp)
  ##if(anyNP) np.index <- which(!is.snp) else np.index <- integer()
  center <- TRUE
  pkgs <- c("oligoClasses", "VanillaICE")
  isff <- is(r, "ff")
  if(isff) pkgs <- c("ff", pkgs)
  matrixFun <- generatorViterbiBeadStudioSet(r, b, chr, center=TRUE,
                                             snp.index=snp.index, anyNP=anyNP,
                                             cnStates=cnStates)
  if(is.log) limits <- c(-4,3) else limits <- c(0, 5)
  rm(pos, chr, b, r); gc()
  i <- NULL
  results <- foreach(i=chunks, .packages=pkgs) %dopar% {
    viterbi2Wrapper(index.samples=i,
                    snp.index=snp.index,
                    anyNP=anyNP,
                    is.log=is.log,
                    limits=limits,
                    cnStates=cnStates,
                    grFun=grFun,
                    matrixFun=matrixFun, ...)
  }
  grl <- GRangesList(results)
  metadata(grl) <- list(genome=genomeBuild(object))
  grl
}

#' Deprecated functions in the VanillaICE package
#'
#' These functions have been deprecated.  The functions are only
#' provided only for compatability with older versions and will be
#' defunct at the next release.
#'
#' @param x a numeric matrix
#' @param takeLog whether to first log2 transform the numeric matrix
#' @param ... additional arguments to \code{mad}
#' @seealso \code{\link{mad}}
#' @rdname Deprecated
#' @return a matrix of the same dimension as the input. Within a column, the entries are identical
#' @export
robustSds <- function(x, takeLog=FALSE, ...){
  .Deprecated("rowMAD")
  if(takeLog) x <- log2(x)
  sds <- apply(x, 2, "mad", ...)
  sds <- matrix(sds, nrow(x), ncol(x), byrow=TRUE)
  dimnames(sds) <- dimnames(x)
  return(sds)
}


hmmSnpSet2 <- function(object, sampleIds, TAUP=1e10, tauMAX,
		       normalIndex=1L,
		       rohIndex=normalIndex+1L,
		       S=2L,
		       ...){
	pkgs <- c("GenomicRanges", "VanillaICE", "oligoClasses", "matrixStats")
	if(missing(sampleIds)) sampleIds <- sampleNames(object)
	index.samples <- match(sampleIds, sampleNames(object))
	chunks <- splitIndicesByLength(index.samples, ocSamples())
	g <- calls(object)
	pos <- position(object)
	chr <- chromosome(object)
	grFun <- generatorGRanges(chr, pos, genomeBuild(object), TAUP=TAUP, tauMAX=tauMAX)
	isff <- is(g, "ff")
	if(isff) pkgs <- c("ff", pkgs)
	matrixFun <- generatorViterbiSnpSet(g, chr, isff)
	rm(pos, chr); gc()
	i <- NULL
	results <- foreach(i=chunks, .packages=pkgs) %dopar% {
		viterbiForSnpSet2(index.samples=i,
				  grFun=grFun,
				  matrixFun=matrixFun,
				  S=S,
				  ...)
	}
	grl <- GRangesList(results)
	metadata(grl) <- list(genome=genomeBuild(object))
	grl
}

hmmSnpSetIce <- function(object, sampleIds, TAUP=1e10, tauMAX,
			 ...){
	pkgs <- c("GenomicRanges", "VanillaICE", "oligoClasses", "matrixStats")
	if(missing(sampleIds)) sampleIds <- sampleNames(object)
	index.samples <- match(sampleIds, sampleNames(object))
	chunks <- splitIndicesByLength(index.samples, ocSamples())
	g <- calls(object)
	gt.conf <- snpCallProbability(object)
	pos <- position(object)
	chr <- chromosome(object)
	grFun <- generatorGRanges(chr, pos, genomeBuild(object),
				  TAUP=TAUP, tauMAX=tauMAX)
	isff <- is(g, "ff")
	if(isff) pkgs <- c("ff", pkgs)
	matrixFun <- generatorViterbiSnpSetIce(g, gt.conf, chr, isff)
	rm(pos, chr, g, gt.conf); gc()
	i <- NULL
	results <- foreach(i=chunks, .packages=pkgs) %dopar% {
		viterbiForSnpSetIce(index.samples=i,
				    grFun=grFun,
				    matrixFun=matrixFun,
				    annotationPkg=annotation(object),
				    ...)
	}
	GRangesList(results)
}


hmmBafLrrSetList2 <- function(object, sampleIds, TAUP=1e10, tauMAX,
			      cnStates=c(-2, -0.4, 0, 0, 0.4, 1), is.log=TRUE,
			      ...){
	pkgs <- c("GenomicRanges", "VanillaICE", "oligoClasses", "matrixStats")
	if(isPackageLoaded("ff")) pkgs <- c("ff", pkgs)
	if(missing(sampleIds)) sampleIds <- sampleNames(object)
	index.samples <- match(sampleIds, sampleNames(object))
	chunks <- splitIndicesByLength(index.samples, ocSamples())
	rlist <- lrr(object)
	blist <- baf(object)
	pos <- unlist(position(object))
	chr <- rep(chromosome(object), elementLengths(object))
	grFun <- generatorGRanges(chr, pos, genomeBuild(object), TAUP=TAUP, tauMAX=tauMAX)
	is.snp <- unlist(lapply(featureDataList(object), isSnp))
	snp.index <- which(is.snp)
	anyNP <- any(!is.snp)
	##if(anyNP) np.index <- which(!is.snp) else np.index <- integer()
	center <- TRUE
	pkgs <- c("oligoClasses", "VanillaICE")
	isff <- is(rlist[[1]], "ff")
	if(isff) pkgs <- c("ff", pkgs)
	matrixFun <- generatorViterbi(rlist, blist, chr, center=TRUE,
				      snp.index=snp.index, anyNP=anyNP)
	rm(pos, chr, blist, rlist); gc()
	i <- NULL
	results <- foreach(i=chunks, .packages=pkgs) %dopar% {
		viterbi2Wrapper(index.samples=i,
				snp.index=snp.index,
				anyNP=anyNP,
				is.log=TRUE,
				limits=c(-4, 3),
				cnStates=cnStates,
				grFun=grFun,
				matrixFun=matrixFun, ...)
	}
	grl <- GRangesList(results)
	metadata(grl) <- list(genome=genomeBuild(object))
	grl
}


genotypeEmissionCrlmm <- function(object,
				  gt.conf,
				  cdfName,
				  prHetCalledHom,
				  prHetCalledHet,
				  prHomInNormal,
				  prHomInRoh){
	stopifnot(is(object, "matrix"))
	GT <- as.integer(object)
	##rm(object); gc()
	if(cdfName == "pd.genomewidesnp.6"){
		annotation <- "genomewidesnp6"
	} else annotation <- cdfName
	loader(paste(annotation, "Conf.rda", sep=""), .vanillaIcePkgEnv, "VanillaICE")
	hapmapP <- getVarInEnv("reference")
##	prHetCalledHom <- hmm.params[["prHetCalledHom"]]
##	prHetCalledHet <- hmm.params[["prHetCalledHet"]]
##	prHomInNormal <- hmm.params[["prHomInNormal"]]
##	prHomInRoh <- hmm.params[["prHomInRoh"]]
	if(length(cdfName) < 1) stop("must specify annotation")
	##data(list=paste(annotation, "Conf", sep=""), package="VanillaICE", envir=environment())
	if(length(prHomInNormal) == nrow(gt.conf)){  ##convert to vector
		prHomInNormal <- as.numeric(matrix(prHomInNormal, nrow(gt.conf), ncol(gt.conf), byrow=FALSE))
	} else prHomInNormal <-  rep(prHomInNormal, length(GT))
	hapmapP[, 2] <- 1-exp(-hapmapP[, 2]/1000)
	##p = 1-exp(-X/1000)
	##1000*log(1-p)=X
	##confidence <- 1-exp(-GTconf/1000)
	i11 <- hapmapP[, 1] == 3  ##called homozygous truth homozygous
	i12 <- hapmapP[, 1] == 4  ##called homozygous truth heterozygous
	i21 <- hapmapP[, 1] == 1  ##called het truth hom
	i22 <- hapmapP[, 1] == 2  ##called het truth het
	f11 <- density(hapmapP[i11, 2], from=0, to=1, n=1e3)
	f12 <- density(hapmapP[i12, 2], from=0, to=1, n=1e3)
	f21 <- density(hapmapP[i21, 2], from=0, to=1, n=1e3)
	f22 <- density(hapmapP[i22, 2], from=0, to=1, n=1e3)
	##-------------------------------------------------------------------------
	##distribution of observed call probabilities when the call is homozygous
	##-------------------------------------------------------------------------
	##-------------------------------------------------------------------------
	##P(phat | LOH, gte)
	##-------------------------------------------------------------------------
	##GT <- as.integer(genotypes)
	##confidence <- as.numeric(confidence)
	##confidence <- as.numeric(gt.conf)
	confidence <- gt.conf
	rm(gt.conf); gc()
	pTruthIsNormal <- pTruthIsRoh <- rep(NA, length(GT))
	confidence[confidence==0] <- 0.01 ##Otherwise, NA's result
	hom <- which(GT == 1 | GT == 3)
	observedPcalledHom <- cut(confidence[hom], breaks=f11$x, labels=FALSE)
	pTruthIsRoh[hom] <- f11$y[observedPcalledHom]
	het <- which(GT == 2)
	observedPcalledHet <- cut(confidence[het], breaks=f11$x, labels=FALSE)
	pTruthIsRoh[het] <- f21$y[observedPcalledHet]
	##-------------------------------------------------------------------------
	##Calculate P(phat | Normal, HOM)
	##-------------------------------------------------------------------------
	chet1 <- f22$y[cut(confidence[het], breaks=f22$x, labels=FALSE)]
	chet2 <- f21$y[cut(confidence[het], breaks=f21$x, labels=FALSE)]
	##term5[1]=P(true genotype is HET | genotype call is AB, state is normal)
	pTruthIsNormal[het] <- chet1*prHetCalledHet + chet2*(1-prHetCalledHet)
	##chom1=called homozygous truth heterozygous
	chom1 <- f12$y[cut(confidence[hom], breaks=f12$x, labels=FALSE)]
	##chom2=called homozygous truth homozygous
	chom2 <- f11$y[cut(confidence[hom], breaks=f11$x, labels=FALSE)]
	##chom4 <- 0.9999    ##P(HOM|CHOM)
	##probability that the true state is HOM when genotype call is homozygous
	##prHetCalledHom = P(true genotype is HET | calls is AA or BB, state is normal)
	pTruthIsNormal[hom] <- chom1*prHetCalledHom + chom2*(1-prHetCalledHom)
	fNormal <- fLoh <- rep(NA, length(GT))
	fNormal[hom] <- prHomInNormal[hom] * pTruthIsNormal[hom]
	fNormal[het] <- (1-prHomInNormal[het]) * pTruthIsNormal[het]
	fLoh[hom] <- prHomInRoh * pTruthIsRoh[hom]
	fLoh[het] <- (1-prHomInRoh) * pTruthIsRoh[het]
	f <- array(NA, dim=c(nrow(object), ncol(object), 2)) ##dimnames=list(featureNames(object),
							##     sampleNames(object),
							  ##   c("normal", "ROH")))
	f[, , 1] <- matrix(fNormal, length(object), ncol(object))
	f[, , 2] <- matrix(fLoh, length(object), ncol(object))
	dimnames(f)[[3]] <- c("normal", "ROH")
	f[f  == 0] <- min(f[f > 0], na.rm=TRUE)
	f <- log(f)
	return(f)
}

computeLoglikFromViterbi2 <- function(object, chrom, id, pos, log.it=TRUE){
	viterbiSequence <- viterbiStatePath(object)
	S <- nStates(object)
	rl <- Rle(viterbiSequence)
	starts <- start(rl)
	LLR <- rep(NA,  length(starts))
	if(log.it){
		log.emission <- matrix(log(emission(object)), object@numberFeatures, nStates(object))
		log.initial <- log(initialProb(object))
	} else {
		log.emission <- matrix(emission(object), object@numberFeatures, nStates(object))
		log.initial <- initialProb(object)
	}
	p <- c(NA, transitionProb(object))
	##p <- c(NA, as.numeric(viterbiResults[["tau"]]))
	c1 <- normal2altered(object)
	c2 <- altered2normal(object)
	c3 <- altered2altered(object)
	lP.N2N <- log(1-((1-p)*(S-1)*c1)) ##probability normal -> normal
	lP.N2A <- log((1-p)*c1) ##probability normal -> altered
	P.A2A <- sapply(1-((1-p)*(c2+(S-2)*c3)), function(x) max(x, 0.01))
	lP.A2A <- log(P.A2A) ## probability altered to same altered state
	lP.A2N <- log((1-p)*c2) ##probability altered -> normal
	lP.A2Astar <- log((1-p)*c3) ## probability altered -> different altered state
	for(k in seq_along(starts)){
		LLR[k] <- computeLoglikForRange(from=start(rl)[k],
						to=end(rl)[k],
						viterbiSequence=viterbiSequence,
						normalIndex=normalStateIndex(object),
						log.initial=log.initial,
						log.emission=log.emission,
						lP.N2N=lP.N2N,
						lP.N2A=lP.N2A,
						lP.A2N=lP.A2N,
						lP.A2A=lP.A2A)
	}
	start.index <- start(rl)
	end.index <- end(rl)
	start <- pos[start.index]
	end <- pos[end.index]
	numMarkers <- width(rl)
	states <- viterbiSequence[start.index]
	ir <- IRanges(start=start, end=end)
	charChrom <- oligoClasses::integer2chromosome(chrom)
	##sl <- getSequenceLengths(genomeBuild(object))[charChrom]
	rd <- GRanges(seqnames=paste("chr", charChrom, sep=""),
		      ir,
		      state=as.integer(states),
		      numberProbes=numMarkers,
		      LLR=LLR)
##	names(rd) <- id
##	rd <- RangedDataHMM(ir,
##			    chromosome=rep(chrom, length(LLR)),
##			    sampleId=rep(id, length(LLR)),
##			    state=states,
##			    coverage=numMarkers,
##			    startIndexInChromosome=start.index,
##			    endIndexInChromosome=end.index,
##			    LLR=LLR)
	return(rd)
}

gtEmissionFromMatrix <- function(object,
				 S,
				 gt.conf,
				 is.snp,
				 normalIndex=3L,
				 rohIndex=c(2L, 4L),
				 ICE=FALSE,
				 prGtHom=c(0.7, 0.99),
				 prGtMis=rep(1/S,S),
				 prHetCalledHom=0.001,
				 prHetCalledHet=0.995,
				 prHomInNormal=0.8,
				 prHomInRoh=0.999,
				 annotationPkg,
				 log.it=TRUE,
				 ...){
	if(!ICE){
		p <- prGtHom
		prGenotypeMissing <- prGtMis
		stopifnot(length(p) == S)
		if(!is.numeric(object)) stop("genotypes must be integers (1=AA, 2=AB, 3=BB) or NA (missing)")
		emission <- array(NA, dim=c(nrow(object), ncol(object), S))
		missingGT <- any(is.na(object))
		for(s in seq_len(S)){
			tmp <- object
			tmp[tmp == 1 | tmp == 3] <- p[s]
			tmp[tmp == 2] <- 1-p[s]
			index1 <- is.na(tmp) & !is.snp
			index2 <- is.na(tmp) & is.snp
			if(missingGT){
				tmp[index2] <- prGenotypeMissing[s]
				tmp[index1] <- 1/S
			}
			emission[, , s] <- tmp
		}
		if(log.it) emit <- log(emission) else emit <- emission
	} else {
		not.valid <- invalidGtConfidence(gt.conf)
		if(any(not.valid)){
			stop("Invalid genotype confidence scores.\n",
			     "\tIf ICE is TRUE, all confidence scores must be between 0 and 1")
		}
		logemit <- array(NA, dim=c(nrow(object), ncol(object), S))
		pkg <- strsplit(annotationPkg, "Crlmm")[[1]][[1]]
		tmp <- genotypeEmissionCrlmm(object, gt.conf=gt.conf,
					     cdfName=pkg,
					     prHetCalledHom,
					     prHetCalledHet,
					     prHomInNormal,
					     prHomInRoh)
		logemit[, , rohIndex] <- tmp[, , "ROH"]
		logemit[, , -rohIndex] <- tmp[, , "normal"]
		if(log.it) emit <- logemit else emit <- exp(logemit)
	}
	return(emit)
}

invalidGtConfidence <- function(x){
	is.na(x) | x < 0 | x > 1 | is.nan(x) | is.infinite(x)
}
#
# viterbiForSnpSet <- function(gt, S=2L,
# 			     is.snp, pos, gt.conf,
# 			     chrom,
# 			     ICE=FALSE,
# 			     rohIndex=2L,
# 			     annotationPkg,
# 			     prGtHom,
# 			     prGtMis=rep(1/S,S),
# 			     prHetCalledHom=0.001,
# 			     prHetCalledHet=0.995,
# 			     prHomInNormal=0.8,
# 			     prHomInRoh=0.999,
# 			     normalIndex=1L,
# 			     TAUP=1e8,
# 			     ...){ ## additional arguments passed to Viterbi (e.g., TAUP, normal2altered...)
# 	log.beta.gt <- gtEmissionFromMatrix(object=gt,
# 					    S=S,
# 					    is.snp=is.snp,
# 					    pos=pos,
# 					    gt.conf=gt.conf,
# 					    ICE=ICE,
# 					    annotationPkg=annotationPkg,
# 					    prGtHom=prGtHom,
# 					    prGtMis=prGtMis,
# 					    prHetCalledHom=prHetCalledHom,
# 					    prHetCalledHet=prHetCalledHet,
# 					    prHomInNormal=prHomInNormal,
# 					    prHomInRoh=prHomInRoh,
# 					    rohIndex=2L)
# 	transitionProb <- computeTransitionProb(pos, S=S, TAUP=TAUP)
# 	## construct ViterbiSet
# 	J <- ncol(gt)
# 	viterbiList <- vector("list", J)
# 	for(j in seq_len(J)){
# 		tmp <- new("Viterbi",
# 			   numberFeatures=nrow(gt),
# 			   numberStates=S,
# 			   emission=as.matrix(as.numeric(log.beta.gt[, j, ])),
# 			   normalIndex=normalIndex,
# 			   transitionProb=transitionProb,
# 			   ...)
# 		viterbiList[[j]] <- fit(tmp)
# 	}
# 	id <- object <- NULL
# 	rdlist <- foreach(object = viterbiList,
# 			  id=colnames(gt)) %do% {
# 				  computeLoglikFromViterbi2(object=object,
# 							    chrom=chrom,
# 							    id=id,
# 							    pos=pos,
# 							    log.it=FALSE)
# 			  }
# 	res <- GRangesList(rdlist)
# 	names(res) <- colnames(gt)
# 	return(res)
# }
#
# setMethod("fit", signature(object="Viterbi"), function(object){
# 	fitViterbi(object)
# })
#
# fitViterbi <- function(object){
# 	tmp <- .C("viterbi",
# 		  emission(object),
# 		  initialProb(object),
# 		  transitionProb(object), ## log scale?
# 		  object@arm,
# 		  nStates(object),
# 		  object@numberFeatures,
# 		  viterbiStatePath(object),
# 		  delta(object),
# 		  object@normal2altered,
# 		  object@altered2normal,
# 		  object@altered2altered,
# 		  normalStateIndex(object),
# 		  pAA(object))
# 	new("Viterbi",
# 	    emission=emission(object),
# 	    initialProb=initialProb(object),
# 	    transitionProb=transitionProb(object),
# 	    arm=object@arm,
# 	    numberStates=nStates(object),
# 	    numberFeatures=object@numberFeatures,
# 	    viterbiStatePath=tmp[[7]],
# 	    delta=tmp[[8]],
# 	    normal2altered=object@normal2altered,
# 	    altered2normal=object@altered2normal,
# 	    altered2altered=object@altered2altered,
# 	    normalIndex=normalStateIndex(object),
# 	    pAA=pAA(object))
# }
#
computeLoglikForRange <- function(from, to,
				  viterbiSequence, normalIndex,
				  log.initial,
				  log.emission,
				  lP.A2N,
				  lP.N2A,
				  lP.N2N,
				  lP.A2A){
	index <- seq(from, to)
	thisState <- unique(viterbiSequence[index])
	if(thisState == normalIndex) return(0)
	first.index <- min(index)
	last.index <- max(index)
	T <- length(viterbiSequence)
	## 6 Rules  (1 < t < T)
	## 1.  index=1
	## 2.  index=t
	## 3.  index=t,t+1
	## 4.  index=T
	## 5.  index=1,2
	## 6,  index=T-1, T
	##------
	## 1. index=1
	if(first.index == last.index & last.index==1){
		##note the last term cancels
		logLik.vit <- log.initial[thisState]+log.emission[1, thisState] + lP.A2N[2] + log.emission[2, normalIndex]
		logLik.null <- log.initial[normalIndex]+log.emission[1, normalIndex] + lP.N2N[2] + log.emission[2, normalIndex]
		LLR <- logLik.vit-logLik.null
		return(LLR)
	}
	##2 index = t
	if(length(index) == 1 & first.index > 1 & last.index < T){
		##note the last term cancels
		logLik.vit <- sum(lP.N2A[index] + log.emission[index, thisState]) + lP.A2N[last.index+1]+log.emission[last.index+1, normalIndex]
		logLik.null <- sum(lP.N2N[index] + log.emission[index, normalIndex]) + lP.N2N[last.index+1] + log.emission[last.index+1, normalIndex]
		LLR <- logLik.vit-logLik.null
		return(LLR)
	}
	##if T=t+1?
	## 3: index = t, ..., t*  , t>1, t* < T, t* != t
	if(first.index != last.index & first.index > 1 & last.index < T){
		index2 <- index[-1]
		logLik.vit <- lP.N2A[first.index] +
			sum(lP.A2A[index2]) +
				lP.A2N[last.index+1] +
					log.emission[first.index, thisState] +
						sum(log.emission[index2, thisState]) +
							log.emission[last.index+1, normalIndex]
		logLik.null <-
			sum(lP.N2N[index]) +
				lP.N2N[last.index+1]  +
					sum(log.emission[index, normalIndex]) +
						log.emission[last.index+1, normalIndex]
		LLR <- logLik.vit-logLik.null
		return(LLR)
	}
	## 4: index = T
	if(first.index == last.index & last.index == T){
		logLik.vit <- lP.N2A[T] + log.emission[T, thisState]
		logLik.null <- lP.N2N[T] + log.emission[T, normalIndex]
		LLR <- logLik.vit-logLik.null
		return(LLR)
	}
	## 5: index = 1, 2, ...
	if(first.index != last.index & first.index == 1 & last.index < T){
		index2 <- index[-1]## t=2, ...., t*
		logLik.vit <- log.initial[thisState] + log.emission[first.index, thisState]  + sum(lP.A2A[index2] + log.emission[index2, thisState]) + lP.A2N[last.index+1] + log.emission[last.index+1, normalIndex]
		logLik.null <- log.initial[normalIndex] + log.emission[first.index, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex]) + lP.N2N[last.index+1] + log.emission[last.index+1, normalIndex]
		LLR <- logLik.vit-logLik.null
		return(LLR)
	}
	if(first.index != last.index & first.index == 1 & last.index == T){
		index2 <- index[-1]## t=2, ...., t*
		logLik.vit <- log.initial[thisState] + log.emission[first.index, thisState]  + sum(lP.A2A[index2] + log.emission[index2, thisState]) ##+ lP.A2N[last.index+1] + log.emission[last.index+1, normalIndex]
		logLik.null <- log.initial[normalIndex] + log.emission[first.index, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex]) ##+ lP.N2N[last.index+1] + log.emission[last.index+1, normalIndex]
		LLR <- logLik.vit-logLik.null
		return(LLR)
	}
	## 6: index = t, ...T
	if(first.index != last.index & last.index == T){
		index2 <- index[-1]
		logLik.vit <- lP.N2A[first.index] + log.emission[first.index, thisState] + sum(lP.A2A[index2] + log.emission[index2, thisState])
		logLik.null <- lP.N2N[first.index] + log.emission[first.index, normalIndex] + sum(lP.N2N[index2] + log.emission[index2, normalIndex])
		LLR <- logLik.vit - logLik.null
		return(LLR)
	}
	stop("none of conditions in likRatio function satisfied")
}

checkAnnotationForICE <- function(object){
	if(!annotation(object) %in% icePlatforms()){
		stop("ICE is TRUE, but hapmap crlmm confidence scores for ", annotation(object), " are not available.")
	}
}


#' List platforms for which ICE option is supported.
#'
#'   When procecessing genotypes with the \pkg{crlmm}, confidence
#' scores for the diallelic genotype calls are available.  One can
#' estimate the emission probabilities for the crlmm diallelic
#' genotypes using the confidence scores by setting the value of
#' \code{ICE} to TRUE in the constructor for the \code{HmmOptionList}
#' class.  Currently, only certain platforms are supported for this
#' option.
#' @return   A character vector of the annotation packages that are supported for
#'  the ICE option
#' @references Scharpf, RB et al., 2008, Annals of Applied Statistics
#' @examples
#' icePlatforms()
#' @keywords misc
#' @export
icePlatforms <- function(){
	c("pd.genomewidesnp.6",
	  "genomewidesnp6",
	  "genomewidesnp6Crlmm",
	  "pd.mapping250k.nsp",
	  "pd.mapping250k.sty",
	  "pd.mapping250k.nsp, pd.mapping250k.sty")
}

setMethod("gtEmission", signature(object="matrix"),
	  function(object, gt.conf,
		   is.snp,
		   rohIndex=c(2L, 4L),
		   S=6L,
		   annotationPkg,
		   ICE=FALSE,
		   prGtHom=c(0.7,0.99),
		   prGtMis=rep(1/S, S),
		   prHetCalledHom=0.001,
		   prHetCalledHet=0.995,
		   prHomInNormal=0.8,
		   prHomInRoh=0.999, ...){
		  gtEmissionFromMatrix(object, gt.conf=gt.conf,
				       is.snp=is.snp,
				       rohIndex=rohIndex,
				       S=S,
				       annotationPkg=annotationPkg, ICE=ICE,
				       prGtHom=prGtHom,
				       prGtMis=prGtMis,
				       prHetCalledHom=prHetCalledHom,
				       prHetCalledHet=prHetCalledHet,
				       prHomInNormal=prHomInNormal,
				       prHomInRoh=prHomInRoh,...)
	  })

gtEmissionFromMatrix <- function(object,
				 S,
				 gt.conf,
				 is.snp,
				 normalIndex=3L,
				 rohIndex=c(2L, 4L),
				 ICE=FALSE,
				 prGtHom=c(0.7, 0.99),
				 prGtMis=rep(1/S,S),
				 prHetCalledHom=0.001,
				 prHetCalledHet=0.995,
				 prHomInNormal=0.8,
				 prHomInRoh=0.999,
				 annotationPkg,
				 log.it=TRUE,
				 ...){
	if(!ICE){
		p <- prGtHom
		prGenotypeMissing <- prGtMis
		stopifnot(length(p) == S)
		if(!is.numeric(object)) stop("genotypes must be integers (1=AA, 2=AB, 3=BB) or NA (missing)")
		emission <- array(NA, dim=c(nrow(object), ncol(object), S))
		missingGT <- any(is.na(object))
		for(s in seq_len(S)){
			tmp <- object
			tmp[tmp == 1 | tmp == 3] <- p[s]
			tmp[tmp == 2] <- 1-p[s]
			index1 <- is.na(tmp) & !is.snp
			index2 <- is.na(tmp) & is.snp
			if(missingGT){
				tmp[index2] <- prGenotypeMissing[s]
				tmp[index1] <- 1/S
			}
			emission[, , s] <- tmp
		}
		if(log.it) emit <- log(emission) else emit <- emission
	} else {
		not.valid <- invalidGtConfidence(gt.conf)
		if(any(not.valid)){
			stop("Invalid genotype confidence scores.\n",
			     "\tIf ICE is TRUE, all confidence scores must be between 0 and 1")
		}
		logemit <- array(NA, dim=c(nrow(object), ncol(object), S))
		pkg <- strsplit(annotationPkg, "Crlmm")[[1]][[1]]
		tmp <- genotypeEmissionCrlmm(object, gt.conf=gt.conf,
					     cdfName=pkg,
					     prHetCalledHom,
					     prHetCalledHet,
					     prHomInNormal,
					     prHomInRoh)
		logemit[, , rohIndex] <- tmp[, , "ROH"]
		logemit[, , -rohIndex] <- tmp[, , "normal"]
		if(log.it) emit <- logemit else emit <- exp(logemit)
	}
	return(emit)
}

assayDataListDims <- function(object) {
  nms <- if (assayDataStorageMode(object) == "list") names(object) else ls(object)
	if (length(nms) == 0)
          return(matrix(integer(0), nrow = 2, ncol = 0,
                        dimnames = list(c("Features", "Samples"), character(0))))
  d <- lapply(nms, function(i) lapply(object[[i]], dim)) ##dim(object[[i]]))
  names(d) <- nms
  return(d)
}

validAssayDataDims <- function(object){
  msg <- NULL
  d <- assayDataListDims(object)
  firstElement <- d[[1]]
  d <- d[-1]
  res <- sapply(d, function(i) identical(i, firstElement))
  ## check that the 3rd dimension is 3
  if(!all(res)){
    msg <- "Assay data elements must have the same dimension"
  }
  if(is.null(msg)) return(TRUE) else return(msg)
}

assayDataStorageMode <- Biobase:::assayDataStorageMode
setMethod("calls", signature(object="BeadStudioSet"), function(object) assayDataElement(object, "call"))

setAs("BeadStudioSet", "data.frame",
      function(from, to){
	      ##isint <- isInteger(from)
	      isint <- TRUE
	      logr.ratio <- as.numeric(lrr(from))
	      if(isint) logr.ratio <- logr.ratio/100
	      bf <- as.numeric(assayDataElement(from, "baf"))
	      if(isint) bf <- bf/1000
	      gt.present <- "call" %in% ls(assayData(from))
	      if(gt.present){
		      gt <- as.integer(calls(from))
	      }
	      x <- rep(position(from)/1e6, ncol(from))
	      ##x <- rep(position(object)[marker.index], 4)/1e6
	      is.snp <- rep(isSnp(from), ncol(from))
	      id <- rep(sampleNames(from), each=nrow(from))
	      if(!gt.present){
		      df <- data.frame(x=x,
				       lrr=logr.ratio,
				       baf=bf,
				       id=id,
				       is.snp=is.snp,
				       stringsAsFactors=FALSE)
	      } else {
		      df <- data.frame(x=x,
				       lrr=logr.ratio,
				       gt=gt,
				       baf=bf,
				       id=id,
				       is.snp=is.snp,
				       stringsAsFactors=FALSE)
	      }
	      return(df)
      })

#' Deprecated BeadStudioSet and BeadStudioSetList
#'
#' The BeadStudioSet, BeadStudioSetList classes are deprecated.  The
#' corresponding constructor and classes are provided only for
#' compatability with older versions and will be defunct at the next
#' release.
#'
#' Use the replacement function SnpArrayExperiment.  For reading in
#' plain text source files such as GenomeStudio-processed data
#' (Illumina platform), see \code{\link{CopyNumScanParams}} and
#' \code{\link{parseSourceFile}}.
#'
#' @name BeadStudioSet
#' @aliases BeadStudioSet BeadStudioSetList BeadStudioSetList,BeadStudioSet-method BeadStudioSetList,character-method
#' @keywords internal
#' @param filenames full path to input files
#' @param header_info header information
#' @param genome character string indicating genome build
#' @param annotationPkg name of annotation package
#' @param chromosome which chromosomes to process
#' @param ...
#' @rdname BeadStudioSet
#' @seealso \code{link{CopyNumScanParams}} \code{\link{parseSourceFile}}
#' @export
BeadStudioSet <- function(filenames,
                          header_info,
			  genome=c("hg19", "hg18"),
			  annotationPkg, chromosome=1:22, ...){
  .Deprecated("SnpArrayExperiment")

  genome <- match.arg(genome)
  ## get the feature data
  tmp <- fread(filenames[1], nrows=1)
  select <- match(header_info, colnames(tmp))
  dat <- fread(filenames[1], select=select)
  R <- setRownames(as.matrix(dat[, "LRR"]), rownames(dat))
  feature_data <- GenomeAnnotatedDataFrameFrom(R,
                                               annotationPkg=annotationPkg,
                                               genome=genome)
  id <- make.unique(basename(filenames))
  if(length(filenames) > 1){
    datalist <- lapply(filenames[-1], fread, select=select)
    datalist[[length(datalist)+1]] <- dat
    names(datalist) <- c(id[-1], id[1])
    datalist <- datalist[id]
  } else datalist <- list(dat)
  ad <- createAssayData(datalist, id)
  obj <- new("BeadStudioSet",
             assayData=ad,
             annotation=annotationPkg,
             featureData=feature_data)
  uchrom <- unique(chromosome(obj))
  if(!all(uchrom %in% chromosome))
    obj <- obj[chromosome(obj) %in% chromosome, ]
  obj
}

createAssayData <- function(datalist, id){
  LRR <- setColnames(sapply(datalist, "[[", "LRR"), id)
  BAF <- setColnames(sapply(datalist, "[[", "BAF"), id)
  if("genotypes" %in% colnames(datalist[[1]])){
    genotypes <- setColnames(sapply(datalist, "[[", "genotypes"), id)
    ad <- assayDataNew("lockedEnvironment",
                       lrr=integerMatrix(LRR, 100),
                       baf=integerMatrix(BAF, 1000),
                       calls=genotypes)
  } else {
    ad <- assayDataNew("lockedEnvironment",
                       lrr=integerMatrix(LRR, 100),
                       baf=integerMatrix(BAF, 1000))
  }
  ad
}

setRownames <- function(object=nm, nm){
  rownames(object) <- nm
  object
}

setColnames <- function(object=nm, nm){
  colnames(object) <- nm
  object
}

initializeLrrAndBafMatrix <- function(nrow, ncol, col.names, outdir){
	ldPath(outdir)
	bafs <- initializeBigMatrix("baf", nr=nrow, nc=ncol, vmode="integer")
	lrrs <- initializeBigMatrix("lrr", nr=nrow, nc=ncol, vmode="integer")
	colnames(bafs) <- colnames(lrrs) <- col.names
	res <- list(baf=bafs, lrr=lrrs)
	return(res)
}

setMethod(BeadStudioSetList, "BeadStudioSet",
          function(x, ...){
            x <- x[order(chromosome(x), position(x)), ]
            chrom <- chromosome(x)
            pd <- phenoData(x)
            fd <- .splitGenomeAnnotatedDataFrame(featureData(x), chrom)
            r <- assayData(x)[["lrr"]]
            b <- assayData(x)[["baf"]]
            rlist <- lapply(split(r, chrom), as.matrix)
            blist <- lapply(split(b, chrom), as.matrix)
            ad <- AssayDataList(lrr=rlist, baf=blist)
            new("BeadStudioSetList",
                assayDataList=ad,
                featureDataList=fd,
                phenoData=pd,
                chromosome=as.character(unique(chrom)),
                annotation=x@annotation,
                genome=x@genome, ...)
          })


setMethod(BeadStudioSetList, "character",
          function(x, header_info,
                   genome=c("hg19", "hg18"),
                   annotationPkg,
                   chromosome=1:22, ...){
            bsset <- BeadStudioSet(x, header_info,
                                   genome,
                                   annotationPkg,
                                   chromosome, ...)
            BeadStudioSetList(bsset)
          })


setMethod("stack", signature(x="BeadStudioSetList"),
	  function(x, ...){
            b <- baf(x)
            Rs <- sapply(b, nrow)
            Cs <- ncol(b[[1]])
            logRR <- bf <- matrix(NA, sum(Rs), Cs)
            chrom <- factor(rep(chromosome(x), Rs), ordered=TRUE, levels=chromosome(x))
            index <- split(seq_len(sum(Rs)), chrom)
            for(i in seq_along(x)){
              j <- index[[i]]
              bf[j, ] <- baf(x[[i]])[,]
              logRR[j, ] <- lrr(x[[i]])[,]
            }
            fns <- as.character(unlist(Biobase::featureNames(x)))
            dimnames(bf) <- dimnames(logRR) <- list(fns, sampleNames(x[[1]]))
            pos <- unlist(position(x))
            issnp <- unlist(lapply(x@featureDataList, isSnp))
            featureData <- new("GenomeAnnotatedDataFrame",
                               position=pos,
                               chromosome=as.integer(rep(chromosome(x), Rs)),
                               isSnp=issnp,
                               row.names=fns)
            featureData <- featureData[match(fns, featureNames(featureData)), ]
            ##all.equal(featureNames(featureData), rownames(bf))
            obj <- new("BeadStudioSet",
                       baf=bf,
                       lrr=logRR,
                       featureData=featureData,
                       phenoData=phenoData(x),
                       annotation=annotation(x),
                       genome=genomeBuild(x))
            ##is.integer=isInteger(x))
            return(obj)
	  })

stackFeatureDataList <- function(x){
  fns <- unlist(lapply(x, featureNames))
  pos <- unlist(lapply(x, position))
  issnp <- unlist(lapply(x, isSnp))
  chrom <- unlist(lapply(x, chromosome))
  featureData <- new("GenomeAnnotatedDataFrame",
                     position=pos,
                     chromosome=chrom,
                     isSnp=issnp,
                     row.names=fns)
}


setMethod("split", c("GenomeAnnotatedDataFrame", "ANY"),
          function(x, f, drop=FALSE, ...){
            .splitGenomeAnnotatedDataFrame(x, f, drop=drop, ...)
})

.splitGenomeAnnotatedDataFrame <- function(x, f, drop=FALSE, ...){
  chrom <- chromosome(x)
  pos <- position(x)
  poslist <- split(pos, f)
  chrlist <- split(chrom, f)
  issnp <- split(isSnp(x), f)
  idlist <- split(featureNames(x), f)
  constructFeatureData <- function(pos, chr, id, is_snp){
    new("GenomeAnnotatedDataFrame",
        isSnp=is_snp,
        position=pos,
        chromosome=chr,
        row.names=id)
  }
  fdlist <- mapply(constructFeatureData, pos=poslist, chr=chrlist,
                   id=idlist, is_snp=issnp)
  names(fdlist) <- unique(chrom)
  fdlist
}

#' @aliases baf,SnpSet2-method
#' @rdname Defunct
setMethod("baf", signature(object="SnpSet2"), function(object) assayDataElement(object, "baf"))

##setMethod("SummarizedHMM", "missing", function(forward_backward,
##                                               loglik,
##                                               rowRanges=HmmGRanges()){
##  new("SummarizedHMM")
##})
##
##forwardBackwardAssays <- function(x, ...){
##  assays <- GenomicRanges:::.ShallowSimpleListAssays(
##    data = SimpleList(forward_backward=x, ...))
##  assays
##}
##
##setMethod("SummarizedHMM", "matrix", function(forward_backward,
##                                              loglik,
##                                              rowRanges=HmmGRanges()){
##  new("SummarizedHMM", assays=forwardBackwardAssays(),
##      rowRanges=HmmGRanges(), ...)
##})

setMethod(NA_filter, "oligoSnpSet", function(x){
  xx <- list(baf(x)[,1], copyNumber(x)[,1], start(x))
  i <- NA_index(xx)
  if(length(i) > 0) x <- x[i, ]
  x
})


setAs("oligoSnpSet", "SnpArrayExperiment",
      function(from){
        rowranges <- as(featureData(from), "SnpGRanges")
        coldata <- as(phenoData(from), "DataFrame")
        SnpArrayExperiment(cn=copyNumber(from), baf=baf(from),
                           rowRanges=rowranges,
                           colData=coldata)
      })

setMethod("chromosome", "oligoSnpSet", function(object) as.character(chromosome(featureData(object))))

#' Subset method for deprecated \code{oligoSetList}
#'
#' The \code{oligoSetList} class is deprecated.  Use
#' \code{\link{SnpArrayExperiment}} instead.
#'
#' @aliases [[,oligoSetList,ANY,ANY-method
#' @rdname oligoSetList
#' @param x a \code{oligoSetList} object
#' @param i A length-one numeric vector specifying which chromosome to extract
#' @param j A numeric vector specifying samples to extract (optional)
#' @param ... Ignored
#' @param exact Ignored
#' @export
setMethod("[[", signature(x="oligoSetList"),
	  function(x, i, j, ..., exact=TRUE){
            if(missing(i)) return(x)
            if(length(i) == 1){
              r <- copyNumber(x)[[i]]
              b <- baf(x)[[i]]
              gt <- calls(x)[[i]]
              gtp <- snpCallProbability(x)[[i]]
              colnames(r) <- colnames(b) <- colnames(gt) <- colnames(gtp) <- sampleNames(phenoData(x))
              fd <- featureDataList(x)[[i]]
              x <- new("oligoSnpSet",
                       call=gt,
                       callProbability=gtp,
                       copyNumber=r,
                       baf=b,
                       phenoData=phenoData(x),
                       featureData=fd,
                       genome=genomeBuild(x),
                       annotation=annotation(x))
            } else {
              stop("subscript out of bounds")
            }
	  })

## simulateSingleDupBaf <- function(b, is.snp, from, to, ...){
## 	stopifnot(is(b, "numeric"))
## 	names(b) <- NULL
## 	b.all <- b
## 	b <- b[is.snp]
## 	sds <- VanillaICE:::updateSigma(b, is.snp=rep(TRUE, length(b)), sigma0=c(0.02, 0.04, 0.02))
##
## 	p <- 0.5
## 	q3 <- (1-p)^3
## 	p2q <- p^2*(1-p)
## 	pq2 <- p*(1-p)^2
## 	p3 <- p^3
## 	##tmp <- cbind(q3, 3*pq2, 3*p2q, p3)
## 	##stopifnot(all(rowSums(tmp) == 1))
## 	index <- seq(from, to, by=1)
##
## 	z <- sample(1:4, size=length(index), replace=TRUE, prob=c(q3, 3*pq2, 3*p2q, p3))
## 	nZ <- table(z)
## 	d1 <- rtnorm(length(index), 0, sds[1], lower=0, upper=1)
## 	d2 <- rtnorm(length(index), 1/3, sds[1], lower=0, upper=1)
## 	d3 <- rtnorm(length(index), 2/3, sds[1], lower=0, upper=1)
## 	d4 <- rtnorm(length(index), 1, sds[3], lower=0, upper=1)
## 	simB <- rep(NA, length(index))
## 	simB[z==1] <- sample(d1, nZ[1])
## 	simB[z==2] <- sample(d2, nZ[2])
## 	simB[z==3] <- sample(d3, nZ[3])
## 	simB[z==4] <- sample(d4, nZ[4])
## 	b.all[index] <- simB
## 	##het <- rtnorm(length(index), 0.5, sds[2], lower=0, upper=1)
## 	return(b.all)
## }
##
## simulateDoubleDupBaf <- function(b, is.snp, from, to, ...){
## 	stopifnot(is(b, "numeric"))
## 	names(b) <- NULL
## 	b.all <- b
## 	b <- b[is.snp]
## 	sds <- VanillaICE:::updateSigma(b, is.snp=rep(TRUE, length(b)), sigma0=c(0.02, 0.04, 0.02))
## 	p <- 0.5
## 	q4 <- (1-p)^4
## 	pq3 <- p*(1-p)^3
## 	p2q2 <- p^2*(1-p)^2
## 	p3q <- p^3*(1-p)
## 	p4 <- p^4
## 	##tmp <- cbind(q3, 3*pq2, 3*p2q, p3)
## 	##stopifnot(all(rowSums(tmp) == 1))
## 	index <- seq(from, to, by=1)
## 	z <- sample(1:5, size=length(index), replace=TRUE, prob=c(q4, pq3, p2q2, p3q, p4))
## 	nZ <- table(z)
## 	d1 <- rtnorm(length(index), 0, sds[1], lower=0, upper=1)
## 	d2 <- rtnorm(length(index), 1/4, sds[1], lower=0, upper=1)
## 	d3 <- rtnorm(length(index), 1/2, sds[2], lower=0, upper=1)
## 	d4 <- rtnorm(length(index), 3/4, sds[1], lower=0, upper=1)
## 	d5 <- rtnorm(length(index), 1, sds[3], lower=0, upper=1)
## 	simB <- rep(NA, length(index))
## 	simB[z==1] <- sample(d1, nZ[1])
## 	simB[z==2] <- sample(d2, nZ[2])
## 	simB[z==3] <- sample(d3, nZ[3])
## 	simB[z==4] <- sample(d4, nZ[4])
## 	simB[z==5] <- sample(d5, nZ[5])
## 	b.all[index] <- simB
## 	return(b.all)
## }
##
## simulateSingleDelBaf <- function(b, is.snp, from, to, ...){
## 	index <- seq(from, to, by=1)
## 	stopifnot(all(diff(index) > 0))
## 	stopifnot(length(index) > 1)
## 	stopifnot(is(b, "numeric"))
## 	names(b) <- NULL
## 	b.all <- b
## 	b <- b[is.snp]
## 	sds <- VanillaICE:::updateSigma(b, is.snp=rep(TRUE, length(b)), sigma0=c(0.02, 0.04, 0.02))
## 	p <- 0.5
## 	q <- 1-p
##
## 	z <- sample(1:2, size=length(index), replace=TRUE, prob=c(p, q))
## 	nZ <- table(z)
## 	d1 <- rtnorm(length(index), 0, sds[1], lower=0, upper=1)
## 	d2 <- rtnorm(length(index), 1, sds[3], lower=0, upper=1)
## 	simB <- rep(NA, length(index))
## 	simB[z==1] <- sample(d1, nZ[1])
## 	simB[z==2] <- sample(d2, nZ[2])
## 	b.all[index] <- simB
## 	##het <- rtnorm(length(index), 0.5, sds[2], lower=0, upper=1)
## 	return(b.all)
## }
##
## artificialData <- function(states, nmarkers){
## 	data(oligoSetExample, package="oligoClasses")
## 	oligoSet <- chromosomePositionOrder(oligoSet)
## 	pos <- position(oligoSet)
## 	state.path <- rep(states, nmarkers)
## 	copynumber <- rep(2, length(state.path))
## 	copynumber[state.path==2] <- 1.5##bias
## 	copynumber[state.path==5] <- 2.5##bias
## 	genotypes <- rep(NA, length(copynumber))
## 	gt <- rmultinom(n=length(copynumber), size=1, prob=rep(1/3,3))
## 	genotypes[gt[1, ] == 1] <- 1L
## 	genotypes[gt[2, ] == 1] <- 2L
## 	genotypes[gt[3, ] == 1] <- 3L
## 	genotypes[state.path==4 | state.path==2] <- 1L
## 	genotypes <- as.matrix(genotypes)
## 	## make signal fairly obvious
## 	sigmas <- rgamma(length(copynumber), 4, scale=0.05)
## 	b <- rbaf(as.matrix(genotypes), sigma=0.01, epsilon=0.001, states=state.path)
## 	dat <- as.matrix(rnorm(length(state.path), mean=copynumber, sd=sigmas))
## 	i <- seq_along(state.path)
## 	rownames(dat) <- rownames(genotypes) <- featureNames(oligoSet)[i]
## 	##pos <- seq(1, by=3e3, length.out=length(copynumber))
## 	object <- new("oligoSnpSet",
## 		      copyNumber=integerMatrix(dat, scale=100),
## 		      call=as.matrix(genotypes),
## 		      callProbability=snpCallProbability(oligoSet)[i, , drop=FALSE])
## 	##baf(object) <- b
## 	assayDataElement(object, "baf") <- integerMatrix(b, scale=1000)
## 	##df <- data.frame(position=pos, chromosome=rep(1L, length(pos)), isSnp=
## 	fData(object)$position <- as.integer(pos[i])
## 	fData(object)$chromosome <- 1L
## 	fData(object)$isSnp <- TRUE
## 	return(object)
## }
##
## rbaf <- function(genotypes, sigma, epsilon, states){
## 	baf <- matrix(NA, nrow(genotypes), ncol(genotypes))
## 	Ns <- table(genotypes)
## 	a <- pnorm(0, mean=0, sd=sigma)
## 	b <- pnorm(1, mean=0, sd=sigma)
## 	I <- runif(Ns[1], 0, 1) > epsilon
## 	baf[genotypes==1] <- I*qnorm(a+runif(Ns[1], 0, b-a), mean=0, sd=sigma) + (1-I)*runif(Ns[1], 0, 1)
## 	I <- runif(Ns[2], 0, 1) > epsilon
## 	baf[genotypes==2] <- I*rnorm(Ns[2], mean=0.5, sd=sigma*2) + (1-I)*runif(Ns[2], 0, 1)
## 	a <- pnorm(0, mean=1, sd=sigma)
## 	b <- pnorm(1, mean=1, sd=sigma)
## 	I <- runif(Ns[3], 0, 1) > epsilon
## 	baf[genotypes==3] <- I*qnorm(a+runif(Ns[3], 0, b-a), mean=1, sd=sigma) + (1-I) * runif(Ns[3], 0, 1)
##
## 	## assume 1/2 are hets to make it easy
## 	ndup <- sum(states==5)
## 	ndup.het <- ceiling(ndup/2)
## 	ndup.hom <- ndup-ndup.het
##
## 	index5 <- which(states==5)
## 	index25 <- sample(index5, ceiling(ndup.het/2))
## 	index5 <- setdiff(index5, index25)
##
## 	index75 <- setdiff(index5, floor(ndup.het/2))
## 	indexhom <- setdiff(index5, index75)
##
## 	n25 <- length(index25)
## 	n75 <- length(index75)
## 	nhom <- length(index5)
## 	I <- runif(n25, 0, 1) > epsilon
## 	baf[index25] <- I*rnorm(n25, mean=1/3, sd=sigma*2) + (1-I)*runif(n25, 0, 1)
## 	I <- runif(n75, 0, 1) > epsilon
## 	baf[index75] <- I*rnorm(n75, mean=2/3, sd=sigma*2) + (1-I)*runif(n75, 0, 1)
## 	a <- pnorm(0, mean=1, sd=sigma)
## 	b <- pnorm(1, mean=1, sd=sigma)
## 	baf[indexhom] <- qnorm(a+runif(Ns[3], 0, b-a), mean=1, sd=sigma)
## 	rownames(baf) <- rownames(genotypes)
## 	return(baf)
## }

#'   Wrapper function for fitting the viterbi algorithm
#'
#'   The viterbi algorithm, implemented in C, estimates the optimal
#' state path as well as the forward and backward variables that are
#' used for updating the mean and variances in a copy number HMM.  The
#' function \code{viterbi2Wrapper} should not be called directly by
#' the user. Rather, users should fit the HMM by passing an
#' appropriate container to the method \code{hmm}.  We document the
#' \code{viterbi2Wrapper} arguments as several of the arguments can be
#' modified from their default value when passed from the \code{hmm}
#' method through the \code{...}. In particular, \code{nupdates},
#' \code{p.hom}, and \code{prOutlierBaf}.
#'
#' @param index.samples Index for the samples that are to be processed.
#' @param cnStates     \code{numeric} vector for the initial copy number state means.
#' @param prOutlierBAF     A list with elements 'initial', 'max', and 'maxROH' corresponding to the initial estimate of the probability that a B allele frequency (BAF) is an outlier, the maximum value for this parameter over states that do not involve homozygous genotypes, and the maximum value over states that assume homozygous genotypes.  This parameter is experimental and could be used to fine tune the HMM for different platforms. For example, the BAFs for the Affy platform are typically more noisey than the BAFs for Illumina.  One may want to set small values of these parameters for Illumina (e.g, 1e-5, 1e-3, and 1e-5) and larger values for Affy (e.g., 1e-3, 0.01, 1e-3).
#' @param p.hom     \code{numeric}: weight for observing homozygous genotypes.  For value \code{0}, homozygous genotypes / B allele frequencies have the same emission probability in the 'normal' state as in the states hemizygous deletion and in copy-neutral region of homozygosity.  Regions of homozygosity are common in normal genomes. For small values of \code{p.hom}, hemizygous deletions will only be called if the copy number estimates show evidence of a decrease from normal.
#' @param is.log     \code{logical}: Whether the copy number estimates in the \code{r} matrix are on the log-scale.
#' @param limits     \code{numeric} vector of length two specifying the range of the copy number estimates in \code{r}. Values of \code{r} outside of this range are truncated. See \code{copyNumberLimits}.
#' @param normalIndex     \code{integer} specifying the index for the normal state.  Note that states must be ordered by the mean of the copy number state. E.g., state 1 is homozygous deletion (0 copies), state 2 is hemizygous deletion (1 copy), normal (2 copies), ...  In a 6-state HMM, normalIndex should be 3.
#' @param nupdates     \code{integer} specifying the maximum number of iterations for reestimating the mean and variance for each of the copy number states.  The number of iterations may be fewer than \code{nupdates} if the difference in the log-likelihood between successive iterations is less than \code{tolerance}.
#' @param tolerance     \code{numeric} value for indicating convergence of the log-likelihood. If the difference in the log-likelihood of the observed data given the HMM model at iteration i and i-1 is less than tolerance, no additional updates of model parameters using the EM algorithm is needed.
#' @param computeLLR     Logical.  Whether to compute a log likelihood ratio (LLR) comparing the predicted state to the normal state. This is calculated post-hoc and is not precisely the likelihood estimated from the Viterbi algorithm. When \code{FALSE}, the LLR is not calculated and the algorithm is slightly faster.
#' @param returnEmission     Logical. If TRUE, an array of emission probabilities are returned.  The dimensions of the array are SNPs, samples, and copy number states.
#' @param verbose     Logical. Whether to print some of the details of the processing.
#' @param grFun An R function for coercing the state-path from the HMM
#'   to a GRanges object. Takes advantage of lexical scope.
#' @param matrixFun An R function for subsetting the assay data (takes
#'    advantage of lexical scope).
#' @param snp.index The SNP indices
#' @param anyNP An indicator for whether any of the markers are
#'  nonpolymorphic, and therefore BAFs / genotypes are ignored
#' @return   A \code{GRanges} object if \code{returnEmission} is FALSE.  Otherwise,
#'  an array of emission probabilities is returned.
#' @keywords smooth
#' @export
viterbi2Wrapper <- function(index.samples,
			    cnStates,
			    prOutlierBAF=list(initial=1e-5, max=1e-3, maxROH=1e-5),
			    p.hom=0.05,
			    is.log,
			    limits,
			    normalIndex=3L,
			    nupdates=10,
			    tolerance=5,
			    computeLLR=TRUE,
			    returnEmission=FALSE,
			    verbose=FALSE,
			    grFun,
			    matrixFun,
			    snp.index,
			    anyNP){
  toGRanges <- grFun$toGRanges
  transitionProbs <- grFun$transitionProbs
  tau <- grFun$tau
  b.r <- matrixFun(index.samples)
  b <- b.r[["b"]]
  r <- b.r[["r"]]
  rm(b.r, grFun); gc()
  nc <- ncol(r); nr <- nrow(r); S <- length(cnStates)
  ids <- colnames(r)
  ##if(center) r <- centerCopyNumber(r, is.snp) + cnStates[normalIndex]
  r <- thresholdCopyNumber(r, limits=limits)
  updateFun <- generatorFun(r, b,
                            snp.index=snp.index,
                            cnStates=cnStates,
                            normalIndex=normalIndex,
                            tau=tau,
                            limits=limits,
                            prOutlierBAF=prOutlierBAF,
                            p.hom=p.hom,
                            is.log=is.log,
                            computeLLR=computeLLR,
                            verbose=verbose,
                            transitionProbs=transitionProbs)
  statePath <- matrix(NA, nrow(r), nc)
  grl <- vector("list", nc)
  if(returnEmission) emitArray <- array(NA, dim=c(nrow(r), nc, length(cnStates)))
  ## Things that change with iterator i:
  ##  --  first two moments
  ##  --  mixture probabilities
  if(anyNP) emitB <- matrix(1, nrow(r), length(cnStates))
  for(j in seq_len(nc)){
    paramsB <- updateFun$initialBafParams()
    paramsC <- updateFun$initialCnParams(j)
    ## if we subset r and bf here we wouldn't need to do "[" for each update i
    bf <- b[, j]
    cn <- r[, j]
    sameStatePath <- 0
    llr <- loglik <- rep(0, nupdates)
    for(i in seq_len(nupdates)){
      paramsBprev <- paramsB
      paramsCprev <- paramsC
      if(anyNP){
        emitB[snp.index, ] <- updateFun$updateBafEmission(bf, paramsB, j)
      } else emitB <- updateFun$updateBafEmission(bf, paramsB, j)
      emitC <- updateFun$updateCnEmission(cn, paramsC, j)
      emit <- emitB*emitC
      rm(emitC)
      vit.res <- updateFun$fitViterbi(emit)
      if(i > 1) sameStatePath <- if(identical(statePath, vit.res[["statePath"]])) sameStatePath+1 else 0
      statePath <- vit.res[["statePath"]]
      h <- matrix(vit.res[["fv"]]*vit.res[["bv"]],
                  nr, S)
      if(anyNP){
        paramsB <- updateFun$updateBafParams(bf, paramsB, h[snp.index, ], j)
      } else paramsB <- updateFun$updateBafParams(bf, paramsB, h, j)
      if(verbose) print(paramsB)
      paramsC <- updateFun$updateCnParams(cn, paramsC, h, j)
      if(verbose) print(paramsC)
      ## can we compare the scale factor across runs?
      ## Equation 103, Rabiner
      loglik[i] <- sum(log(vit.res[["sf"]]))
      if(verbose) message(loglik[i])
      ##if(i == 1) next()
      if(i > 1){
        llr[i] <- loglik[i]-loglik[i-1]
        cond <- (abs(llr[i]) < tolerance) || (sameStatePath > 1)
      } else cond <- FALSE
      if(cond | i == nupdates) {
        if(!returnEmission){
          statePath <- vit.res[["statePath"]]
          ##gr <- updateFun$toGRanges(statePath, j)
          gr <- toGRanges(statePath, ids[j])
          if(computeLLR) {
            llr <- updateFun$computeLogLikRatio(gr, emit)
          }
          grl[[j]] <- gr
        }
        break()
      }
    }
    if(returnEmission)  emitArray[, j, ] <- emit
  }
  if(returnEmission) {
    result <- emitArray
  } else {
    grl <- GRangesList(grl)
    names(grl) <- colnames(r)
    result <- unlist(grl)
  }
  return(result)
}

viterbiWrapperG2 <- function(index.samples,
			     cnStates,
			     p.hom=0.05,
			     is.log,
			     limits,
			     normalIndex=3L,
			     nupdates=10,
			     tolerance=5,
			     computeLLR=TRUE,
			     returnEmission=FALSE,
			     verbose=FALSE,
			     grFun,
			     matrixFun,
			     is.snp,
			     anyNP,
			     ...){
	toGRanges <- grFun$toGRanges
	transitionProbs <- grFun$transitionProbs
	tau <- grFun$tau
	b.r <- matrixFun(index.samples)
	g <- b.r[["g"]]
	r <- b.r[["r"]]
	rm(b.r, grFun); gc()
	nc <- ncol(r); nr <- nrow(r); S <- length(cnStates)
	ids <- colnames(r)
	r <- thresholdCopyNumber(r, limits=limits)
	updateFun <- generatorFunG2(r,
				    cnStates=cnStates,
				    normalIndex=normalIndex,
				    tau=tau,
				    limits=limits,
				    p.hom=p.hom,
				    is.log=is.log,
				    computeLLR=computeLLR,
				    verbose=verbose,
				    transitionProbs=transitionProbs)
	statePath <- matrix(NA, nrow(r), nc)
	grl <- vector("list", nc)
	emitG <- gtEmission(object=g,
			    is.snp=is.snp,
			    rohIndex=c(2L, 4L),
			    prGtHom=c(2/3, 0.99, 0.7, 0.99, 1/2, 2/5),
			    S=length(cnStates),
			    log.it=FALSE)
	if(returnEmission) emitArray <- array(NA, dim=c(nrow(r), nc, length(cnStates)))
	## Things that change with iterator i:
	##  --  first two moments
	##  --  mixture probabilities
	if(anyNP) emitB <- matrix(1, nrow(r), length(cnStates))
	for(j in seq_len(nc)){
		paramsC <- updateFun$initialCnParams(j)
		cn <- r[, j]
		sameStatePath <- 0
		llr <- loglik <- rep(0, nupdates)
		for(i in seq_len(nupdates)){
			paramsCprev <- paramsC
			emitC <- updateFun$updateCnEmission(cn, paramsC, j)
			emit <- emitG[,j,]*emitC
			rm(emitC)
			vit.res <- updateFun$fitViterbi(emit)
			if(i > 1) sameStatePath <- if(identical(statePath, vit.res[["statePath"]])) sameStatePath+1 else 0
			statePath <- vit.res[["statePath"]]
			h <- matrix(vit.res[["fv"]]*vit.res[["bv"]],
				    nr, S)
			paramsC <- updateFun$updateCnParams(cn, paramsC, h, j)
			if(verbose) print(paramsC)
			## can we compare the scale factor across runs?
			## Equation 103, Rabiner
			loglik[i] <- sum(log(vit.res[["sf"]]))
			if(verbose) message(loglik[i])
			if(i == 1) next()
			llr[i] <- loglik[i]-loglik[i-1]
			cond <- (abs(llr[i]) < tolerance) || (sameStatePath > 1)
			if(cond | i == nupdates) {
				statePath <- vit.res[["statePath"]]
				gr <- toGRanges(statePath, ids[j])
				if(computeLLR) {
					llr <- updateFun$computeLogLikRatio(gr, emit)
				}
				grl[[j]] <- gr
				break()
			}
		}
		if(returnEmission)  emitArray[, j, ] <- emit
	}
	if(returnEmission) {
		result <- emitArray
	} else {
		grl <- GRangesList(grl)
		names(grl) <- colnames(r)
		result <- grl
	}
	return(unlist(result))
}


viterbiWrapperG <- function(r, gt, pos, is.snp, cnStates,
			     chrom,
			     p.hom=0.05,
			     TAUP=1e8,
			     is.log,
			     center=TRUE,
			     limits,
			     initialProb=rep(1/length(cnStates), length(cnStates)),
			     normalIndex=3L,
			     nupdates=10,
			     tolerance=5,
			     computeLLR=TRUE,
			     ...){
	emitG <- gtEmission(object=gt, is.snp=is.snp, rohIndex=c(2L, 4L), prGtHom=c(2/3, 0.99, 0.7, 0.99, 1/2, 2/5), S=length(cnStates), log.it=FALSE)
	nc <- ncol(r)
	updateFun <- generatorFunG(r, gt, is.snp=is.snp,
				   cnStates=cnStates, normalIndex=normalIndex,
				   TAUP=TAUP, limits=limits, center=center,
				   position=pos, is.log=is.log,
				   computeLLR=computeLLR, chrom=chrom)
	statePath <- matrix(NA, nrow(r), nc)
	grl <- vector("list", nc)
	## Things that change with iterator i:
	##  --  first two moments
	##  --  mixture probabilities
	## pseudocode
	for(j in seq_len(nc)){
		paramsC <- updateFun$initialCnParams(j)
		##gt <- g[, j]
		cn <- r[, j]
		sameStatePath <- 0
		llr <- loglik <- rep(0, nupdates)
		for(i in seq_len(nupdates)){
			paramsCprev <- paramsC
			emitC <- updateFun$updateCnEmission(cn, paramsC, j)
			emit <- emitG[,j,]*emitC
			vit.res <- updateFun$fitViterbi(emit)
			if(i > 1) sameStatePath <- if(identical(statePath, vit.res[["statePath"]])) sameStatePath+1 else 0
			statePath <- vit.res[["statePath"]]
			fv <- vit.res[["fv"]]
			bv <- vit.res[["bv"]]
			##paramsB <- updateFun$updateBafParams(bf, paramsB, fv, bv, j)
			paramsC <- updateFun$updateCnParams(cn, paramsC, fv, bv, j)
			loglik[i] <- sum(log(vit.res[["sf"]]))
			if(i == 1) next()
			llr[i] <- loglik[i]-loglik[i-1]
			cond <- abs(llr[i]) < tolerance || sameStatePath>1
			if(cond | i == nupdates) {
				statePath <- vit.res[["statePath"]]
				gr <- updateFun$toGRanges(statePath, j)
				if(computeLLR) elementMetadata(gr)$LLR <- updateFun$computeLogLikRatio(gr, emit)
				grl[[j]] <- gr
				break()
			}
		}
	}
	grl <- GRangesList(grl)
	names(grl) <- colnames(r)
	return(grl)
}

viterbiForSnpSet2 <- function(index.samples,
			      grFun,
			      matrixFun,
			      S=2L,
			      rohIndex,
			      normalIndex=1L,
			      computeLLR=TRUE,
			      verbose=FALSE,
			      ...){
	toGRanges <- grFun$toGRanges
	transitionProbs <- grFun$transitionProbs
	tau <- grFun$tau
	g <- matrixFun(index.samples)
	nc <- ncol(g); nr <- nrow(g)
	ids <- colnames(g)
	statePath <- matrix(NA, nrow(g), nc)
	emitG <- gtEmissionFromMatrix(object=g,
				      rohIndex=rohIndex,
				      is.snp=rep(TRUE, nr),
				      S=S,
				      log.it=FALSE,
				      ...)
	updateFun <- generatorFunSnpSet(g=g,
					normalIndex=1L,
					tau=tau,
					computeLLR=computeLLR,
					verbose=verbose,
					transitionProbs=transitionProbs,
					S=S)
	grl <- vector("list", nc)
	J <- ncol(g)
	for(j in seq_len(J)){
		vit.res <- updateFun$fitViterbi(emitG[,j, ])
		statePath <- vit.res[["statePath"]]
		gr <- toGRanges(statePath, j)
		if(computeLLR) elementMetadata(gr)$LLR <- updateFun$computeLogLikRatio(gr, emitG[,j,])
		grl[[j]] <- gr
	}
	grl <- GRangesList(grl)
	names(grl) <- colnames(g)
	unlist(grl)
}

viterbiForSnpSetIce <- function(index.samples,
				grFun,
				matrixFun,
				S=2L,
				normalIndex=1L,
				rohIndex=normalIndex+1L,
				computeLLR=TRUE,
				verbose=FALSE,
				annotationPkg,
				...){
	toGRanges <- grFun$toGRanges
	transitionProbs <- grFun$transitionProbs
	tau <- grFun$tau
	res <- matrixFun(index.samples)
	g <- res[["g"]]
	gt.conf <- res[["gt.conf"]]
	rm(res);gc()
	nc <- ncol(g); nr <- nrow(g)
	ids <- colnames(g)
	statePath <- matrix(NA, nrow(g), nc)
	emitG <- gtEmissionFromMatrix(object=g,
				      gt.conf=gt.conf,
				      ICE=TRUE,
				      rohIndex=rohIndex,
				      is.snp=rep(TRUE, nr),
				      S=S,
				      log.it=FALSE,
				      annotationPkg=annotationPkg,
				      ...)
	updateFun <- generatorFunSnpSet(g=g,
					normalIndex=1L,
					tau=tau,
					computeLLR=computeLLR,
					verbose=verbose,
					transitionProbs=transitionProbs,
					S=S)
	grl <- vector("list", nc)
	J <- ncol(g)
	for(j in seq_len(J)){
		vit.res <- updateFun$fitViterbi(emitG[,j, ])
		statePath <- vit.res[["statePath"]]
		gr <- toGRanges(statePath, j)
		if(computeLLR) elementMetadata(gr)$LLR <- updateFun$computeLogLikRatio(gr, emitG[,j,])
		grl[[j]] <- gr
	}
	grl <- GRangesList(grl)
	names(grl) <- colnames(g)
	unlist(grl)
}

rOverlaps <- function(gr1, gr2, fr){
	o1 <- findOverlaps(gr1, gr2)
	i <- queryHits(o1)
	j <- subjectHits(o1)
	if(length(i) > 1){
		g1 <- gr1[i, ]
		g2 <- gr2[j, ]
		ui <- unique(i)
		## some ranges not in gr1
		if(length(ui) < length(gr1)){
			gr1.only <- gr1[-i, ]
		}
		x <- intersect(g1, g2)
		n.probes.intersection <- countOverlaps(x, fr)
		p1 <- n.probes.intersection/numberProbes(g1)
		p2 <- n.probes.intersection/numberProbes(g2)
		ans <- sum(p1 > 0.5 & p2 > 0.5)/length(gr1)
	} else {
		ans <- 0
	}
	return(ans)
}
reciprocalOverlap <- function(gr1, gr2, fr){
	gr1 <- gr1[numberProbes(gr1) >= 10 & state(gr1) %in% c(1,2,5,6), ]
	gr2 <- gr2[numberProbes(gr2) >= 10 & state(gr2) %in% c(1,2,5,6), ]
	p1 <- rOverlaps(gr1, gr2, fr)
	p2 <- rOverlaps(gr2, gr1, fr)
	c(p1, p2)
}

usePennCNVTrioState <- function(x){
	f <- as.integer(substr(x, 1,1))
	m <- as.integer(substr(x, 2,2))
	o <- as.integer(substr(x, 3,3))
	indexScale <- function(x){
		y <- x
		if(any(y <= 2)) {
			x[y<=2] <- y[y<=2]+1
		}
		if(any(y > 2)){
			x[y>2] <- y[y>2]+2
		}
		return(x)
	}
	f <- indexScale(f)
	m <- indexScale(m)
	o <- indexScale(o)
	paste(f,m,o, sep="")
}
