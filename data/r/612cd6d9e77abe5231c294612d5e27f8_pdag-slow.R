# Part of the "parental" package, http://github.com/rbtgde/parental
# 
# This software is distributed under the GPL-3 license.  It is free,
# open source, and has the attribution requirements (GPL Section 7) in
#   http://github.com/rbtgde/parental
# 
# Note that it is required that attributions are retained with each function.
#
# Copyright 2008 Robert J. B. Goudie, University of Warwick

#' Undocumented.
#' 
#' A generic
#' 
#' @param pdag A pdag
#' @param ... Further arguments passed to methods
#' @export
maximallyOrientEdgesSlow <- function(pdag, ...){
  UseMethod("maximallyOrientEdgesSlow")
}

#' Undocumented.
#'
#' ...
#'
#' @param pdag ...
#' @param verbose A logical
#' @param ... Further arguments (unused)
#' @return ...
#' @export
maximallyOrientEdgesSlow.parental <- function(pdag, verbose = F, ...){
  # convert to adjacency matrix
  pdag <- as.adjacency(pdag)
  # maximally orient
  out <- maximallyOrientEdgesSlow(pdag, verbose)
  # convert -1 and 1 both back to 1s
  # the -1, 1 format is used by the internal functions
  out <- abs(out)
  # convert back to a parental
  as.parental(out)
}

# the C versions do not give the same answer as the R versions
# eg for 
# structure(list(4L, integer(0), 9L, integer(0), integer(0), c(11L, 
# 26L), integer(0), integer(0), 5L, 11L, c(22L, 28L), 9L, 5L, integer(0), 
#     integer(0), 24L, integer(0), c(26L, 29L), c(11L, 28L), integer(0), 
#     11L, 25L, integer(0), 30L, integer(0), c(22L, 28L), 21L, 
#     integer(0), c(22L, 28L), 6L), class = c("bn", "parental"))


#' Maximally orient edges.
#'
#' This is the R implementation
#'
#' @param pdag A pdag
#' @param verbose A logical of length 1.
#' @param ... Further arguments (unused)
#' @return Something?
#' @export
maximallyOrientEdgesSlow.matrix <- function(pdag, verbose = F, ...){
  #if (class("pdag") != "pdag"){
  #  stop("Not a pdag")
  #}
  
  # based on code by Imme Ebert-Uphoff (ebert@tree.com), 2007
  
  # uses Rules R1-R4 of Meek (1995) to complete
  # orientations in a pdag as far as possible, 
  # i.e. every compelled edge is oriented.
  #
  # (Rules R1-R4 are also summarized in Pearl (2000), p.51
  #  and Neapolitan (2004), p. 546.)
  #
  # Since the PC algorithm also uses Rules R1-R3, their implementation was 
  # copied (with some modifications) from function learn_struct_pdag_pc.
  #
  # Rule R4 is necessary here, since the orientations in the input 
  # pdag do not just represent v-structures.
  
  # the code below uses -1 for directed edge
  # and 1 for undirected edge
  # so need to set this up
  undirected_edges <- which(pdag == 1 & pdag != t(pdag), arr.ind = T)
  for (rownum in seq_len(nrow(undirected_edges))){
    i <- undirected_edges[rownum, 1]
    j <- undirected_edges[rownum, 2]
    pdag[i, j] <- -1
  }
  
  n <- nrow(pdag)
  old_pdag <- matrix(0, nrow = n, ncol = n)
  
  # recurse until we don't do anything
  while (!identical(pdag, old_pdag)){

    old_pdag <- pdag

    # Rule R1
    directed_edges <- NULL
    directed_edges <- which(pdag == -1, arr.ind = T) # a -> b
    for (rownum in seq_len(nrow(directed_edges))){
      a <- directed_edges[rownum, 1]
      b <- directed_edges[rownum, 2]
      undirected <- abs(pdag) + t(abs(pdag))
      # Adjacency test in undirected matrix:
      #   a adjacent b  <=>  undirected(a,b) ==0
      # That's easier to use than adjacency test in pdag:
      #   a adjacent b  <=>  pdag(a,b)==0 and pdag(b,a)==0

      # Find all nodes c such that  b-c  and c not adjacent a
      C <- which(pdag[b, ] == 1 & undirected[a,] == 0) 
      if (length(C) > 0){
        pdag[b, C] <- -1
        pdag[C, b] <- 0
        if (verbose){
          for (j in seq_along(C)){
            cat('Rule 1:', b, 'to', C[j], "\n")
          }
        }
      }
    }
    
    # Rule R2
    undirected_edges <- NULL
    # get all undirected edges
    undirected_edges <- which(pdag == 1, arr.ind = T)
    # for each undirected edge
    for (rownum in seq_len(nrow(undirected_edges))){
      # get the end points
      a <- undirected_edges[rownum, 1]
      b <- undirected_edges[rownum, 2]
      
      # it seems that we do not check for a route between the nodes
      # instead, it has to be a 2-stage path
      if (any(pdag[a, ] == -1 & pdag[, b] == -1)){
        pdag[a,b] <- -1
        pdag[b,a] <- 0
        
        if (verbose){
          cat('Rule 2:', a, 'to', b, "\n")
        }
      }
    }
    
    # Rule R3
    undirected_edges <- NULL
    undirected_edges <- which(pdag == 1, arr.ind = T) # unoriented a-b edge
    for (rownum in seq_len(nrow(undirected_edges))){
      a <- undirected_edges[rownum, 1]
      b <- undirected_edges[rownum, 2]
      C <- which(pdag[a,] == 1 & pdag[,b] == -1)
      # C contains nodes c s.t. a-c->b-a

      # Extract lines and columns corresponding only to the set of nodes C
      core <- pdag[C,C]

      # Prepare adjacency test:
      unoriented = abs(core) + t(abs(core)) 
      # Now:  a non-adjacent b <==> unoriented(a,b) == 0

      # Prepare to detect existence of non-adjacent pairs of nodes in C.
      # Set diagonal to 1, to prevent finding pairs of IDENTICAL nodes:
      diag(unoriented) <- 1

      # C contains 2 different non-adjacent elements
      if (any(unoriented == 0)){
        pdag[a,b] <- -1
        pdag[b,a] <- 0 
        if (verbose){
          cat('Rule 3:', a, 'to', b, "\n")
        }
      }
    }
    
    # Rule 4
    undirected_edges <- NULL
    undirected_edges <- which(pdag == 1, arr.ind = T) # unoriented a-b edge
    for (rownum in seq_len(nrow(undirected_edges))){
      a <- undirected_edges[rownum, 1]
      b <- undirected_edges[rownum, 2]

      # Prepare adjacency test:
      # unoriented(i,j) is 0 (non-adj) or 1 (directed) or 2 (undirected)
      unoriented <- abs(pdag) + t(abs(pdag))

      # Find c such that c -> b and a,c are adjacent (a-c or a->c or a<-c) 
      C = which((pdag[,b] == -1) & (unoriented[a,]>=1), arr.ind = T)
      for (j in seq_along(C)){
        c <- C[j]
        # Check whether there is any node d, such that
        # d->c  AND  a-d  AND  b NOT adjacent to d
        if (any(pdag[, c]== -1 & pdag[a, ] == 1 & unoriented[b, ] == 0)){
          pdag[a,b] = -1 
          pdag[b,a] = 0  
          if (verbose){
            cat('Rule 4:', a, 'to', b, "\n")
          }
        }
      }
    }
  }

  # Oriented all possible edges.  Return result.
  pdag
}

#' Undocumented.
#' 
#' A generic
#' 
#' @param pdag A pdag
#' @param ... Further arguments passed to method
#' @export
pdag2alldagsSlow <- function(pdag, ...){
  UseMethod("pdag2alldagsSlow")
}

#' Undocumented.
#' 
#' ....
#'
#' @param pdag A pdag
#' @param verbose ...
#' @param ... Further arguments (unused)
#' @return ....
#' @S3method pdag2alldagsSlow parental
#' @method pdag2alldagsSlow parental
pdag2alldagsSlow.parental <- function(pdag, verbose = F, ...){
  pdag <- as.adjacency(pdag)
  out <- pdag2alldagsSlow(pdag, verbose = verbose)
  out <- lapply(out, function(adj){
    adj <- abs(adj)
    as.bn(adj)
  })
  class(out) <- c("bn.list", "parental.list")
  out
}

#' Undocumented.
#' 
#' ....
#'
#' @param pdag ....
#' @param verbose ...
#' @param ... Further arguments (unused)
#' @return ....
#' @S3method pdag2alldagsSlow matrix
#' @method pdag2alldagsSlow matrix
pdag2alldagsSlow.matrix <- function(pdag, verbose = F, ...){
  stopifnot("matrix"        %in% class(pdag),
            dim(pdag)[1]    == dim(pdag)[2],
            class(verbose)  == "logical",
            length(verbose) == 1)
#
# [n_dags,dag_list] = pdag2alldagsSlow( pdag)
#
# generates a cell array of ALL Markov-equivalent DAGs 
# corresponding to a partially directed acyclic graph (PDAG).
#
# Input: PDAG  (PDAG does NOT have to be complete)
#     Format of pdag matrix:
#     Edge with known direction a->b  represented as pdag(a,b)=-1 pdag(b,a)=0
#     Edge with unknown direction a-b represented as pdag(a,b)=1  pdag(b,a)=1
#
# Output: Number of DAGs generated and
#         Cell array of all permissible extensions of PDAG
#
# Sample Use:  
#      # Use output of PC algorithm
#      dag = mk_rnd_dag(4)   # create random DAG
#      # Generate pdag through PC algorithm
#      pdag = learn_struct_pdag_pc('dsep', length(dag), 3, dag)
#      [n_dags,dag_list] = pdag2alldagsSlow(pdag)
#      n_dags  
#
# If you want to generate all DAGs that are Markov equivalent to an 
# input DAG (not a pattern), then use function Markov_equivalent_dags(dag)
# instead which calls this function.
#
# =======================================================================
# Algorithm to generate ALL DAGs (pdag2alldagsSlow):
#
# 0) Initialize an empty list of DAGs.
#
# 1) Complete current PDAG as far as possible using Rules R1-R4.
#
# 2) Select an unoriented edge X-Y.  
#
#    a) If none left:  
#       Done. Add DAG=abs(PDAG) to list of output DAGs.  Return.
#
#    b) Otherwise:       
#       Select an unoriented edge X-Y.
#       Create PDAG1 with X->Y.
#       Create PDAG2 with Y->X.
#       Recursion: For EACH PDAG (PDAG1/2):  Go to Step 1.
#
#
# This algorithm is a slight modification of the algorithm by Meek (1995)  
# which generates a single DAG extension of a PDAG - here we just add 
# recursion to consider both possible orientations for each considered edge.
#
# For the original algorithm by Meek, see 
#    C. Meek, "Causal inference and causal explanation with background 
#    knowledge", UAI 1995, Section 3.1.1, "Phase III" algorithm.
#
# Thanks to Daniel Eaton for extensive testing and bug reports.
#
# Imme Ebert-Uphoff (ebert@tree.com), 2007
# =======================================================================

  # MAIN
  dag_list <- list()  # init empty list of DAGs

  # Complete pdag as far as possible using Rules R1-R4 of Meek (1995)
  cpdag <- maximallyOrientEdgesSlow(pdag, verbose)
  
  # Start recursion
  dag_list <- recurse_unoriented_edgeSlow(cpdag, dag_list, verbose) 

  # return # of DAGs along with dag_list
  n_dags <- length(dag_list)
  if (n_dags == 0){  # no DAGs generated
    cat('PDAG does not have any permissible extension!\n')
  }
  else {
    dag_list
  }
}

##################################################################
# RECURSE_UNORIENTED_EDGE                                        #
#    implements Step 2 of the pdag2alldagsSlow algorithm.        #
##################################################################

#' Recurse unoriented edge (slow).
#'
#' This is the R implementation
#'
#' @param cpdag ...
#' @param dag_list ...
#' @param verbose ...
#' @return ...
#' @export
recurse_unoriented_edgeSlow <- function(cpdag, dag_list, verbose = F){
  
  # input must be a COMPLETE pdag
  # find all undirected edges
  undirected_edges <- which(cpdag == 1, arr.ind = T)
  
  updated_list <- dag_list

  if (nrow(undirected_edges) == 0){
    # if no undirected edges left
    # End of recursion reached.
    # Convert all (-1) values to (1) to yield standard DAG, add DAG to list.
    updated_list <- append(updated_list, list(abs(cpdag)))
  }
  else {
    # choose first unoriented edge
    # (any unoriented edge could be used here)
    a <- undirected_edges[1, 1]
    b <- undirected_edges[1, 2]
    
    # choose two different directions for edge and complete BOTH !      
    # PDAG1: contains a -> b
    pdag1 <- cpdag
    pdag1[a,b] <- -1
    pdag1[b,a] <- 0
    if (verbose){
      cat("PDAG1: ", a, " -> ", b, "\n")
    }
    # complete as far as possible using rules R1-R4:
    cpdag1 = maximallyOrientEdgesSlow(pdag1, verbose = verbose)
    # Continue recursion on another unoriented edge
    updated_list <- recurse_unoriented_edgeSlow(cpdag1, updated_list, verbose)

    # PDAG1: contains b -> a
    if (verbose){
      cat("PDAG2: ", b, " -> ", a, "\n")
    }
    pdag2 = cpdag
    pdag2[a,b] <- 0
    pdag2[b,a] <- -1
    # complete as far as possible using rules R1-R4:
    cpdag2 = maximallyOrientEdgesSlow(pdag2, verbose = verbose)
    # Continue recursion on another unoriented edge
    updated_list <- recurse_unoriented_edgeSlow(cpdag2, updated_list, verbose)
  }
  updated_list
}