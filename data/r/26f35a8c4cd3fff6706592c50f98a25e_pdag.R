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
maximallyOrientEdges <- function(pdag, ...){
  UseMethod("maximallyOrientEdges")
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
maximallyOrientEdges.parental <- function(pdag, verbose = F, ...){
  # convert to adjacency matrix
  pdag <- as.adjacency(pdag)
  # maximally orient
  out <- maximallyOrientEdges(pdag, verbose)
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

#' Undocumented.
#'
#' ...
#'
#' @param pdag ...
#' @param verbose ...
#' @param ... further arguments (unused)
#' @return ...
#' @S3method maximallyOrientEdges matrix
#' @method maximallyOrientEdges matrix
maximallyOrientEdges.matrix <- function(pdag, verbose = F, ...){
  stopifnot(class(pdag)     == "matrix",
            dim(pdag)[1]    == dim(pdag)[2],
            class(verbose)  == "logical",
            length(verbose) == 1)
  .Call("maximallyOrientEdges", pdag, verbose, PACKAGE = "parental")
}

#' Undocumented.
#' 
#' A generic
#' 
#' @param pdag A pdag
#' @param ... Further arguments passed to method
#' @export
pdag2alldags <- function(pdag, ...){
  UseMethod("pdag2alldags")
}

#' Undocumented.
#' 
#' ....
#'
#' @param pdag A pdag
#' @param verbose ...
#' @param ... Further arguments (unused)
#' @return ....
#' @S3method pdag2alldags parental
#' @method pdag2alldags parental
pdag2alldags.parental <- function(pdag, verbose = F, ...){
  pdag <- as.adjacency(pdag)
  out <- pdag2alldags(pdag, verbose = verbose)
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
#' @S3method pdag2alldags matrix
#' @method pdag2alldags matrix
pdag2alldags.matrix <- function(pdag, verbose = F, ...){
  stopifnot("matrix"        %in% class(pdag),
            dim(pdag)[1]    == dim(pdag)[2],
            class(verbose)  == "logical",
            length(verbose) == 1)
#
# [n_dags,dag_list] = pdag2alldags( pdag)
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
#      [n_dags,dag_list] = pdag2alldags(pdag)
#      n_dags  
#
# If you want to generate all DAGs that are Markov equivalent to an 
# input DAG (not a pattern), then use function Markov_equivalent_dags(dag)
# instead which calls this function.
#
# =======================================================================
# Algorithm to generate ALL DAGs (pdag2alldags):
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
  cpdag <- maximallyOrientEdges(pdag, verbose)
  
  # Start recursion
  dag_list <- recurse_unoriented_edge(cpdag, dag_list, verbose) 

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
#    implements Step 2 of the pdag2alldags algorithm.        #
##################################################################


#' Recurse unoriented edge.
#' 
#' ...
#'
#' @param cpdag A cpdag
#' @param dag_list ...
#' @param verbose A logical indicating whether verbose output should be 
#'   given.
#' @return Unknown
#' @useDynLib parental
#' @export
recurse_unoriented_edge <- function(cpdag, dag_list, verbose = F){
  stopifnot(class(cpdag)    == "matrix",
            dim(cpdag)[1]   == dim(cpdag)[2],
            class(verbose)  == "logical",
            length(verbose) == 1)
  .Call("recurse_unoriented_edge", cpdag, dag_list, verbose, PACKAGE = "parental")
}