# InferenceMultinom4CA
# Functions for Inferences for CA based on the multinomial distribution.
# To be used
# when the data are a real contingency table with large N.
# Bootstrap estimates are obtained by creating
# bootstrap contingency tables
# from a multinomial distribution.
# Permutation tests are obtained by creating
# contingency tables matching H0
# (i.e., multinomial with Pij = Pi*Pj)
# Permutation test will not work though
# because in MCA coded a 0/1 the columns are not
# independent (but come in blocks).
# -------------------------------------------------------------------
# Hervé Abdi
# Last Update: November 3, 2016. Revisited October 14, 2017.
#
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Functions below
# commented fir roxygen2


# -------------------------------------------------------------------
# function Boot4MultCA
#' \code{Boot4MultCA}: create a Bootstrap Cube for a CA
#' obtained from bootstraping the rows
#' the data-table.
#'
#' \code{Boot4MultCA}:
#' Create Bootstrap Cubes for the I and J sets
#' of a CA
#' obtained from bootstraping the entries/cells
#' of a real contingency table.
#' \code{Boot4MultCA} uses the multinomial distribution to generate the
#' Bootstrap samples (function \code{rmultinom})
#' \code{Boot4MultCA} uses the "transition formula" to get
#' the values of the column factors.
#' Gives also the bootstrap eigenvalues
#' (if \code{eigen = TRUE}).
#' Note: the \code{rmultinom()} function
#' cannon handle numbers of observations that are too high
#' (i.e., roughly larger than 10^9), so if the table total
#' is larger than 10^8, the table is recoded so that
#' its sum is roughly equal to 10^8.
#' Planned development: A compact version that gives only
#' bootstrap ratios (not the whole brick  \code{BootstrapBricks}).
#' @param X the data matrix
#' @param Fi = NULL, the I-set factor scores
#' (for the rows) from the analysis of X.
#' if NULL, \code{Boot4RowCA} will compute them.
#' @param Fj = NULL, the J-set factor scores
#' (for the columns) from the analysis of X.
#' if NULL, \code{Boot4RowCA} will compute them.
#' @param delta = NULL, the singular values
#' from the CA of X. If NULL (default),
#' \code{Boot4RowCA} will compute them.
#' @param nf2keep How many factors to
#' keep for the analysis (Default = 3).
#' @nIter (Default = 1000). Number of Iterations
#' (i.e. number of Bootstrtap samples).
#' @param critical.value (Default = 2).
#' The critical value for a BR to be considered
#' significant.
#' @param eig if TRUE compute bootstraped
#' confidence intervals (CIs) for the eigenvalues
#' (default is FALSE).
#' @param alphavalue the alpha level to compute
#' confidence intervals for the eigenvalues
#' (with CIS at 1-alpha). Default is .05
#' @return a list with \code{bootCube.i} of
#' Bootstraped factor scores (I-set)
#'  \code{bootRatios.i}: the bootstrap ratios;
#'  \code{bootRatiosSignificant.i}: the Significant
#'  BRs;
#'  a list with \code{bootCube.j}:
#' An Items*Dimension*Iteration Brick of
#' Bootstraped factor scores (J-set);
#'  \code{bootRatios.j}: the bootstrap ratios;
#'  \code{bootRatiosSignificant.j}: the Significant
#'  BRs;
#'  \code{eigenValues} the nIter * nL table
#'  of eigenvalues; \code{eigenCIs}: the CIs for the
#'  eigenvalues.
#' @author Herve Abdi
#' @import ExPosition
#' @export
Boot4MultCA <- function(X,
                        Fi = NULL,
                        Fj = NULL,
                        delta = NULL,
                        nf2keep = 3,
                        nIter = 1000,
                        critical.value = 2,
                        eig = FALSE,
                        alphaLevel = .05){
  # NB Internal function here for coherence
  .boot.ratio.test <- function(boot.cube, critical.value=2){
    boot.cube.mean <- apply(boot.cube, c(1,2),mean)
    boot.cube.mean_repeat <- array(boot.cube.mean, dim =c (dim(boot.cube)))
    boot.cube.dev <- (boot.cube - boot.cube.mean_repeat)^2
    s.boot<-(apply(boot.cube.dev,c(1,2),mean))^(1/2)
    boot.ratios <- boot.cube.mean / s.boot
    significant.boot.ratios <- (abs(boot.ratios) > critical.value)
    rownames(boot.ratios) <- rownames(boot.cube)
    rownames(significant.boot.ratios) <- rownames(boot.cube)
    return(list(sig.boot.ratios=significant.boot.ratios,boot.ratios=boot.ratios))
  }
  #
  # End of .boot.ratio.test
  X <- as.matrix(X)
  #
  # fix the size problem for multinomial
  ndigits <- round(log(sum(X),10))
  if (ndigits > 8 ) {X <- round(X/( 10^(ndigits - 8 ) )) }
  
  
  if (is.null(Fi) | is.null(Fj) | is.null(delta)){
    
    resCA <- ExPosition::epCA(X,graphs = FALSE)
    Fi    <- resCA$ExPosition.Data$fi
    Fj    <- resCA$ExPosition.Data$fj
    delta <- resCA$ExPosition.Data$pdq$Dv
  }
  
  #
  nL <- min(c(length(delta),ncol(Fi)))
  # make sure that dimensions fit
  if ( nf2keep > nL){nf2keep = nL}
  Fi = Fi[,1:nf2keep]
  Fj = Fj[,1:nf2keep]
  delta <- delta[1:nf2keep]
  #
  # Compute the BootstrapBrick
  #
  # Initialize
  nJ <-  NCOL(X)
  nN <-  sum(X)
  nI <-  NROW(X)
  # J-set
  fj.boot    <- array(NA, dim = c(nJ,nf2keep,nIter))
  # Name.
  dimnames(fj.boot)[1] <- list(colnames(X))
  dimnames(fj.boot)[2] <- list(paste0("Dimension ",1: nf2keep))
  dimnames(fj.boot)[3] <- list(paste0("Iteration ", 1:nIter))
  # I-set
  fi.boot    <- array(NA, dim = c(nI,nf2keep,nIter))
  # Name.
  dimnames(fi.boot)[1] <- list(rownames(X))
  dimnames(fi.boot)[2] <- list(paste0("Dimension ",1: nf2keep))
  dimnames(fi.boot)[3] <- list(paste0("Iteration ", 1:nIter))
  
  # Create the matrix of Column Profiles
  # C <- t(apply(X,2,function(x) x / sum(x)))
  # R <- t(apply(X,1,function(x) x / sum(x)))
  
  # Now Create Fj from Fi and delta
  MatExpansion.j <- matrix(1/delta, nJ,length(delta),
                           byrow = TRUE)
  MatExpansion.i <- matrix(1/delta, nI,length(delta),
                           byrow = TRUE)
  # Test the transition formula
  # test.fj <- (C %*%  fi) *  MatExpansion
  # A Bootstrapped version
  #
  # Loop
  if (eig){# Initialize eigenValues
    maxrank     <- min(NROW(X),NCOL(X)) - 1
    eigenValues <- matrix(0,nIter, maxrank)
  }
  for (ell in 1:nIter){# ell loop
    # BootIndex <-  sample(nI, replace = TRUE)
    # boot.fj
    #fj.boot[,,ell] <-  (C[,BootIndex] %*%
    #                      Fi[BootIndex,]) * MatExpansion
    # BootIndex <-  sample(nI, replace = TRUE)
    # boot.fj
    Xboot <- matrix(
      as.vector( rmultinom(1, nN,
                           X) ),
      nrow=nI,ncol=nJ,byrow = FALSE)
    # Column Profiles
    C <-  t(apply(Xboot,2,function(x) x / sum(x)))
    # row Profiles
    R <-  t(apply(Xboot,1,function(x) x / sum(x)))
    #
    fj.boot[,,ell] <-  (C  %*%
                          Fi) * MatExpansion.j
    #
    fi.boot[,,ell] <-  (R  %*%
                          Fj) * MatExpansion.i
    if (eig){
      # Xboot <- X[BootIndex,]
      # Check that there are no zero columns
      Xboot <- Xboot[,colSums(Xboot) > 0]
      eigenCA <- .eig4CA(Xboot)
      # Trick here for the rank of the eigenvalues
      index <- min(maxrank,length(eigenCA))
      eigenValues[ell,1:index] <-
        eigenCA[1:index ]
    }
  }# end of ell loop
  # Boot-ratios
  BR.j <- .boot.ratio.test(fj.boot,critical.value)
  BR.i <- .boot.ratio.test(fi.boot,critical.value)
  #
  return.list <- structure(
    list(
      bootstrapBrick.i =     fi.boot,
      bootRatios.i =  BR.i$boot.ratios,
      bootRatiosSignificant.i =
        BR.i$sig.boot.ratios,
      bootstrapBrick.j =     fj.boot,
      bootRatios.j =  BR.j$boot.ratios,
      bootRatiosSignificant.j =
        BR.j$sig.boot.ratios
    ),
    class = "bootBrick.ij")
  if (eig){
    # eliminate empty eigenvalues
    eigenValues <- eigenValues[, colSums(eigenValues) > 0]
    return.list$eigenValues = eigenValues
    # Get the CI
    # order the eigenvalues to get the CIs
    sortedEigenValues <- apply(eigenValues,2,sort)
    index  =  round(nIter * (alphaLevel /2))
    if (index == 0) index <- 1
    eigenCI = sortedEigenValues[c(index,nIter-(index-1)),]
    return.list$eigenCI <- eigenCI
  } # end if eigen
  return(return.list)
}
# End of function Boot4RowCA
# *******************************************************************************
#' Change the print function for class bootBrick.ij
#'
#'  Change the print function for bootBrick.ij
#'  (output of Boot4MultCA)
#'
#' @param x a list: output of Boot4MultCA
#' @param ... everything else for the function
#' @author Herve Abdi
#' @export
print.bootBrick.ij <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Bootstraped Factor Scores (BFS) and Bootstrap Ratios  (BR) \n")
  cat(" for the I and J-sets of a CA (obtained from multinomial resampling of X) \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ bootstrapBrick.i        ", "an I*L*nIter Brick of BFSs  for the I-Set")
  cat("\n$ bootRatios.i            ", "an I*L matrix of BRs for the I-Set")
  cat("\n$ bootRatiosSignificant.i ", "an I*L logical matrix for significance of the I-Set")
  cat("\n$ bootstrapBrick.j        ", "a  J*L*nIter Brick of BFSs  for the J-Set")
  cat("\n$ bootRatios.j            ", "a  J*L matrix of BRs for the J-Set")
  cat("\n$ bootRatiosSignificant.j ", "a  J*L logical matrix for significance of the J-Set")
  cat("\n$ eigenValues             ", "a  nIter*L matrix of the bootstraped CA eigenvalues")
  cat("\n$ eigenCI                 ", "a  2*L with min and max CI for the eigenvalues")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.bootBrick.ij
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Permutation test for CA
# To be used with matrices analyzed by CA
# based on the multinomial distribution.
# Idea from Greenacre 2016.
# November 1st, 2016
# Hervé Abdi
#********************************************************************
# \code{Perm4MultCA}
#'Permutation test for CA
#'on real contingency tables
#'obtained from a multinormal distribution.
#'
#' \code{Perm4MultCA} computes a permutation test
#' for CA when CA is performed
#' on a real contingency table.
#' The resampling is obtained
#' rom a multinormal distribution.
#' \code{Perm4MultCA}
#' can be used for big tables
#' for the omnibus test (i.e., inertia) and for
#' the eigenvalues.
#' @param X the data matrix (non-negative numbers)
#' @compact if TRUE return only p-values for omnibus test
#' default is FALSE.
#' @nIter (Default = 1000). Number of Iterations
#' (i.e. number of permuted samples computed).
#' @return a list with
#' \code{fixedInertia}: the CA-inertia of the data matrix;
#' \code{fixedEigenvalues}: the CA-eigenvalues of
#' the data matrix;
#' \code{pOmnibus}: the probability associated
#' to the inertia.
#' If \code{compact} is \code{FALSE}, return also
#' \code{permInertia}:
#' an \code{nIter} * 1 vector containing the
#' permutated inertia;
#' \code{pEigenvalues}: The probabilites
#' associated to each eigenvalue;
#' If \code{compact} is is \code{FALSE}, return also
#' \code{permEigenvalues}: an
#' \code{nIter} * \code{L} matrix giving
#' the permuted eigenvalues.
#' @author Herve Abdi
#' @export

Perm4MultCA <- function(X,
                        nIter = 1000,
                        compact = FALSE){
  nI <- NROW(X)
  nJ <- NCOL(X)
  # End of .boot.ratio.test
  X <- as.matrix(X)
  #
  # fix the size problem for multinomial
  ndigits <- round(log(sum(X),10))
  if (ndigits > 8 ) {X <- round(X/( 10^(ndigits - 8 ) )) }
  #
  maxRank <- min(nI,nJ) - 1
  if (maxRank == 0){stop('X should be a matrix of rank > 1')}
  # First compute the Fixed Effect Inertia
  fixedEigenvalues <- rep(0,maxRank)
  fixedEV <- .eig4CA(X)
  # Make sure that the length fit
  if (length(fixedEV) > maxRank){
    fixedEigenvalues <- fixedEV[1:maxRank]
  }
  if (length(fixedEV) == maxRank){fixedEigenvalues <- fixedEV}
  if (length(fixedEV) < maxRank){
    fixedEigenvalues[1:length(fixedEV)] <- fixedEV
  }
  
  fixedInertia <- sum(fixedEigenvalues)
  #
  # Initialize
  permInertia     <- rep(NA,nIter)
  permEigenvalues <- matrix(NA, nrow = nIter, ncol = maxRank)
  #
  # Go for the loop
  # Use replicate instead
  # first define the function
  # First X under the null
  nN = sum(X)
  X4H0 <- (1/sum(X)^2) *
    (as.matrix(rowSums(X))%*%t(as.matrix(colSums(X))))
  # Alocal function for replicate
  .truc <- function(X,longueur = min(dim(X)),nN){
    valP   <- rep(0, longueur)
    #resvp <- .eig4CA( apply(X,2,sample ))
    #X4test <- matrix(rmultinom(1, nN, X),
    #                 nrow = nI, ncol = nJ, byrow = FALSE)
    resvp <-  .eig4CA.robust(
      matrix(rmultinom(1, nN, X),
             nrow = nI, ncol = nJ, byrow = FALSE)
      #matrix(
      #as.vector(X4test),
      #nrow = nI, ncol = nJ,byrow = FALSE)
    )
    valP[1:length(resvp)] <- resvp
    return(valP)
  }
  laLongueur <- maxRank + 1 # to fix rounding error for ev
  permEigenvalues <- replicate(nIter,
                               .truc(X4H0,laLongueur,nN) )
  
  # print(c('maxRank =',maxRank))
  #  print(head(permEigenvalues))
  #  print(c('Dimension permEigenValues ',
  #         nrow(permEigenvalues),' - ',ncol(permEigenvalues) ) )
  
  permEigenvalues <- t(permEigenvalues[1:maxRank,])
  # Done without a loop!
  permInertia = rowSums(permEigenvalues)
  #
  pOmnibus = sum(permInertia > fixedInertia) / nIter
  if (pOmnibus == 0) pOmnibus <- 1/nIter # no 0
  pEigenvalues <- rowSums( t(permEigenvalues) >
                             (fixedEigenvalues)) / nIter
  pEigenvalues[pEigenvalues == 0 ] <- 1/nIter
  return.list <- structure(
    list(fixedInertia = fixedInertia,
         fixedEigenvalues = fixedEigenvalues,
         pOmnibus = pOmnibus,
         pEigenvalues = pEigenvalues
    ),
    class = 'perm4CA')
  if (!compact){
    return.list$permInertia =  permInertia
    return.list$permEigenvalues = permEigenvalues
  }
  return(return.list)
} # End of perm4CA
#--------------------------------------------------------------------

# *******************************************************************************
#' Change the print function for perm4CA class
#'
#'  Change the print function for perm4CA class
#'  objects
#'  (output of Perm4RowCA)
#'
#' @param x a list: output of perm4RowCA
#' @param ... everything else for the functions
#' @author Herve Abdi
#' @export
print.perm4CA <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Results of Permutation Tests for CA of Matrix X \n")
  cat(" for Omnibus Inertia and Eigenvalues \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ fixedInertia     ", "the Inertia of Matrix X")
  cat("\n$ fixedEigenvalues ", "a L*1 vector of the eigenvalues of X")
  cat("\n$ pOmnibus         ",  "the probablity associated to the Inertia")
  cat("\n$ pEigenvalues     ", "an L* 1 matrix of p for the eigenvalues of X")
  cat("\n$ permInertia      ", "vector of the permuted Inertia of X")
  cat("\n$ permEigenvalues  ", "matrix of the permuted eigenvalues of X")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.perm4CA
#---------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
# function .eig4CA. below
#
# function to give the eigenvalues of CA matrix
#
#'  computes the eigenvalues for
#' correspondence analysis (CA)
#'
#' A function to compute the eigenvalues for
#' correspondence analysis (CA). Needs a matrix
#' of non-negative numbers. The trivial eigenvalue
#' equals to 1, is not computed.
#' NB for computational efficiency,
#'  the matrix diagonalized is NOT the
#' matrix diagonalized in CA but it has the
#' same eigenvalues.
#' @param X a matrix of non-negative numbers
#' @author Herve Abdi
#' @export
.eig4CA <- function(X){# give the eigenvalues of a CA matrix
  # NB the matrix diagonalized is NOT the
  # matrix diagonalized in CA but it has the
  # same eigenvalues.
  # Author Herve Abdi. October 31, 2016.
  
  # first make sure that we always
  # diagonalize the smaller matrix
  if (dim(X)[1] > dim(X)[2]){ X <- t(X)}
  
  P  <- X / sum(X)
  # Get Masses etc.
  r = rowSums(P)
  c = colSums(P)
  nI <- NROW(P)
  nJ <- NCOL(P)
  Pcent <- P - as.matrix(r)%*%c
  M12 <- matrix(1/sqrt(r), nrow = nI, ncol = nJ  )
  W12 <- matrix(1/sqrt(c), nrow = nI, ncol = nJ, byrow = TRUE)
  Y <- as.matrix(M12 * Pcent * W12)
  #S = Y %*% t(Y)
  eig4CA <- eigen( Y %*% t(Y),
                   symmetric = TRUE,
                   only.values = TRUE)$values
  # Make sure that only positive eigenvalues are kept
  eig4CA <- eig4CA[eig4CA >= 0]
  return(eig4CA)
} # End .eig4CA
#----------------------------------------------------------------------------------------
# End of function .eig4CA
#----------------------------------------------------------------------------------------

# NewMalivaudTest4CA
# a compact version of Malinvaud-Saparta Test.
# Hervé Abdi. November 6, 2016.
#-------------------------------------------------------------------------------
# MalinvaudQ4CA

#' computes the Malinvaud-Saporta test for
#' signficance in correspondence analysis
#'                   with asymptotically
#'                   (and, if provided permutation)
#'                    derived p-values
#'
#'
#' Compute the Malinvaud-Saporta test for significance
#' in Correspondence Analysis. Provides asymptotic
#' Chis-square based p-values, and if a set
#' of permuted eigenvalues is provided
#' (obtained, e.g., from \code{Perm4MultCA})
#' provides _p_-values based on permutation tests.
#'References:
#' Malinvaud, E. (1987). Data Analysis in applied socio-economic statistics
#' with special considerations of correspondence analysis.
#' Marketing Science Conference Proceedings, HEC-ISA, Jouy-en-Josas.
#' Also cited in Saporta (2011).
#' Probabilité et Analyse des Données (3rd Ed).
#'    Technip, Paris. p. 209.
#'@param Data a matrix or dataframe with non-negative elements
#'@param permutedEigenValues (default = NULL) a matrix
#' with the eigenvalues obtained from a permutation test
#' @param ndigit4print (Default = 4) number of digits
#' to sue to print the results
#' @author Herve Abdi
#' @return: A dataframe with the results of the test
#' @export

MalinvaudQ4CA  <-   function(Data, # The original Data Table
                             permutedEigenValues = NULL,
                             # the permuted eigenvalues
                             # as a niteration * rank of X matrix
                             ndigit4print = 4
                             # how many digits for printing
){# Function begins here
  # References:
  # Malinvaud, E. (1987). Data Analysis in applied socio-economic statistics
  # with special considerations of correspondence analysis.
  # Marketing Science Conference Proceedings, HEC-ISA, Jouy-en-Josas.
  # Also cited in Saporta (2011).
  # Probabilité et Analyse des Données (3rd Ed).
  #    Technip, Paris. p. 209.
  #   Val.P <- ResFromExposition$ExPosition.Data$eigs
  
  # First an internal function to compute the eigenvalues
  # function to give the eigenvalues of CA matrix
  #'  computes the eigenvalues for
  #' correspondence analysis (CA)
  #'
  #' A function to compute the eigenvalues for
  #' correspondence analysis (CA). Needs a matrix
  #' of non-negative numbers. The trivial eigenvalue
  #' equals to 1, is not computed.
  #' NB for computational efficiency,
  #'  the matrix diagonalized is NOT the
  #' matrix diagonalized in CA but it has the
  #' same eigenvalues.
  #' @param X a matrix of non-negative numbers
  #' @author Herve Abdi
  #' @export
  N.pp  <- sum(Data) # Grand Total of the data table
  #if(is.null(Val.P)){#Val.P = .compactCA(Data,eig.only = TRUE)
  Val.P = .eig4CA.robust(Data)
  #}
  # Compute eigenvalues
  nL <- length(Val.P) # how many eigenvalues
  if (!is.null(permutedEigenValues)){
    # in case of conflict between perm dimension and Val.P
    nL.perm <- NCOL(permutedEigenValues)
    if (nL.perm < nL){ # patch with 0
      nIter <- NROW(permutedEigenValues)
      permutedEigenValues.tmp <- matrix(0,
                                        nIter,
                                        nL)
      permutedEigenValues.tmp[,1:nL.perm] <-
        permutedEigenValues
      permutedEigenValues <- permutedEigenValues.tmp
    }} # end of test on nL
  nI <- nrow(Data)
  nJ <- ncol(Data)
  Q     <- N.pp * cumsum(Val.P[nL:1])[nL:1]
  Q.nu  <- (nI - 1:nL)*(nJ - 1:nL)
  # Get the values from Chi Square
  pQ = 1 - pchisq(Q,Q.nu)
  # Add NA to make clear that the last
  #     dimension is not tested
  Le.Q = c(Q,NA)
  Le.pQ = c(pQ,NA)
  Le.Q.nu = c(Q.nu,0)
  #
  NamesOf.Q = c('Malinvaud-Saporta Test. Ho: Omnibus', paste0('Dim-',seq(1:nL)))
  names(Le.Q)    <- NamesOf.Q
  names(Le.pQ)   <- NamesOf.Q
  names(Le.Q.nu) <- NamesOf.Q
  #
  # if we have the permuted eigenvalues
  # use them to derive the p-values
  if (!is.null(permutedEigenValues)){# If LesEigPerm exist get p-values
    Q.perm     <-  N.pp*t(apply(permutedEigenValues[,nL:1],1,cumsum) )[,nL:1]
    #
    Logical.Q.perm =  t(t(Q.perm) >  (Q))
    pQ.perm = colMeans(Logical.Q.perm)
    pQ.perm[pQ.perm == 0] = 1 / nrow(Q.perm)
    Le.pQ.perm = c(pQ.perm,NA)
    # return the table
    Malinvaud.Q = data.frame(matrix(c(
      c(round(c(sum(Val.P),Val.P),digits=ndigit4print) ),
      round(Le.Q,      digits=ndigit4print),
      round(Le.pQ,     digits=ndigit4print),
      round(Le.Q.nu,   digits=ndigit4print),
      round(Le.pQ.perm,digits=ndigit4print)),
      nrow = 5, byrow=TRUE))
    colnames(Malinvaud.Q) = NamesOf.Q
    rownames(Malinvaud.Q) = c('Inertia / sum lambda',
                              'Chi2',
                              'p-Chi2','df',
                              'p-perm')
  } else {
    Malinvaud.Q = data.frame(matrix(c(
      c(round(c(sum(Val.P),Val.P),digits=ndigit4print) ),
      round(Le.Q,      digits=ndigit4print),
      round(Le.pQ,     digits=ndigit4print),
      round(Le.Q.nu,   digits=ndigit4print)
      # ,round(Le.pQ.perm,digit=ndigit4print)
    ),
    nrow = 4, byrow=TRUE))
    colnames(Malinvaud.Q) = NamesOf.Q
    rownames(Malinvaud.Q) = c('Inertia / sum lambda',
                              'Chi2',
                              'p-Chi2','df'
                              #,'p-perm'
    )
  }
  return(Malinvaud.Q)
} # End of function MalinvaudQ4CA here
#


#=====================================================================
# function eig4CA.robust
# a version of eig4CA that can cope with a division by 0
#
#=====================================================================

#----------------------------------------------------------------------------------------
# function .eig4CA. below
#
# function to give the eigenvalues of CA matrix
#
#'  computes the eigenvalues a matrix of non-negative
#'  elements for
#' correspondence analysis (CA).
#'
#' A function to compute the eigenvalues for
#' correspondence analysis (CA). Needs a matrix
#' of non-negative numbers. The trivial eigenvalue
#' equals to 1, is not computed.
#' NB for computational efficiency,
#'  the matrix diagonalized is NOT the
#' matrix diagonalized in CA but it has the
#' same eigenvalues.
#' NB rows and columns with 0 sums have an inverse et to 0.
#' @param X a matrix of non-negative numbers
#' @author Herve Abdi
#' @export
.eig4CA.robust <- function(X){# give the eigenvalues of a CA matrix
  # NB the matrix diagonalized is NOT the
  # matrix diagonalized in CA but it has the
  # same eigenvalues.
  # Author Herve Abdi. October 31, 2016.
  
  # first make sure that we always
  # diagonalize the smaller matrix
  if (dim(X)[1] > dim(X)[2]){ X <- t(X)}
  
  P  <- X / sum(X)
  # Get Masses etc.
  r = rowSums(P)
  c = colSums(P)
  #nI <- NROW(P)
  #nJ <- NCOL(P)
  Pcent <- P - as.matrix(r)%*%c
  r_12 <- 1/sqrt(r)
  r_12[r==0] <- 0 # fix problem with divide by 0
  c_12 <- 1/sqrt(c)
  c_12[c==0] <- 0 # fix problem with divide by 0
  # Two ways of avoiding multiplication by diag matrices
  # 1. a la repmat ...
  #M12 <- matrix(r_12, nrow = nI, ncol = nJ  )
  #W12 <- matrix(c_12, nrow = nI, ncol = nJ, byrow = TRUE)
  #Y <- as.matrix(M12 * Pcent * W12)
  # 2. Equivalent (marginaly faster)
  #
  Y <- t(t( r_12 * Pcent) * c_12)
  #S = Y %*% t(Y)
  eig4CA <- eigen( Y %*% t(Y),
                   symmetric = TRUE,
                   only.values = TRUE)$values
  # Make sure that only positive eigenvalues are kept
  eig4CA <- eig4CA[eig4CA >= 0]
  return(eig4CA)
} # End .eig4CA.robust
#----------------------------------------------------------------------------------------
# End of function .eig4CA.robust
#----------------------------------------------------------------------------------------
