#
# create inference battery for PLSC
# 1. Permutation
# 2. Bootstrap



#--------------------------------------------------------------------
#' Compute a SCP matrix with several possible
#' combination of centering and normalizing.
#'
#' Compute a SCP matrix from 2 matrices X and Y
#'  with several possible
#' combinations of centering and normalizing.
#' Both X and Y are pre-processed
#' (i.e., centered / normalized)
#' Used for functions related to PLSC 
#' inter-battery analysis / co-inertia...
#' Allows different types of normalization
#' based on the \code{ExPosition} function
#' \code{expo.scale}.
#' @param DATA1 an N*I matrix of quantitative data
#' @param DATA2 an N*J matrix of quantitative data
#' @param center1 when TRUE (default) \code{DATA1}
#' will be centered
#' @param center2 when TRUE (default) \code{DATA2}
#' will be centered
#' @param scale1 when TRUE (default) \code{DATA1}
#' will be normalized. Depends upon \code{ExPosition}
#' function \code{expo.scale} whose description is:
#'boolean, text, or (numeric) vector. 
#'If boolean or vector, 
#'it works just as scale. 
#'The following text options are available:
#' 'z': z-score normalization,
#' 'sd': standard deviation normalization, 
#' 'rms': root mean square normalization,
#'  'ss1': sum of squares
#'  (of columns) equals 1 
#'  (i.e., column vector of length of 1).
#' @param scale2 when TRUE (default) \code{DATA2}
#' will be normalized
#'  (same options as for \code{scale1}).
#' @permType what type of permutation is used
#' if 'byMat' (default) only the labels of the observations
#' are permutated, other option is 'byColumns' then
#' all columns of each matrix are independently 
#' permuted.
#' @return S the cross-product matrix from  X and Y.
#' @import ExPosition
#' @export
#' 
compS <- function(DATA1,
                  DATA2,
                  center1 = TRUE,
                  center2 = TRUE,
                  scale1 =  'ss1' , #   'ss1' ,
                  scale2 =  'ss1' 
                   ){
   X <- DATA1
   Y <- DATA2
   if (center1 & center2 
            & (scale1 == 'ss1') 
            & (scale2 == 'ss1') ){
                  S = cor(X,Y) } else {
              Xc <- ExPosition::expo.scale(X, center = center1,
                                               scale = scale1)
              Yc <- ExPosition::expo.scale(Y, center = center2,
                                               scale = scale2)
              S <- t(Xc) %*% Yc
                  }

       return(S)               
} # end of function compS
#--------------------------------------------------------------------
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#' Permutation for PLSC (as implemented
#' in \code{TExPosition::tepPLS})
#' 
#' Permutation for PLSC (as implemented
#' in \code{TExPosition::tepPLS}).
#' Compute an omnibus permutation test  and
#' specific test for the eigenvalues when
#' performing a PLSC from 
#' 2 matrices X and Y.
#'  Several possible
#' combinations of centering and normalizing
#' are possible (see paramater \code{scale1, 
#' scale2, center2, scale2}).
#' Used for functions related to PLSC 
#' inter-battery analysis / co-inertia...
#' The different types of normalization are
#' based on the \code{ExPosition::expo.scale} 
#' function. Two different permutation schemes
#' are currently available (see paramater
#' \code{permType}).
#' @param DATA1 an N*I matrix of quantitative data
#' @param DATA2 an N*J matrix of quantitative data
#' @param center1 when TRUE (default) \code{DATA1}
#' will be centered
#' @param center2 when TRUE (default) \code{DATA2}
#' will be centered
#' @param scale1 when TRUE (default) \code{DATA1}
#' will be normalized. Depends upon \code{ExPosition}
#' function \code{expo.scale} whose description is:
#'boolean, text, or (numeric) vector. 
#'If boolean or vector, 
#'it works just as scale. 
#'The following text options are available:
#' 'z': z-score normalization,
#' 'sd': standard deviation normalization, 
#' 'rms': root mean square normalization,
#'  'ss1': sum of squares
#'  (of columns) equals 1 
#'  (i.e., column vector of length of 1).
#' @param scale2 when TRUE (default) \code{DATA2}
#' will be normalized
#'  (same options as for \code{scale1}).
#' @param nIter (Default = 1000). Number of Iterations 
#' (i.e. number of permuted samples computed).
#' @permType what type of permutation is used
#' if 'byMat' (default) only the labels of the observations
#' are permutated, other option is 'byColumns' then
#' all columns of each matrix are independently 
#' permuted.
#' @param compact if TRUE return
#' (Default) only p-values for omnibus test
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

perm4PLSC <- function(DATA1,
                  DATA2,
                  center1 = TRUE,
                  center2 = TRUE,
                  scale1 =  'ss1' , #   'ss1' ,
                  scale2 =  'ss1',
                  nIter = 1000,
                  permType = 'byMat' , # 'byColumns
                  compact = FALSE
){
  if (permType != 'byColumns') permType <- 'byMat'
  DATA1 <- as.matrix(DATA1)
  DATA2 <- as.matrix(DATA2)
  X = DATA1
  Y = DATA2
  if (NCOL(X) > NCOL(Y)){
      X = DATA2
      Y = DATA1
      }
  
  nN <- NROW(X)
  nI <- NCOL(X)
  nJ <- NCOL(Y)
  if( !(nN == NROW(Y))){stop('DATA1 and DATA2 non-conformable')}
  maxRank <- min(nI,nJ)
  # Compute fixed SCP matrix for X & Y
  Sfixed = compS(DATA1,
            DATA2,
            center1 = center1,
            center2 = center2,
            scale1 =  scale1, #   'ss1' ,
            scale2 =  scale2)
  fixedEigenvalues <- rep(0,maxRank)
  fixedEV <- eigen(t(Sfixed) %*% (Sfixed), 
                   symmetric = TRUE, 
                   only.values = TRUE)$values
  # Make sure that the length fit
  if (length(fixedEV) > maxRank){
    fixedEigenvalues <- fixedEV[1:maxRank] 
  }
  if (length(fixedEV) == maxRank){fixedEigenvalues <- fixedEV}
  if (length(fixedEV) < maxRank){
    fixedEigenvalues[1:length(fixedEV)] <- fixedEV 
  }
  fixedInertia <- sum(fixedEigenvalues)
  # The random permutations below
  # Initialize
  permInertia     <- rep(NA,nIter)
  permEigenvalues <- matrix(NA, nrow = nIter, ncol = maxRank)
  #
  # Use replicate
  # first define the function

  .truc <- function(X,Y,
                    longueur = min(c(dim(X),NCOL(Y))),
                    permType = permType){
     valP   <- rep(0, longueur)
     #resvp <- .eig4CA( apply(X,2,sample ))
     if ( permType == 'byMat'){
       Xrand <- X[sample(nN),]
       Yrand <- Y
     }
     if ( permType == 'byColumns'){
       Xrand <- apply(X,2,sample )
       Yrand <- apply(Y,2,sample )
     }
     Srand <- compS(Xrand,Yrand)
     resvp <-   eigen(t(Srand) %*% Srand, 
                     symmetric = TRUE, 
                     only.values = TRUE)$values
    valP[1:length(resvp)] <- resvp
    return(valP)
          }
  laLongueur <- maxRank + 1 # to fix rounding error for ev
  permEigenvalues <- replicate(nIter, 
                               .truc(X,Y,laLongueur,permType) )
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
    class = 'perm4PLSC')
  if (!compact){
    return.list$permInertia =  permInertia
    return.list$permEigenvalues = permEigenvalues
  }
  return(return.list)
} # End of function perm4PLSC  

# *******************************************************************************
#' Change the print function for perm4PLSC class
#' 
#'  Change the print function for perm4PLSC class
#'  objects
#'  (output of Perm4PLSC)
#'  
#' @param x a list: output of perm4RowCA 
#' @param ... everything else for the functions
#' @author Herve Abdi
#' @export
print.perm4PLSC <- function (x, ...) { 
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
} # end of function print.perm4PLSC
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Bootstrap here
#

# -------------------------------------------------------------------
# function Boot4PLSC
#' Create a Bootstrap Cube for PLSC
#' 
#' Create Bootstrap Cubes for the I and J sets 
#' of a CA
#' obtained from bootstraping the rows
#' of the two data-tables used for PLSC. 
#' Uses the "transition formula" to get
#' the values of the rows and columns loadings
#' from multiplication of the latnet variables.
#' Gives also the bootstrap eigenvalues
#' (if \code{eigen = TRUE}).
#' Note: the \code{rmultinom()} function
#' cannon handle numbers of observation that are too high
#' (i.e., roughly larger than 10^9), so if the table total
#' is larger than 10^8, the table is recoded so that
#' its sum is roughly 10^8. 
#' Planned development: A compact version that gives only
#' bootstrap ratios (not BootstrapBricks)
#' @param DATA1 an N*I  data matrix
#' @param DATA2 an N*J  data matrix 
#' (measured on the same observations as DATA2)
#' @param center1 when TRUE (default) \code{DATA1}
#' will be centered
#' @param center2 when TRUE (default) \code{DATA2}
#' will be centered
#' @param scale1 when TRUE (default) \code{DATA1}
#' will be normalized. Depends upon \code{ExPosition}
#' function \code{expo.scale} whose description is:
#'boolean, text, or (numeric) vector. 
#'If boolean or vector, 
#'it works just as scale. 
#'The following text options are available:
#' 'z': z-score normalization,
#' 'sd': standard deviation normalization, 
#' 'rms': root mean square normalization,
#'  'ss1': sum of squares
#'  (of columns) equals 1 
#'  (i.e., each column vector has length of 1).
#' @param scale2 when TRUE (default) \code{DATA2}
#' will be normalized
#'  (same options as for \code{scale1}).
#' @param Fi = NULL, the I factor scores
#' for the columns of DATA1.
#' if NULL, the function computes them.
#' @param Fj = NULL, the J factor scores
#' for the columns of DATA2.
#' if NULL the function computes them.
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
#' (default is FALSE). Not Currently implemented
#' @param alphavalue the alpha level to compute
#' confidence interval for the eigenvalues
#' (with CIS at 1-alpha). Default is .05 
#' @return a list with \code{bootCube.i of
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
#' 
Boot4PLSC <- function(DATA1, DATA2,
                      center1 = TRUE,
                      center2 = TRUE,
                      scale1 = 'ss1',
                      scale2 = 'ss1',
                        Fi = NULL,
                        Fj = NULL,
                        nf2keep = 3,
                        nIter = 1000,
                        critical.value = 2,
                        eig = FALSE, 
                        # To be implemented later
                        # has no effect currently
                        alphaLevel = .05){
  # NB Internal function here for coherence
  .boot.ratio.test <- function(boot.cube,
                               critical.value=2){
    boot.cube.mean <- apply(boot.cube,c(1,2),mean)
    boot.cube.mean_repeat <- array(boot.cube.mean,
                                dim=c(dim(boot.cube)))
    boot.cube.dev <- (boot.cube - boot.cube.mean_repeat)^2
    s.boot<-(apply(boot.cube.dev,c(1,2),mean))^(1/2)
    boot.ratios <- boot.cube.mean / s.boot
    significant.boot.ratios <- (abs(boot.ratios) > critical.value)
    rownames(boot.ratios) <- rownames(boot.cube)
    rownames(significant.boot.ratios) <- rownames(boot.cube)
    return(list(sig.boot.ratios=significant.boot.ratios,
                boot.ratios=boot.ratios))
  }
  # 
  # End of .boot.ratio.test
  X <- ExPosition::expo.scale(DATA1, center = center1,
                               scale = scale1)
  Y <- ExPosition::expo.scale(DATA2, center = center2,
                               scale = scale2)
  nN = NROW(X)
  if (nN != NROW(Y)){stop('input matrices not conformable')}
  nI= NCOL(X)
  nJ = NCOL(Y)
  maxRank <- min(nI,nJ)
  if (maxRank < nf2keep) nf2keep = maxRank
  if  ( is.null(Fi) | is.null(Fj) ){
  # compute Fi and Fj  
    S <- t(X) %*% Y 
    svd.S <- svd(S, nu = nf2keep, nv = nf2keep)
    if (nf2keep > length(svd.S$d)) nf2keep = length(svd.S$d)
    Lx <- X %*% svd.S$u
    Ly <- Y %*% svd.S$v
    Fi <- svd.S$u * matrix(svd.S$d,nI,nf2keep,byrow = TRUE)
    Fj <- svd.S$v * matrix(svd.S$d,nJ,nf2keep,byrow = TRUE)
  } else { # Compute lx and ly from Fi and Fj
    nL = min(NCOL(Fi),NCOL(Fj))
    if (nL < nf2keep) nf2keep = nL
    Fi = Fi[,1:nf2keep]
    Fj = Fj[,1:nf2keep]
    delta.inv <- 1 / sqrt(colSums(Fi^2))
    Lx <-  X %*% (Fi * matrix(delta.inv,nI,nf2keep,byrow = TRUE) ) 
    Ly <-  Y %*% (Fj * matrix(delta.inv,nJ,nf2keep,byrow = TRUE) ) 
  }
  # Now we have Lx Ly Fi and Fj
  #
  # J-set
  fj.boot    <- array(NA, dim = c(nJ,nf2keep,nIter)) 
  # Name.
  dimnames(fj.boot)[1] <- list(colnames(Y))
  dimnames(fj.boot)[2] <- list(paste0("Dimension ",1: nf2keep))
  dimnames(fj.boot)[3] <- list(paste0("Iteration ", 1:nIter))
  # I-set
  fi.boot    <- array(NA, dim = c(nI,nf2keep,nIter)) 
  # Name.
  dimnames(fi.boot)[1] <- list(colnames(X))
  dimnames(fi.boot)[2] <- list(paste0("Dimension ",1: nf2keep))
  dimnames(fi.boot)[3] <- list(paste0("Iteration ", 1:nIter))
  for (ell in 1:nIter){# ell loop
   boot.index <- sample(nN, replace = TRUE)
   fi.boot[,,ell] <- t(X[boot.index,]) %*% Ly[boot.index,] 
   fj.boot[,,ell] <- t(Y[boot.index,]) %*% Lx[boot.index,] 
   ## Code Below taken from BOOTCA. To be used 
   ## to implement the eig option later
   # if (eig){
   #   # Xboot <- X[BootIndex,]
   #   # Check that there are no zero columns
   #   Xboot <- Xboot[,colSums(Xboot) > 0]
   #   eigenCA <- .eig4CA(Xboot) 
   #   # Trick here for the rank of the eigenvalues
   #   index <- min(maxrank,length(eigenCA))
   #   eigenValues[ell,1:index] <- 
   #     eigenCA[1:index ]
   # }
  }
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
    class = "bootBrick.ij4plsc")
## Code Below taken from BOOTCA. To be used 
## to implement the eig option later
# if (eig){
#   # eliminate empty eigenvalues
#   eigenValues <- eigenValues[, colSums(eigenValues) > 0]
#   return.list$eigenValues = eigenValues
#   # Get the CI
#   # order the eigenvalues to get the CIs
#   sortedEigenValues <- apply(eigenValues,2,sort)
#   index  =  round(nIter * (alphaLevel /2))
#   if (index == 0) index <- 1
#   eigenCI = sortedEigenValues[c(index,nIter-(index-1)),]
#   return.list$eigenCI <- eigenCI
# } # end if eigen
  return(return.list)
} # End of Function 

# *******************************************************************************
#' Change the print function for class bootBrick.ij4plsc 
#' 
#'  Change the print function for bootBrick.ij4plsc
#'  (output of Boot4MultCA)
#'  
#' @param x a list: output of Boot4PLSC
#' @param ... everything else for the function
#' @author Herve Abdi
#' @export
print.bootBrick.ij4plsc <- function (x, ...) { 
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Bootstraped Factor Scores (BFS) and Bootstrap Ratios  (BR) \n")
  cat(" for the I and J-sets of a CA (obtained from multinomial resampling of X) \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ bootstrapBrick.i         ", "an I*L*nIter Brick of BFSs  for the I-Set")
  cat("\n$ bootRatios.i             ", "an I*L matrix of BRs for the I-Set")
  cat("\n$ bootRatiosSignificant.i  ", "an I*L logical matrix for significance of the I-Set")
  cat("\n$ bootstrapBrick.j         ", "a  J*L*nIter Brick of BFSs  for the J-Set")
  cat("\n$ bootRatios.j             ", "a  J*L matrix of BRs for the J-Set")
  cat("\n$ bootRatiosSignificant.j  ", "a  J*L logical matrix for significance of the J-Set")
 #  cat("\n$ eigenValues          ", "a  nIter*L matrix of the bootstraped CA eigenvalues")
 #  cat("\n$ eigenCI              ", "a  2*L with min and max CI for the eigenvalues")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.bootBrick.ij 
#--------------------------------------------------------------------

