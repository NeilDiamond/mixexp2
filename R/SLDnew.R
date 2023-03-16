
# -------------------------------------------------------------------------

#' Create Simplex Lattice Design
#'
#' @param fac Number of Factors
#' @param lev Number of Levels per Factor
#' @param center logical: Is center point required; or numerical: number of center points
#' @param axial logical: Are axial points required
#'
#' @return The simplex lattice design
#' @export
#'
#' @examples SLDnew(3, 2, center=TRUE, axial=TRUE)
SLDnew <- function(fac, lev, center=FALSE, axial=FALSE){
  # Adapted from SLD function in mixexp package
  cnames <- paste("x", 1:fac, sep = "")
  SL <- t(combinat::xsimplex(fac,lev))/lev
  centermat <- matrix(1/fac, center, fac)
  axialmat <-  matrix(rep(unlist(matrix(0.5/fac, fac, fac) + 0.5*diag(fac)),axial), ncol=fac, byrow=T)
  SL <- rbind(SL,centermat,axialmat)
  colnames(SL) <- cnames
  dimS <- dim(SL)
  rows <- dimS[1]
  rnames <- paste(1:rows)
  rownames(SL) <- rnames
  return(SL)
}
