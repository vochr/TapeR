#' @title construct B-Splines design matrices
#' @description Internal function not usually called by users
#' @param x relative height measurements
#' @param par.lme Fitted model object, return of \code{\link{TapeR_FIT_LME.f}}
#' @param ... not currently used
#'
#' @return List with height measurements (\code{x}), the fixed effects B-splines
#' matrix and the random effects B-splines matrix.
#' @author Edgar Kublin

XZ_BSPLINE.f <- function(x, knt, ord, ...){
    
    BS <- BSplines(knots = knt, ord = ord, der = 0, x = x)
    BS <- BS[1:nrow(BS), 1:ncol(BS)-1, drop=F]
    
    return(BS)
  }
