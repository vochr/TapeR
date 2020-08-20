#' @title find relative height of given diameter
#' @description Internal function not usually called by users
#' @param xm relative heights for which measurements are available
#' @param ym corresponding diameter measurements in height \code{xm}
#' @param y0 given diameter for which height is required
#' @param par.lme Fitted model object, return of \code{\link{TapeR_FIT_LME.f}}
#' @param R0 indicator whether taper curve should interpolate measurements
#' @param ... not currently used
#' @details function used to transform given diameter in volume calculation into
#' height; c.f \code{\link{E_VOL_AB_HmDm_HT.f}}; with \code{R0} one can decide 
#' whether the measured diameters are forced to lie exactly on the taper curve
#' \code{TRUE} or not \code{FALSE}. See also \code{\link{SK_EBLUP_LME.f}}.
#' @return relative height of given diameter \code{y0}
#' @author Edgar Kublin

xy0_SK_EBLUP_LME.f <-
function(xm, ym, y0, par.lme, R0=FALSE, ...){

		SK_LME = SK_EBLUP_LME.f(xm = xm, ym = ym, xp = c(0), 
		                        par.lme = par.lme, R0=R0)

		xmin <- uniroot(xy0_root.f, c(0,1), tol = 0.00001, y0 = y0, 
		                SK = SK_LME, par.lme = par.lme)

		x0 = xmin$root

		return(x0)

	}
