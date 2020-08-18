#' @title percentile for estimated taper curve diameter
#' @description Internal funtions not usually called by users.
#' @param Ht tree height
#' @param Hx Numeric vector of stem heights (m) along which to return the expected diameter.
#' @param qD vector of quantiles, passed to \code{pnorm}
#' @param Hm measured height of respective diameters \code{Dm}
#' @param Dm measured diameter
#' @param par.lme List of taper model parameters obtained by \code{\link{TapeR_FIT_LME.f}}.
#' @param ... 
#' @return percentile for estimated taper curve diameter at position \code{Hx}
#' given \code{Ht}, \code{Hm} and \code{Dm}
#'
#' @examples
CdN_DHxHt.f <-
function(Ht, Hx, qD, Hm, Dm, par.lme, ...){#Prb N[D(Hx|N(mw(Ht),sd(Ht))<= qD| Ht/Hm,Dm]
#   ------------------------------------------------------------------------------------------------

#   Percentil fuer geschaetzten Schaftkurvendurchmesser an der Stelle Hx gegeben Ht und (Hm,Dm) :.....

		if(Ht>Hx) {

			SK 	= E_DHx_HmDm_HT.f( Hx, Hm, Dm, mHt = Ht, sHt = 0, par.lme)

			m_DHxHt = as.numeric(SK$DHx)
			s_DHxHt = sqrt(as.numeric(SK$MSE_Mean))

			CdN_DHxHt = pnorm(q = qD, mean = m_DHxHt, sd = s_DHxHt, lower.tail = T, log.p = F)

		}else{
			CdN_DHxHt = 1
		}
			return(CdN_DHxHt)
	}