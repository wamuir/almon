#' Generate polynomial disributed lags from a vector.
#'
#' @param x a vector of time-ordered observations.
#' @param degree the degree of the polynomial. Default = 2.
#' @param lags the number of lags desired. Default = 12.
#'
#' @references  Almon, Shirley. 1965. The distributed lag between capital 
#'        appropriations and net expenditures. Econometrica, 33, 178-196.
#'
#' @export

pdl <- function(x, degree=2, lags=12) {
	alphas <- data.frame(matrix(ncol=0, nrow=length(x)))
	lmat <- sapply(0:lags, function(l) c(rep(NA,l),x)[1:length(x)])
	alpha.mat <- sapply(0:degree, function(d){
	    rowSums(lmat %*% diag(c(0:lags)^d))
	    })
	colnames(alpha.mat) <- paste0("alpha",0:degree)
	return(alpha.mat)
}

