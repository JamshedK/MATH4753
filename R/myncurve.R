#' Title
#'
#' @param mu
#' @param sigma
#' @param a
#'
#' @return
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(x=c(mu-3*sigma, xcurve, a),
          y=c(0,ycurve, 0),
          col="Red")
  return(list(pnorm(a, mean=mu, sd=sigma)))
}
