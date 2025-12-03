#' myncurve
#'
#' @param mu the mean for the normal distribution
#' @param sigma the standard deviation for the normal distribution
#' @param a the x value to calculate the percentile
#'
#' @returns a normal distribution with the P(X<a)
#' @export
#'
#' @examples myncurve(mu=0,sigma=1,a=0)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurv = seq(mu-3*sigma,a,length=1000)
  ycurv = dnorm(xcurv,mean=mu,sd=sigma)
  polygon(x=c(mu-3*sigma,xcurv,a),y=c(0,ycurv,0),col="lightblue")
  prob = pnorm(a,mean=mu,sd=sigma)
  list(mu = mu, sigma = sigma, probability = prob)
}


