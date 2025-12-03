#' Title number of tickets
#'
#' @param N number of seats on the plane
#' @param gamma acceptable probability of loss
#' @param p probability of a seat being filled
#'
#' @returns number of tickets to sell
#' @export
#'
#' @examples ntickets(N=400,gamma=0.02,p=0.95)
ntickets <- function(N=200,gamma=0.8,p=0.95) {

  upper=N
  limit <- while (qbinom(1-gamma,upper,0.95)<=N){
    invisible(upper)
    upper=upper+1
  }

  xx <- seq(N,upper*1.1,by=1) # figure out upper value
  obj1 <- abs(pbinom(N,xx,p)-1+gamma)
  n_d <- xx[which.min(obj1)]

  obj2 <- function(n,N1=N,p1=p,gamma1=gamma) {
    pnorm(N1+0.5,n*p1,sqrt(n*p1*(1-p1)))-1+gamma1
  }

  n_c <- uniroot(obj2,c(N,upper*1.2)) # figure out upper value

  curve(obj2, from=N-20,to=upper*1.1,
        main=paste("Normal Approximation object function optimized at n = ",round(n_c$root,4)),
        xlab = "Number of tickets sold",
        ylab = "Size of objective function")
  abline(v=n_c$root,h=pnorm(N+0.5,n_c$root*p,sqrt(n_c$root*p*(1-p)))-1+gamma,col="red")


  plot(xx,obj1,
       main = paste("Object function minimized at n = ",n_d),
       xlab = "Number of tickets sold",
       ylab = "Size of objective function",
       type = "b",
       pch = 21,
       bg = ifelse(xx != n_d, "lightgreen", "blue"),
  )

  lst <-list(nd = n_d, nc = n_c$root , N=N,p=p,gamma=gamma)
  invisible(lst)
}
