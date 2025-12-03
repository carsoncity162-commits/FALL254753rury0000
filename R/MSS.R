#' Model Sum of Squares
#'
#' @param yhat Estimate for y
#' @param mean Mean of y
#'
#' @returns The Model Sum of Squares given a vector estimate for y
#' @export
#'
#' @examples
MSS <- function(yhat,mean){
  sum((yhat-mean)^2)
}
