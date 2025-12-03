#' Title birthday
#'
#' @param x total number of people in the room
#'
#' @returns probability of having 2 or more people in the with the same birthday
#' @export
#'
#' @examples birthday(20)
birthday <- function(x){
  1- exp(lchoose(365,x)+lfactorial(x)-x*log(365))
}
