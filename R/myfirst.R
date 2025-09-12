#' My first function
#'
#' @param x A numeric vector.
#'
#' @returns A list of y and x.
#' @export
#'
#' @examples
#' myfirstfunction(1:10)
myfirstfunction <- function(x){
  y <- x^2
  plot(y~x)
  return(list(x = x, y= y))
}
