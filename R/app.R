#' Title Shiny MLE
#'
#' @returns shiny app
#' @export
#'
#' @examples
#' \dontrun{shinymle()}
shinymle <- function(){
  shiny::runApp(system.file("Shiny",package = "FALL254753rury0000"),
                launch.browser = TRUE)
}
