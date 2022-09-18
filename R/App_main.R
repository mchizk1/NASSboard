
#' GUI for Interactive Mapping of NASS Census Data
#'
#' Running this function with no arguments will launch a general user interface for
#' mapping NASS data at the county level.
#'
#' @examples
#' NASSboard()
#'
#' @export

NASSboard <- function(){
  shiny::shinyApp(ui = ui(), server = server)
}
