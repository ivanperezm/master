#' Start master
#' 
#' Start master template
#' @author Ivan Perez
#' @export

start <- function(){
  fp = system.file("shinydashboard", package = "master")
  shiny::runApp(fp)
}