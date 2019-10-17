#' @export
#' @import shiny shinyjs shinyBS
#' @title Run GeneEx - webapp for sRACIPE
#' @description GeneEx enables interactive simulations, visualization, and data
#' analysis using sRACIPE.
#' @return Webapp interface 
runGeneEx <- function() {
  appDir <- system.file("GeneEx", package = "sRACIPE")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `sRACIPE`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
