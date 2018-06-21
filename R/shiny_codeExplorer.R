#' @title Interactive code exploration
#'
#' @description Shiny app for code exploration.
#'
#'
#' @return Run a Shiny app.
#'
#' @importFrom shiny runApp
#'
#' @export
shiny_codeExplorer <- function(){
  appDir <- system.file("shiny", package = "codeExplorer")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing package.", call. = FALSE)
  }
  
  runApp(appDir, display.mode = "normal")
}