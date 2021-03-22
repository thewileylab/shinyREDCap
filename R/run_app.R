#' Run Application
#'
#' @description 
#' Start the shinyREDCap Application. 
#' 
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' 
#' @return No return value, called to start the shinyREDCap Application!
#' 
run_app <- function(
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(...)
  )
}

#' shinyREDCap: Access REDCap Instruments through R Shiny
#' 
#' A shiny module to connect you to a REDCap project and perform a chart review from within an R Shiny Application.
#' 
#' 
#' @docType package
#' @name shinyREDCap
NULL
#> NULL