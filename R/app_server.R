#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  observeEvent(input$debug, {
    browser()
  })
  # Call the setup server function
  setup_vars <- redcap_server(id = 'setup_namespace', subject_id)
  # Encapsulate the subject ID selector as a reactive
  subject_id <- reactive({ input$subject_id }) ## Pass to instrument function
}
