#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  fluidPage(
    column(width = 6,
           actionButton(inputId = 'debug',label = 'Debug'),
           tags$h2("Setup REDCap"),
           # Call the setup UI function
           redcap_setup_ui(id = 'setup_namespace')
           ),
    column(width = 6,
           tags$h2("Interact with REDCap Instruments"),
           tags$h3("Select a patient from the list"),
           # Create a subject ID selector. This can come from anywhere, 
           selectInput(inputId = 'subject_id',label = 'Subject ID',choices = c('922873','922874', '922875','922876','922877','922878')),
           # Call the redcap instrument UI function
           redcap_instrument_ui(id = 'setup_namespace')
           )
  )
  }
