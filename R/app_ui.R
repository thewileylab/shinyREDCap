#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  fluidPage(
    column(width = 6,
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
           redcap_instrument_ui(id = 'instrument_namespace')
           )
  )
  }

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'shinyREDCap'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyWidgets::useShinydashboard(),
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs()
  )
}

