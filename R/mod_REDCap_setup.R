# UI ----
#' REDCap Setup UI
#'
#' @param id The module namespace
#'
#' @return The REDCap Setup UI
#' @export
#' 
#' @importFrom shinydashboard box
#' @importFrom shinyjs hidden
#' @importFrom shinycssloaders withSpinner
#'
redcap_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    shinydashboard::box(title = 'Connect to REDCap',
                        width = '100%',
                        status = 'danger',
                        solidHeader = F,
                        div(id=ns('redcap_connect_div'),
                            uiOutput(ns('setup')),
                            uiOutput(ns('setup_connect_btn')),
                            uiOutput(ns('setup_connect_error'))
                        ),
                        div(id=ns('redcap_connect_success_div'),
                            uiOutput(ns('setup_connect_success')) %>% shinycssloaders::withSpinner() 
                        )
    ),
    shinyjs::hidden(
      div(id=ns('redcap_configure_div'),
          shinydashboard::box(title = 'Configure REDCap',
                              width = '100%',
                              status = 'danger',
                              solidHeader = F,
                              div(id=ns('redcap_configuration_options_div'),
                                  uiOutput(ns('rc_configure_identifier')),
                                  uiOutput(ns('rc_configure_reviewer')),
                                  uiOutput(ns('rc_configure_select_reviewer')),
                                  uiOutput(ns('rc_configure_select_btn'))
                              ),
                              div(id=ns('redcap_configured_success_div'),
                                  uiOutput(ns('rc_configured_message')) %>% shinycssloaders::withSpinner()
                              )
          )
      )
    )
  )
}

# Server ----
#' REDCap Setup Server
#'
#' @param id The module namespace
#'
#' @return REDCap connection variables and project information
#' @export
#' 
#' @importFrom dplyr relocate as_tibble slice pull filter select mutate mutate_all case_when count distinct group_by
#' @importFrom magrittr %>% 
#' @importFrom purrr flatten_dfr
#' @importFrom redcapAPI exportProjectInformation exportFieldNames exportInstruments exportMetaData
#' @importFrom rlang .data
#' @importFrom shinyjs show hide reset
#' @importFrom stringr str_trim
#' @importFrom tidyr separate separate_rows replace_na
#' @importFrom tibble tibble deframe
#'
redcap_setup_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      ## REDCap Setup Values ----
      redcap_setup <- reactiveValues(
        ### Module Info
        moduleName = 'REDCap',
        moduleType = 'abstraction',
        ### Connection Variables
        rc_con = NULL,
        rc_project_info = NULL,
        rc_field_names = NULL,
        rc_record_id_field = NULL,
        rc_instruments_tbl = NULL,
        rc_meta_data = NULL,
        rc_record_id_label = NULL,
        rc_records = NULL,
        is_connected = 'no',
        ### Configuration Variables
        temp_identifier_field = NULL,
        temp_reviewer_field = NULL,
        config_error = NULL,
        requires_reviewer = NULL,
        identifier_label = NULL,
        identifier_field = NULL,
        reviewer_label = NULL,
        reviewer_field = NULL,
        reviewer = NULL,
        is_configured = 'no'
        )
      
      ## REDCap Connection ----
      setup <- reactive({
        tagList(
          textInput(inputId = ns('rc_url'),label = 'REDCap URL:',value = 'https://'),
          passwordInput(inputId = ns('rc_token'),label = 'REDCap API Token:')
          )
        })
      
      rc_connect_btn <- reactive({
        req(input$rc_url, input$rc_token)
        if(input$rc_url == '' | input$rc_token == '') {
          return(NULL)
          } else {
            actionButton(inputId = ns('rc_connect'),label = "Connect to REDCap",icon = icon('notes-medical'))
            }
        })
      
      rc_connect_error <- eventReactive(redcap_setup$rc_con, {
        if(is.character(redcap_setup$rc_con ) ) {
          if(redcap_setup$rc_con == 'redcap_url_error') {
            return(HTML("<font color='#e83a2f'>Incorrect REDCap API URL. Please ensure you entered the correct URL.</font>"))
            } else if (redcap_setup$rc_con == 'redcap_unknown_error') {
              return(HTML("<font color='#e83a2f'>An unexpected server response was received, please verify that a REDCap Instance exists at the specified URL.</font>"))
              } else if (redcap_setup$rc_con == 'redcap_token_error' ) {
                return(HTML("<font color='#e83a2f'>Incorrect API key. Please ensure you have enabled API access to your project and/or double check your API credentials.</font>"))
                } else {
                  return(NULL)
                  }
          } else {
            return(NULL)
            }
        })
      
      rc_connected_message <- eventReactive(redcap_setup$rc_project_info, {
        if (nrow(redcap_setup$rc_project_info ) > 0) {
          HTML(paste('<H3>Success!!</H3>', 
                     'You have connected to the', redcap_setup$rc_project_info$project_title, 'Project in REDCap.',
                     '<br>',
                     '<br>',
                     '<H4>Project Information:</H4>',
                     '<b>Project ID:</b>', redcap_setup$rc_project_info$project_id,
                     '<br>',
                     '<b>Created:</b>', redcap_setup$rc_project_info$creation_time,
                     '<br>',
                     '<b>Production Status:</b>', redcap_setup$rc_project_info$in_production,
                     '<br><br>',
                     '<b>Please configure a REDCap Instrument in the box below before continuing.</b>',
                     '<br><br>'))
          } else {HTML(paste('No REDCap Projects were found. Please connect to a different REDCap Project.',
                             '<br><br>')
                       )
            }
        })
      
      ## REDCap Connection UI Outputs ----
      output$setup <- renderUI({ setup() })
      output$setup_connect_btn <- renderUI({ rc_connect_btn() })
      output$setup_connect_error <- renderUI({ rc_connect_error() })
      output$setup_connect_success <- renderUI({ 
        req(rc_connected_message() )
        tagList(
          rc_connected_message(),
          actionButton(inputId = ns('rc_disconnect'),label = 'Disconnect')
          )
        })
      
      ## REDCap Connection Observers ----
      observeEvent(input$rc_connect, { ### store redcapAPI connection object (or error) when connect button is pressed
        if ( input$rc_connect == 0 ) return()
        redcap_setup$rc_con <- redcap_connection(input$rc_url, input$rc_token)
        })
      
      observeEvent(redcap_setup$rc_con, {
        # browser()
        message('Retrieving REDCap project information')
        if (redcap_setup$rc_con %>% class() == 'redcapApiConnection') { ### When correct information is entered, the class of rc_con will be redcapApiConnection
          shinyjs::hide('redcap_connect_div') ### Hide REDCap connection GUI
          redcap_setup$rc_project_info <- redcapAPI::exportProjectInformation(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store Project Info
          redcap_setup$rc_field_names <- redcapAPI::exportFieldNames(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store REDCap Field Names
          redcap_setup$rc_record_id_field <- redcap_setup$rc_field_names %>% slice(1) %>% pull(.data$export_field_name) ### Store REDCap Record ID field (always first field)
          redcap_setup$rc_instruments_tbl <- redcapAPI::exportInstruments(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store REDCap Instrument Names
          redcap_setup$rc_instruments_list <-redcap_setup$rc_instruments_tbl %>% 
            relocate(.data$instrument_label, .data$instrument_name) %>% 
            deframe()
          redcap_setup$rc_meta_data <- redcapAPI::exportMetaData(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store REDCap Instrument Meta Data
          redcap_setup$rc_record_id_label <- redcap_setup$rc_meta_data %>% 
            filter(.data$field_name == redcap_setup$rc_record_id_field)%>% 
            pull(.data$field_label)
          redcap_setup$rc_meta_exploded <- redcap_setup$rc_meta_data %>% 
            select(.data$field_name, .data$field_type, .data$select_choices_or_calculations) %>% 
            mutate(select_choices_or_calculations = case_when(.data$field_type == 'yesno' ~ '1, Yes | 0, No',
                                                              .data$field_type == 'truefalse' ~ '1, True | 0, False',
                                                              TRUE ~ .data$select_choices_or_calculations
                                                              )
                   ) %>% 
            separate_rows(.data$select_choices_or_calculations, sep = '\\|') %>% 
            separate(.data$select_choices_or_calculations, into = c('value','value_label'), sep = ',') %>% 
            mutate_all(str_trim) %>% 
            mutate_all(replace_na, replace = '')
          redcap_setup$rc_records <- safe_exportRecords(redcap_setup$rc_con, redcap_setup$rc_field_names)
          redcap_setup$is_connected <- 'yes' ### Report REDCap is connected
          message('Complete')
          shinyjs::show('redcap_configure_div') ### Show REDCap configure GUI
          shinyjs::show('redcap_configuration_options_div')
          } 
        })
      
      observeEvent(input$rc_disconnect, { ### On REDCap disconnect
        if ( input$rc_connect == 0 ) return()
        redcap_setup$rc_con <- NULL ### Clear REDCap connection info
        redcap_setup$rc_project_info <- NULL ### Clear REDCap Project Information
        redcap_setup$rc_field_names <- NULL ### Clear REDCap Field Names
        redcap_setup$rc_record_id_field <- NULL ### Clear REDCap Record ID Field
        redcap_setup$rc_instruments_tbl <- NULL ### Clear REDCap Instruments
        redcap_setup$rc_instruments_list <- NULL ### Clear REDCap Instruments List
        redcap_setup$rc_meta_data <- NULL ### Clear REDCap meta data
        redcap_setup$rc_record_id_label <- NULL ### Clear REDCap Record ID Label
        redcap_setup$rc_meta_exploded <- NULL ### Clear the exploded REDCap meta data
        redcap_setup$rc_records <- NULL ### Clear initially collected REDCap Records
        redcap_setup$is_connected <- 'no' ### Report REDCap is disconnected
        shinyjs::show('redcap_connect_div') ### Show REDCap connection GUI
        shinyjs::reset('redcap_connect_div') ### Reset inputs on REDCap connection GUI
        shinyjs::reset('redcap_configure_div')
        shinyjs::hide('redcap_configure_div') ### Hide REDCap configuration GUI
        })
      
      ## REDCap Configuration ----
      redcap_project_record_id_selectInput <- reactive({
        req(redcap_setup$rc_meta_data )
        selectInput(inputId = ns('rc_identifier_field'),
                    label = 'Which variable contains your record identifier (e.g., MRN, subject ID)?',
                    choices = redcap_setup$rc_meta_data %>%
                      slice(-1) %>% ### Remove the REDCap Identifier Field
                      filter(.data$field_type == 'text') %>% 
                      select(.data$field_label) %>%
                      deframe()
                    )
        })
      
      redcap_project_reviewer_id_selectInput <- reactive({
        req(redcap_setup$rc_meta_data, input$rc_identifier_field )
        selectInput(inputId = ns('rc_reviewer_field'), 
                    label = 'Which variable contains your reviewer identifier?',
                    choices = append('(Not Applicable)', 
                                     redcap_setup$rc_meta_data %>%
                                       slice(-1) %>% ### Remove the REDCap Identifier Field
                                       filter(.data$field_type == 'text' & .data$field_label != input$rc_identifier_field ) %>% 
                                       select(.data$field_label) %>%
                                       deframe()
                                     )
                    )
        })
      
      observeEvent(c(redcap_setup$rc_records, redcap_setup$rc_meta_data, input$rc_identifier_field, input$rc_reviewer_field), {
        # browser()
        req(redcap_setup$is_connected == 'yes', input$rc_reviewer_field)
        redcap_setup$temp_identifier_field <- redcap_setup$rc_meta_data %>% 
          filter(.data$field_label == input$rc_identifier_field) %>% 
          pull(.data$field_name)
        
        redcap_setup$temp_reviewer_field <- if(input$rc_reviewer_field == '(Not Applicable)') {
          '(Not Applicable)'
          } else {
            redcap_setup$rc_meta_data %>% 
              filter(.data$field_label == input$rc_reviewer_field) %>% 
              distinct(.data$field_label,.keep_all = T) %>% ## identical labels? Go with the first
              pull(.data$field_name)
            }
        qty_redcap_records <- redcap_setup$rc_records %>% 
          select(redcap_setup$temp_identifier_field) %>% 
          nrow()
        qty_identifiers <- redcap_setup$rc_records %>% 
          select(redcap_setup$temp_identifier_field) %>%
          distinct() %>% 
          nrow()
        reviews_per_identifier_field <- if(redcap_setup$temp_reviewer_field != '(Not Applicable)') {
          redcap_setup$rc_records %>%
            select(redcap_setup$temp_identifier_field, redcap_setup$temp_reviewer_field) %>% 
            group_by(!!as.name(redcap_setup$temp_identifier_field), !!as.name(redcap_setup$temp_reviewer_field) ) %>% 
            count() 
          } else {
            tibble::tibble(n = '', .rows = 0)
            }
        reviewer_check <- reviews_per_identifier_field %>% 
          filter(.data$n > 1) %>%
          flatten_dfr() %>%
          nrow()
        if(reviewer_check > 0 ) {
          redcap_setup$config_error <- 'yes'
          } else {
            redcap_setup$config_error <- 'no'
            }
        if(qty_redcap_records > qty_identifiers) {
          redcap_setup$requires_reviewer <- 'yes'
          } else {
            redcap_setup$requires_reviewer <- 'no'
          }
        })
      
      rc_current_reviewer_selectInput <- reactive({
        req(input$rc_reviewer_field, redcap_setup$config_error, redcap_setup$requires_reviewer)
        if( input$rc_reviewer_field == '(Not Applicable)' | redcap_setup$temp_reviewer_field == '(Not Applicable)') {
          return(NULL)
          } else if(redcap_setup$config_error == 'yes'){
            return(HTML("<font color='#e83a2f'>Warning: This REDCap instrument contains multiple records from the same reviewer for an individual record identifier. Please visit REDCap via the web to correct the instrument. </font>"))
            } else {
              rc_previous_reviewers <- redcap_setup$rc_records %>% 
                select(redcap_setup$temp_reviewer_field)
              selectizeInput(inputId = ns('rc_current_reviewer'),
                             label = 'Select your name from the list, or enter a new one:',
                             choices = append('',
                                              rc_previous_reviewers
                                              ),
                             selected = '',
                             options = list(create = TRUE,
                                            placeholder = 'New Reviewer'))
            }
        })
      
      rc_configure_btn <- reactive({
        req(input$rc_reviewer_field, redcap_setup$requires_reviewer, redcap_setup$config_error)
        input$rc_reviewer_field
        if(input$rc_reviewer_field == '(Not Applicable)' & redcap_setup$requires_reviewer == 'yes' ) {
          return(HTML("<font color='#e83a2f'>Warning: Multiple REDCap records exist for unique record identifiers. Please configure a reviewer identifier.</font>"))
          } else if (input$rc_reviewer_field == '(Not Applicable)' & redcap_setup$requires_reviewer == 'no' ) {
            actionButton(inputId = ns('rc_configure_btn'), label = 'Configure REDCap')
            } else if (is.null(input$rc_current_reviewer) ) { ### This input is null for a very brief moment while initializing. Don't allow configuration at this point in time.
              return(NULL)
              } else if (input$rc_current_reviewer == '') {
                return(NULL)
                } else {
                  actionButton(inputId = ns('rc_configure_btn'), label = 'Configure REDCap Instrument') 
                  }
        })
      
      observeEvent(input$rc_configure_btn, {
        # browser()
        shinyjs::hide('redcap_configuration_options_div')
        redcap_setup$identifier_label <- input$rc_identifier_field
        redcap_setup$identifier_field <- redcap_setup$temp_identifier_field
        redcap_setup$reviewer_label <- input$rc_reviewer_field
        redcap_setup$reviewer_field <- redcap_setup$temp_reviewer_field
        redcap_setup$reviewer <- input$rc_current_reviewer
        redcap_setup$is_configured <- 'yes'
        shinyjs::show('redcap_configured_success_div')
        })
      
      observeEvent(redcap_setup$is_configured, 
                   ignoreInit = T, {
                     req(redcap_setup$is_configured == 'yes')
                     if(redcap_setup$reviewer_field == '(Not Applicable)') {
                       redcap_setup$rc_configured_message <- HTML(
                         paste('<H3>Success!!</H3>',
                               'You have configured the REDCap Instrument.',
                               '<br>',
                               '<br>',
                               '<H4>Configuration Information:</H4>',
                               '<b>Identifier Field:</b>', redcap_setup$identifier_label,
                               '<br>',
                               '<b>Reviewer Field:</b>', redcap_setup$reviewer_label,
                               '<br><br>',
                               '<b>You may now proceed to record review. Have fun and watch out for bugs!</b>',
                               '<br><br>'))
                       } else {
                         redcap_setup$rc_configured_message <- HTML(
                           paste('<H3>Success!!</H3>',
                                 'You have configured the REDCap Instrument.',
                                 '<br>',
                                 '<br>',
                                 '<H4>Configuration Information:</H4>',
                                 '<b>Identifier Field:</b>', redcap_setup$identifier_label,
                                 '<br>',
                                 '<b>Reviewer Field:</b>', redcap_setup$reviewer_label,
                                 '<br>',
                                 '<b>Reviewer Name:</b>', redcap_setup$reviewer,
                                 '<br><br>',
                                 '<b>You may now proceed to record review. Have fun and watch out for bugs!</b>',
                                 '<br><br>'))
                         }
                     })
      
      # REDCap Configuration UI Outputs ----
      output$rc_configure_identifier <- renderUI({ redcap_project_record_id_selectInput() })
      output$rc_configure_reviewer <- renderUI({ redcap_project_reviewer_id_selectInput() })
      output$rc_configure_select_reviewer <- renderUI({ rc_current_reviewer_selectInput() })
      output$rc_configure_select_btn <- renderUI({ rc_configure_btn() })
      output$rc_configured_message <- renderUI({ 
        req(redcap_setup$rc_configured_message)
        tagList(
          redcap_setup$rc_configured_message,
          actionButton(inputId = ns('rc_reconfig'),label = 'Reconfigure REDCap')
          )
        })
      
      observeEvent(input$rc_reconfig, { 
        # browser()
        shinyjs::hide('redcap_configured_success_div')
        redcap_setup$is_configured <- 'no'
        redcap_setup$temp_identifier_field <- NULL
        redcap_setup$temp_reviewer_field <- NULL
        redcap_setup$config_error <- NULL
        redcap_setup$requires_reviewer <- NULL
        redcap_setup$identifier_label <- NULL
        redcap_setup$identifier_field <- NULL
        redcap_setup$reviewer_label <- NULL
        redcap_setup$reviewer_field <- NULL
        redcap_setup$reviewer <- NULL
        redcap_setup$rc_configured_message <- NULL
        redcap_setup$rc_records <- safe_exportRecords(redcap_setup$rc_con, redcap_setup$rc_field_names) ### pull records, just in case data was entered
        shinyjs::show('redcap_configuration_options_div')
        shinyjs::reset('redcap_configuration_options_div')
        })
      
      observeEvent(input$rc_disconnect, { 
        # browser()
        message('REDCap Disconnect')
        shinyjs::hide('redcap_configured_success_div')
        redcap_setup$is_configured <- 'no'
        redcap_setup$temp_identifier_field <- NULL
        redcap_setup$temp_reviewer_field <- NULL
        redcap_setup$config_error <- NULL
        redcap_setup$requires_reviewer <- NULL
        redcap_setup$identifier_label <- NULL
        redcap_setup$identifier_field <- NULL
        redcap_setup$reviewer_label <- NULL
        redcap_setup$reviewer_field <- NULL
        redcap_setup$reviewer <- NULL
        redcap_setup$rc_configured_message <- NULL
        shinyjs::reset('redcap_configure_div')
        shinyjs::hide('redcap_configure_div')
        })
      
      ## Return
      return(redcap_setup)
      }
  )
}
