# Helper Functions ----

#' REDCap Connection
#' 
#' A 'safe' wrapper for redcapAPI::redcapConnection(). Will return diagnostic error codes in case incorrect URL or token are provided.
#'
#' @param url The API URL for your institution's REDCap instance
#' @param token The API token for your REDCap project
#' @keywords internal
#' @return If the URL and token are correct, return a redcapAPI connection object. Else, return diagnostic error. 
#' @export
#' @importFrom redcapAPI exportProjectInformation redcapConnection
#' @importFrom stringr str_detect regex
#'
redcap_connection <- function(url, token) { 
  connection_status <- tryCatch({
    project_info <- redcapAPI::exportProjectInformation(redcapAPI::redcapConnection(url, token))
    if(nrow(project_info) == 1) {
      return(redcapAPI::redcapConnection(url,token))
    } else {
      return('redcap_unknown_error')
    }
  }, 
  error=function(error_cond) {
    if(str_detect(as.character(error_cond), pattern = regex('Could not resolve host:', ignore_case = T)) ) {
      message("Incorrect REDCap API URL. If Macbook is 2015-2020 model year, check for stuck keys. Otherwise, make sure you used the correct URL.")
      return('redcap_url_error') 
      } else if (str_detect(as.character(error_cond), pattern = regex('You do not have permissions to use the API', ignore_case = T)) ) {
        message('Incorrect API key. Please ensure you have enabled API access to your project and/or double check your API credentials.')
        return('redcap_token_error')
        } else {
          message("An unexpected server response was received, please verify that a REDCap Instance exists at the specified URL.")
          return('redcap_unknown_error')
          }
    })
  return(connection_status)
}

#' Render REDCap Instrument
#'
#' Collection of functions to map REDCap question types to native Shiny widgets.
#' 
#' @param id Unique REDCap question identifier
#' @param field_label Question text, with formatting
#' @param value Default value or previous data if question has previously been answered 
#' @param placeholder Placeholder text to help a reviewer decide how to answer the question
#' @param ... Any additional parameters
#'
#' @rdname render_redcap_instrument
#' @keywords internal
#' @export
#' @import shiny
#' @importFrom tibble tibble add_row
#' @importFrom tidyr separate_rows separate
#' @importFrom dplyr mutate_all select if_else
#' @importFrom purrr flatten
#' @importFrom stringr str_trim
#' @importFrom rlang .data
#' 

## Create Shiny Widget Translation Functions 
reviewr_textInput <- function(id, field_label, value = NULL, placeholder = NULL, ...) {
  textInput(inputId = id ,label = HTML(field_label), value = value , placeholder = placeholder)
}

#' @rdname render_redcap_instrument
#' @keywords internal
#' @export
reviewr_dateInput <- function(id, field_label, value = NULL, ...) {
  dateInput(inputId = id, label = HTML(field_label), value = value)
}

#' @param required Is this a required REDCap question type?
#' @param choices REDCap choices for the question.
#' @rdname render_redcap_instrument
#' @keywords internal
#' @export
reviewr_dropdown <- function(id, field_label, required, choices, value = NULL, ...) {
  ## Create selectable choices
  required_choice <- if_else(is.na(required), '[Leave Blank]', '[Not Yet Answered]')
  temp <- tibble(choices = choices) %>% 
    separate_rows(choices, sep = '\\|') %>% 
    separate(col = choices, into = c('Values','Names'),sep = ',') %>% 
    mutate_all(str_trim) %>% 
    mutate_all(as.character) %>% 
    add_row(Values = '', Names = required_choice)
  dropdown_choices <- temp$Values
  names(dropdown_choices) <- temp$Names
  dropdown_choices = dropdown_choices
  selectInput(inputId = id, label = HTML(field_label), choices = dropdown_choices, selected = value)
}

#' @rdname render_redcap_instrument
#' @keywords internal
#' @export
reviewr_truefalse <- function(id, field_label, required, value = NULL, ...) {
  if(is.na(required) ) {
    radio_names <- list('True', 'False', HTML("<font color='grey'>[Leave Blank]</font>"))
  } else {
    radio_names <- list('True', 'False', HTML("<font color='grey'>[Not Yet Answered]</font>"))
  }
  radio_values <- c(1, 0, '')
  radioButtons(inputId = id, label = HTML(field_label), choiceNames = radio_names, choiceValues = radio_values, selected = value)
}

#' @rdname render_redcap_instrument
#' @keywords internal
#' @export
reviewr_yesno <- function(id, field_label, required, value = NULL, ...) {
  if(is.na(required) ) {
    radio_names <- list('Yes', 'No', HTML("<font color='grey'>[Leave Blank]</font>"))
  } else {
    radio_names <- list('Yes', 'No', HTML("<font color='grey'>[Not Yet Answered]</font>"))
  }
  radio_values <- c(1, 0, '')
  radioButtons(inputId = id, label = HTML(field_label), choiceNames = radio_names, choiceValues = radio_values, selected = value)
}

#' @rdname render_redcap_instrument
#' @keywords internal
#' @export
reviewr_radio <- function(id, field_label, required, choices, value = NULL, ...) {
  ## Create selectable choices
  if(is.na(required) ) {
    append_val <- list(HTML("<font color='grey'>[Leave Blank]</font>"))
  } else {
    append_val <- list(HTML("<font color='grey'>[Not Yet Answered]</font>"))
  }
  temp <- tibble(choices = choices) %>% 
    separate_rows(choices, sep = '\\|') %>% 
    separate(col = choices, into = c('Values','Names'),sep = ',') %>% 
    mutate_all(str_trim) %>% 
    mutate_all(as.character)
  radio_names <- temp %>% 
    select(.data$Names) %>% 
    flatten() %>% 
    append(append_val)
  radio_values <- temp %>% 
    select(.data$Values) %>% 
    flatten() %>% 
    append(list(''))
  radioButtons(inputId = id, label = HTML(field_label), choiceNames = radio_names, choiceValues = radio_values, selected = value)
}

#' @rdname render_redcap_instrument
#' @keywords internal
#' @export
reviewr_checkbox <- function(id, field_label, choices, value = NULL, ...) {
  ## Create selectable choices
  temp <- tibble(choices = choices) %>% 
    separate_rows(choices, sep = '\\|') %>% 
    separate(col = choices, into = c('Values','Names'),sep = ',') %>% 
    mutate_all(str_trim)
  checkbox_choices <- temp$Values
  names(checkbox_choices) <- temp$Names
  checkboxGroupInput(inputId = id, label = HTML(field_label), choices = checkbox_choices, selected = value)
}

#' @rdname render_redcap_instrument
#' @keywords internal
#' @export
reviewr_notes <- function(id, field_label, value = NULL, ...) {
  textAreaInput(inputId = id, label = HTML(field_label), value = value)
}

#' @rdname render_redcap_instrument
#' @keywords internal
#' @export
reviewr_integer <- function(id, field_label, value = NULL, ...) {
  numericInput(inputId = id, label = HTML(field_label), value = value)
}

## Render REDCap Instrument shinyInput Tags

#' @param current_subject_data Previous REDCap data on the current subject
#' @rdname render_redcap_instrument
#' @keywords internal
#' @export
render_redcap <- function(reviewr_type, field_name, field_label, required, choices, current_subject_data = NULL ) {
  if(reviewr_type == 'reviewr_text') {   ## Text: textInput 
    reviewr_textInput(id = field_name, field_label = field_label, value = current_subject_data)
  } else if(reviewr_type == 'reviewr_date') {             ## Date: dateInput 
    reviewr_dateInput(id = field_name, field_label = field_label, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_dropdown') {        ## DropDown: selectInput
    reviewr_dropdown(id = field_name, field_label = field_label, required, choices = choices, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_truefalse') {       ## TrueFalse: radioButtoms 
    reviewr_truefalse(id = field_name, field_label = field_label, required, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_yesno') {           ## YesNo: radioButtons 
    reviewr_yesno(id = field_name, field_label = field_label, required, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_radio') {           ## Radio: radioButtons 
    reviewr_radio(id = field_name, field_label = field_label, required, choices = choices, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_checkbox') {        ## Checkbox: checkboxGroupInput 
    reviewr_checkbox(id = field_name, field_label = field_label, choices = choices, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_notes') {           ## Notes: textAreaInput 
    reviewr_notes(id = field_name, field_label = field_label, value = current_subject_data)
  } else if (reviewr_type == 'reviewr_integer') {         ## Integer: numericInput 
    reviewr_integer(id = field_name, field_label = field_label, value = current_subject_data)
  } else {                                                ## Unsupported input 
    reviewr_textInput(id = field_name, field_label = "This is an unsupported field type", placeholder = reviewr_type)
  }
}

# Datasets ----
#' REDCap Survey Complete Tbl
#'
#' A dataset containing valid REDCap "Survey Complete" Values. 
#'
#' @docType data
#'
#' @format A data frame with 2 rows and 2 variables:
#' \describe{
#'   \item{redcap_survey_complete_names}{The human readable "Survey Complete" Responses}
#'   \item{redcap_survey_complete_values}{REDCap API values for "Survey Complete" Responses}
#'   ...
#' }
"redcap_survey_complete_tbl"

# UI ----
redcap_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinydashboard(),
    useShinyjs(),
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
                                uiOutput(ns('rc_configured_message'))
                                )
          )
        )
    )
}

redcap_instrument_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinydashboard(),
    useShinyjs(),
    shinydashboard::box(title = "REDCap Instrument: Title Placeholder",
                        width = '100%',
                        status = 'danger',
                        solidHeader = F
                        )
  )
}

# Server ----
redcap_setup_server <- function(input, output, session) {
  ns <- session$ns
  ## Start these divs in a hidden state
  shinyjs::hide('redcap_configure_div')
  shinyjs::hide('redcap_configured_success_div')
  ## REDCap Setup Values ----
  redcap_setup <- reactiveValues(
    ### Module Info
    moduleName = 'REDCap',
    moduleType = 'abstraction',
    ### Connection Variables
    rc_con = NULL,
    rc_project_info = NULL,
    rc_field_names = NULL,
    rc_instrument_names = NULL,
    rc_meta_data = NULL,
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
                       '<br><br>'))
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
    if (redcap_setup$rc_con %>% class() == 'redcapApiConnection') { ### When correct information is entered, the class of rc_con will be redcapApiConnection
      shinyjs::hide('redcap_connect_div') ### Hide REDCap connection GUI
      redcap_setup$rc_project_info <- redcapAPI::exportProjectInformation(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store Project Info
      redcap_setup$rc_field_names <- redcapAPI::exportFieldNames(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store REDCap Field Names
      redcap_setup$rc_instrument_names <- redcapAPI::exportInstruments(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store REDCap Instrument Names
      redcap_setup$rc_meta_data <- redcapAPI::exportMetaData(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store REDCap Instrument Meta Data
      redcap_setup$rc_records <- redcapAPI::exportRecords(redcap_setup$rc_con, factors = F, labels = F) %>% dplyr::as_tibble() ### Store REDCap Records that exist upon connection to assist with configuration.
      redcap_setup$is_connected <- 'yes' ### Report REDCap is connected
      shinyjs::show('redcap_configure_div') ### Show REDCap configure GUI
    } 
  })
  observeEvent(input$rc_disconnect, { ### On REDCap disconnect
    if ( input$rc_connect == 0 ) return()
    redcap_setup$rc_con <- NULL ### Clear REDCap connection info
    redcap_setup$rc_project_info <- NULL ### Clear REDCap Project Information
    redcap_setup$is_connected <- 'no' ### Report REDCap is disconnected
    shinyjs::show('redcap_connect_div') ### Show REDCap connection GUI
    shinyjs::reset('redcap_connect_div') ### Reset inputs on REDCap connection GUI
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
  
  observeEvent(input$rc_reviewer_field, {
    req(input$rc_reviewer_field)
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
    # redcap_setup$requires_reviewer 
  })
  
  rc_current_reviewer_selectInput <- reactive({
    req(input$rc_reviewer_field, redcap_setup$config_error, redcap_setup$requires_reviewer)
    if( input$rc_reviewer_field == '(Not Applicable)' ) {
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
    shinyjs::show('redcap_configuration_options_div')
    shinyjs::reset('redcap_configuration_options_div')
  })
  observeEvent(input$rc_disconnect, { 
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
    shinyjs::reset('redcap_configure_div')
    shinyjs::hide('redcap_configure_div')
    

  })
  ## Return
  return(redcap_setup)
}

redcap_instrument_server <- function(input, output, session, redcap_vars, subject_id) {
  ns <- session$ns
}