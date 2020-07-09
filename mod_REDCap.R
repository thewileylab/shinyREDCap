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
#' @importFrom httr::config
#'

### REDCap API Security ----
### It is good practice to ensure that SSL certs are verified when utilizing the REDCap API. REDCap recommends setting the 
### 'CURLOPT_SSL_VERIFYPEER' option to TRUE to avoid potential man in the middle attacks.
###  - https://redcap.ucdenver.edu/api/help/?content=security
### 
### The redcapAPI package utilizes the httr package to perform operations using the REDCap API. Configuration options can be 
### passed directly to httr via the config option in the redcapConnection function. Here, we set 'ssl_verifypeer = 1L' to ensure
### cert checking is enabled.
### - https://www.rdocumentation.org/packages/redcapAPI/versions/2.3/topics/redcapConnection
### - https://httr.r-lib.org/reference/httr_options.html

redcap_connection <- function(url, token) { 
  connection_status <- tryCatch({
    project_info <- redcapAPI::exportProjectInformation(redcapAPI::redcapConnection(url, token, config = httr::config( ssl_verifypeer = 1L )))
    if(nrow(project_info) == 1) {
      return(redcapAPI::redcapConnection(url,token, config = httr::config( ssl_verifypeer = 1L )))
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
                                uiOutput(ns('rc_configured_message')) %>% shinycssloaders::withSpinner()
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
    shinydashboard::box(title = "REDCap Instrument",
                        width = '100%',
                        status = 'danger',
                        solidHeader = F,
                        actionButton(inputId = ns('boop'),label = 'boop'),
                        uiOutput(ns('instrument_select')),
                        uiOutput(ns('instrument_ui')) %>% withSpinner(type = 5, color = '#e83a2f')
                        ),
    shinydashboard::box(title = 'Upload to REDCap',
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
    rc_instruments_tbl = NULL,
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
      redcap_setup$rc_instruments_tbl <- redcapAPI::exportInstruments(redcap_setup$rc_con) %>% dplyr::as_tibble() ### Store REDCap Instrument Names
      redcap_setup$rc_instruments_list <-redcap_setup$rc_instruments_tbl %>% 
        relocate(instrument_label, instrument_name) %>% 
        deframe()
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
    redcap_setup$rc_field_names <- NULL ### Clear REDCap Field Names
    redcap_setup$rc_instruments_tbl <- NULL ### Clear REDCap Instruments
    redcap_setup$rc_instruments_list <- NULL ### Clear REDCap Instruments List
    redcap_setup$rc_meta_data <- NULL ### Clear REDCap meta data
    redcap_setup$rc_records <- NULL ### Clear initially collected REDCap Records
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
  
  observeEvent(input$boop, {
    browser()
  })
  ## REDCap Instrument Values ----
  redcap_instrument <- reactiveValues(
    selected_instrument_meta = NULL
  )
  instrument_select <- reactive({
    req(redcap_vars$rc_instruments_list, redcap_vars$is_configured == 'yes')
    selectInput(inputId = ns('rc_instrument_selection'),
                label = 'Select REDCap Instrument',
                choices = redcap_vars$rc_instruments_list
                )
    })
  
  ## Extract and Prep REDCap Instrument ----
  observeEvent(input$rc_instrument_selection, {
    redcap_instrument$selected_instrument_meta <- redcap_vars$rc_meta_data %>%
      slice(-1) %>%   # We drop the first row, as it most likely is the auto-increment field used in REDCap
      filter(str_to_lower(.data$form_name) == input$rc_instrument_selection ) %>% # Extract the instrument based on the user selection
      rownames_to_column() %>%
      filter(!.data$field_type %in% c('slider','calc','descriptive')) %>%
      # If some information is not defined within REDCap, it will convert those to logical types by default.  We are
      # assuming that they will be all character values, so we need to perform explicit casting to continue with that
      # assumption.
      mutate_if(is.logical, as.character) %>%
      left_join(ReviewR::redcap_widget_map,
                by = c('field_type' = 'redcap_field_type', 'text_validation_type_or_show_slider_number' = 'redcap_field_val')
      ) %>%
      unite(col = 'shiny_inputID', .data$field_name, .data$reviewr_redcap_widget_function, sep = '_', remove = F) %>%
      mutate(section_header = coalesce(.data$section_header, ''),
             field_note = coalesce(.data$field_note, '')
      )
  })
  ## Create Default REDCap Data Structures
  observeEvent(c(subject_id(), redcap_vars$is_configured), {
    req(redcap_vars$is_connected == 'yes', redcap_vars$is_configured == 'yes', subject_id())
    ### Special case, when the REDCap Instrument has no previous data
    redcap_instrument$default_data <- if(redcapAPI::exportNextRecordName(redcap_vars$rc_con) == 1) { 
      redcap_vars$rc_field_names %>% 
        select(.data$export_field_name, .data$choice_value) %>% 
        mutate(choice_value = map(.x = .data$choice_value, ~ NA)) %>% 
        pivot_wider(names_from = .data$export_field_name, values_from = .data$choice_value) %>% 
        flatten_dfr() %>% 
        tidyr::drop_na()
    ### Export existing Records, filtering to the subject in context  
    } else if (redcapAPI::exportNextRecordName(redcap_vars$rc_con) != 1 & redcap_vars$requires_reviewer == 'no' ) {
      redcapAPI::exportRecords(rcon = redcap_vars$rc_con, factors = F, labels = F ) %>% 
        as_tibble() %>% 
        mutate_all(as.character) %>% 
        mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
        filter(!!as.name(redcap_vars$identifier_field ) == subject_id() )
    ### Export existing Records, filtering to the subject AND reviewer in context  
    } else {
      redcapAPI::exportRecords(rcon = redcap_vars$rc_con, factors = F, labels = F ) %>% 
        as_tibble() %>% 
        mutate_all(as.character) %>% 
        mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
        filter(!!as.name(redcap_vars$identifier_field ) == subject_id() & !!as.name(redcap_vars$reviewer_field) == redcap_vars$reviewer )
    }
  })
  
  observeEvent(redcap_instrument$default_data, {
    req(redcap_vars$is_connected == 'yes', redcap_vars$is_configured == 'yes')

    redcap_instrument$previous_data <- if(nrow(redcap_instrument$default_data ) > 0 ){
      redcap_instrument$default_data %>%
        # Turn wide data from RedCAP to long, collapsing checkbox type quesitions along the way
        pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>%
        separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
        mutate(checkbox_value = map2_chr(.x = .data$checkbox_value, .y = .data$value_present, ~ case_when(.y == 0 ~ '',
                                                                                                          TRUE ~ .x)
        )
        ) %>%
        select(-.data$value_present) %>% # remove value presence variable
        pivot_wider(names_from = .data$checkbox_questions, values_from = .data$checkbox_value, values_fn = list(checkbox_value = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
        pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'default_value', values_transform = list(default_value = as.list), values_ptypes = list(default_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
    } else if(nrow(redcap_instrument$default_data ) == 0 & redcap_vars$requires_reviewer == 'no' ) {
      redcap_instrument$default_data %>%
        add_row(!!redcap_vars$identifier_field := subject_id() ) %>% # Add default data, without reviewer info, if present
        mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
        pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>%
        separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
        select(-.data$checkbox_value) %>% # remove checkbox value variable. Here, we know that nothing has been entered, so it is preferrable to end up with a blank character list
        pivot_wider(names_from = .data$checkbox_questions, values_from = .data$value_present, values_fn = list(value_present = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
        pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'default_value', values_transform = list(default_value = as.list), values_ptypes = list(default_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
    } else {
      redcap_instrument$default_data %>%
      add_row(!!redcap_vars$identifier_field := subject_id(), !!redcap_vars$reviewer_field := redcap_vars$reviewer ) %>% # Add default data, with reviewer info, if present
        mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
        pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>%
        separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
        select(-.data$checkbox_value) %>% # remove checkbox value variable. Here, we know that nothing has been entered, so it is preferrable to end up with a blank character list
        pivot_wider(names_from = .data$checkbox_questions, values_from = .data$value_present, values_fn = list(value_present = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
        pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'default_value', values_transform = list(default_value = as.list), values_ptypes = list(default_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
    }
  })
  ## Create a Shiny tagList for each question type present in the instrument
  rc_instrument_ui <- reactive({
    req(redcap_vars$is_connected == 'yes', redcap_vars$is_configured == 'yes', redcap_instrument$selected_instrument_meta)
    redcap_instrument$selected_instrument_meta %>%
      left_join(redcap_instrument$previous_data ) %>% #### add current subject info, if present, to the mix
      mutate( ## mutate shiny tags/inputs
        shiny_header = map(.data$section_header, h3),
        shiny_field_label = case_when(is.na(.data$required_field) ~ .data$field_label,
                                      TRUE ~ paste(.data$field_label,"<br/><font color='#FC0020'>* must provide value</font>")
        ),
        shiny_input = pmap(list(reviewr_type = .data$reviewr_redcap_widget_function,
                                field_name = ns(.data$shiny_inputID),
                                field_label = .data$shiny_field_label,
                                required = .data$required_field,
                                choices = .data$select_choices_or_calculations,
                                current_subject_data = .data$default_value ### Add this back
        ),
        render_redcap
        ),
        shiny_note = map(.data$field_note, tags$sub),
        shiny_taglist = pmap(list(.data$shiny_header,
                                  .data$shiny_input,
                                  .data$shiny_note
        ),
        tagList
        )
      )
  })
  ## Collect User Entered Instrument data ----
  redcap_module_inputs <- reactive({reactiveValuesToList(input)}) ### This collects all inputs in the module
  instrumentData <- reactive({
    tibble(inputID = names(redcap_module_inputs() ),
           values = unname(redcap_module_inputs() )
           ) %>%
      filter(inputID %in% redcap_instrument$selected_instrument_meta$shiny_inputID) ### Limit to only instrument inputs
  })
  
  ## Process User Entered Data for REDCap Upload ----
  # rc_uploadData <- reactive({
  #   req(rc_instrument(), instrumentData(), rc_id() )
  #   rc_recordID_field <- rc_recordID() %>% extract2(1)
  #   rc_instrument() %>% 
  #     mutate(shiny_inputID = .data$shiny_inputID) %>% ## Namespace
  #     select(.data$shiny_inputID, .data$field_name, .data$select_choices_or_calculations) %>% ## Include select_choices_or_calculations so that all columns can be sent back to REDCap. This allows for overwriting old data with blank ''
  #     add_row(field_name = rc_recordID() %>% flatten() %>% unlist()) %>% ## Add REDCap record ID field back into the instrument, so it can be joined with any previous data.
  #     left_join(instrumentData(), by = c('shiny_inputID' = 'inputID')) %>% ## Join the instrument inputs with the selected instrument. This ensures inputs are collected only for the active instrument
  #     modify_depth(2, as.character) %>% ## the input values are all lists at this moment. Dive into each list (depth = 2) and make sure that the values within the list are coded as characters
  #     separate_rows(.data$select_choices_or_calculations, sep = '\\|') %>% ## Expand select_choices_or_calculations
  #     mutate(select_choices_or_calculations = str_trim(.data$select_choices_or_calculations)) %>% ## Trim
  #     separate(.data$select_choices_or_calculations, into = c('rc_val','rc_label'), sep = ',') %>% ## Separate
  #     ## This mutate adds additional column names to hold values for checkbox questions
  #     mutate(rc_label = str_trim(.data$rc_label), ## Trim
  #            col_names = pmap(list(x = .data$shiny_inputID, y = .data$field_name, z = .data$rc_val ),  function(x,y,z) case_when(str_detect(string = x, pattern = 'reviewr_checkbox') ~ paste0(y, '___', z), ## Create additional column names for inputs where multiple inputs are allowed
  #                                                                                                                                TRUE ~ y)
  #            ),
  #            values = pmap(list(x = .data$shiny_inputID, y = .data$rc_val, z = .data$values), function(x,y,z) case_when(str_detect(string = x, pattern = 'reviewr_checkbox') & y == z ~ '1',
  #                                                                                                                       str_detect(string = x, pattern = 'reviewr_checkbox') & y != z ~ '',
  #                                                                                                                       TRUE ~ z)
  #            ),
  #            col_names = flatten_chr(.data$col_names)
  #     ) %>% 
  #     select(.data$col_names, .data$values) %>% 
  #     unnest(cols = .data$values, keep_empty = T) %>% ## in the case that all checkbox questions are de-selected, this keeps empty values, but stores them as NA. 
  #     ## This mutate modifys values to blanks, except for the special case when the record ID is NA. We would like to trop this value, if NA.
  #     mutate(values = map2_chr(.x = .data$col_names, .y = .data$values, ~ case_when(str_detect(string = .x,pattern = !!rc_recordID_field) & is.na(.y) ~ .y,
  #                                                                                   is.na(.y) ~ '',
  #                                                                                   TRUE ~ .y)
  #     )
  #     ) %>%  
  #     arrange(desc(.data$values)) %>% 
  #     distinct(.data$col_names,.keep_all = T) %>% 
  #     tidyr::drop_na() %>% 
  #     pivot_wider(names_from = .data$col_names, values_from = .data$values) %>% 
  #     bind_cols(rc_id(), rc_complete() ) %>% 
  #     select(!!rc_recordID_field, everything() ) %>% ## RedCAP API likes the record identifier in the first column
  #     flatten_dfr()
  # })
  ## REDCap Instrument UI Outputs ----
  output$instrument_select <- renderUI({ instrument_select() })
  output$instrument_ui <- renderUI({ rc_instrument_ui()$shiny_taglist })  
  
return(redcap_instrument)
}