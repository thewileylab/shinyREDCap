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
#' @importFrom httr config
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
