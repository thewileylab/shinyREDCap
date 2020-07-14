# UI ----
#' REDCap Instrument UI
#'
#' @param id The module namespace
#'
#' @return A Shiny Representation of REDCap Instrument
#' @export
#' 
#' @importFrom shinyWidgets useShinydashboard useSweetAlert
#' @importFrom shinyjs useShinyjs
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#'
redcap_instrument_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinydashboard(),
    useShinyjs(),
    useSweetAlert(),
    shinydashboard::box(title = "REDCap Instrument",
                        width = '100%',
                        status = 'danger',
                        solidHeader = F,
                        uiOutput(ns('instrument_select')),
                        uiOutput(ns('instrument_ui')) %>% withSpinner(type = 5, color = '#e83a2f')
                        ),
    shinydashboard::box(title = 'Upload to REDCap',
                        width = '100%',
                        status = 'danger',
                        solidHeader = F,
                        uiOutput(ns('instrument_status_select')),
                        div(id = ns('instrument_status_select_div'),
                            selectizeInput(inputId = ns('survey_complete'),
                                           label = 'Instrument Status',
                                           choices = NULL
                                           )
                            ),
                        div(id = ns('redcap_upload_btn_div'),
                            actionButton(inputId = ns('upload'), label = 'Upload to REDCap')
                            )
                        )
    )
}

# Server ----
#' REDCap Instrument Server
#'
#' @param id The module namespace
#' @param redcap_vars Variables passed from the REDCap Setup module
#' @param subject_id The currently selected subject id 
#'
#' @return REDCap records from the currently selected project
#' @export
#' 
#' @importFrom dplyr arrange coalesce contains desc slice filter mutate mutate_if mutate_all left_join inner_join select as_tibble pull rename case_when distinct summarise group_by ungroup everything
#' @importFrom DT datatable
#' @importFrom glue glue
#' @importFrom httr config
#' @importFrom magrittr %>% 
#' @importFrom purrr flatten_chr flatten_dfr map map2 map2_chr pmap modify_depth
#' @importFrom redcapAPI exportRecords exportNextRecordName
#' @importFrom REDCapR redcap_write
#' @importFrom rlang .data :=
#' @importFrom snakecase to_sentence_case
#' @importFrom shinyjs hide show
#' @importFrom shinyWidgets confirmSweetAlert sendSweetAlert
#' @importFrom stringr str_to_lower str_detect
#' @importFrom tibble rownames_to_column add_row add_column tibble
#' @importFrom tidyr pivot_wider pivot_longer drop_na replace_na separate unnest
#'
redcap_instrument_server <- function(id, redcap_vars, subject_id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ## REDCap Instrument Values ----  
      redcap_instrument <- reactiveValues(
        selected_instrument_meta = NULL,
        selected_instrument_complete_field = NULL,
        previous_selected_instrument_complete_val = '',
        selected_instrument_meta_required = NULL,
        previous_data = NULL,
        previous_instrument_formatted_data = NULL,
        previous_instrument_formatted_data_labels = NULL,
        current_record_id = NULL,
        current_data = NULL,
        current_instrument_formatted_data = NULL,
        current_instrument_formatted_data_labels = NULL,
        data_comparison = NULL,
        overwrite_modal = NULL,
        upload_status = NULL
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
        req(input$rc_instrument_selection)
        redcap_instrument$selected_instrument_meta <- redcap_vars$rc_meta_data %>%
          slice(-1) %>%   # We drop the first row, as it most likely is the auto-increment field used in REDCap
          filter(str_to_lower(.data$form_name) == input$rc_instrument_selection ) %>% # Extract the instrument based on the user selection
          rownames_to_column() %>%
          filter(!.data$field_type %in% c('slider','calc','descriptive')) %>%
          # If some information is not defined within REDCap, it will convert those to logical types by default.  We are
          # assuming that they will be all character values, so we need to perform explicit casting to continue with that
          # assumption.
          mutate_if(is.logical, as.character) %>%
          left_join(shinyREDCap::redcap_widget_map,
                    by = c('field_type' = 'redcap_field_type', 'text_validation_type_or_show_slider_number' = 'redcap_field_val')
                    ) %>%
          # unite(col = 'shiny_inputID', .data$field_name, .data$reviewr_redcap_widget_function, sep = '_', remove = F) %>%
          mutate(section_header = coalesce(.data$section_header, ''),
                 field_note = coalesce(.data$field_note, '')
                 )
        redcap_instrument$selected_instrument_meta_required <- redcap_instrument$selected_instrument_meta %>% 
          select(.data$field_name, .data$required_field) %>% 
          filter(.data$required_field == 'y')
        redcap_instrument$selected_instrument_complete_field <- glue::glue('{input$rc_instrument_selection}_complete')
        })
      
      ## Retrieve Previous REDCap data ----
      observeEvent(c(subject_id(), redcap_vars$is_configured, redcap_instrument$upload_status), {
        req(redcap_vars$is_connected == 'yes', redcap_vars$is_configured == 'yes', subject_id())
        message('Refreshing instrument data from REDCap')
        # browser()
        ### Special case, when the REDCap Instrument has no previous data
        redcap_instrument$previous_data <- if(redcapAPI::exportNextRecordName(redcap_vars$rc_con) == 1) { 
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
        message('REDCap Refresh Complete')
        })
  
      ## REDCap Survey Complete ----
      observeEvent(c(input$rc_instrument_selection, redcap_instrument$previous_data, redcap_instrument$selected_instrument_complete_field), {
        req(input$rc_instrument_selection, redcap_instrument$previous_data, redcap_instrument$selected_instrument_complete_field)
        # browser()
        redcap_instrument$previous_selected_instrument_complete_val <- redcap_instrument$previous_data %>% 
          pull(redcap_instrument$selected_instrument_complete_field)
        updateSelectizeInput(session = session, 
                             inputId = 'survey_complete',
                             choices = shinyREDCap::redcap_survey_complete %>% deframe(),
                             selected = redcap_instrument$previous_selected_instrument_complete_val,
                             server = T,
                             options = list(create = FALSE,
                                            placeholder = 'Review Not Started'))
        })
      ## Format previous data to display appropriately in the Shiny representation of the REDCap Instrument ----
      observeEvent(redcap_instrument$previous_data, {
        req(redcap_vars$is_connected == 'yes', redcap_vars$is_configured == 'yes')
        
        redcap_instrument$previous_instrument_formatted_data <- if(nrow(redcap_instrument$previous_data ) > 0 ){
          if(ncol(redcap_instrument$previous_data %>% select(contains('___')) ) > 0 ) {
            redcap_instrument$previous_data %>%
              # Turn wide data from RedCAP to long, collapsing checkbox type questions along the way
              pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>%
              separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
              mutate(checkbox_value = map2_chr(.x = .data$checkbox_value, .y = .data$value_present, ~ case_when(.y == 0 ~ '',
                                                                                                                TRUE ~ .x)
                                               )
                     ) %>%
              select(-.data$value_present) %>% # remove value presence variable
              pivot_wider(names_from = .data$checkbox_questions, values_from = .data$checkbox_value, values_fn = list(checkbox_value = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
              pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'previous_value', values_transform = list(previous_value = as.list), values_ptypes = list(previous_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
            } else {
              redcap_instrument$previous_data %>%
                pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'previous_value', values_transform = list(previous_value = as.list), values_ptypes = list(previous_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
              }
          } else if(nrow(redcap_instrument$previous_data ) == 0 & redcap_vars$requires_reviewer == 'no' ) {
            if(ncol(redcap_instrument$previous_data %>% select(contains('___')) ) > 0 ) {
              redcap_instrument$previous_data %>%
                add_row(!!redcap_vars$identifier_field := subject_id() ) %>% # Add default data, without reviewer info, if present
                mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
                pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>%
                separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
                select(-.data$checkbox_value) %>% # remove checkbox value variable. Here, we know that nothing has been entered, so it is preferrable to end up with a blank character list
                pivot_wider(names_from = .data$checkbox_questions, values_from = .data$value_present, values_fn = list(value_present = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
                pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'previous_value', values_transform = list(previous_value = as.list), values_ptypes = list(previous_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
              } else {
                redcap_instrument$previous_data %>%
                  add_row(!!redcap_vars$identifier_field := subject_id() ) %>% 
                  pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'previous_value', values_transform = list(previous_value = as.list), values_ptypes = list(previous_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
                }
            } else {
              if(ncol(redcap_instrument$previous_data %>% select(contains('___')) ) > 0 ) {
                redcap_instrument$previous_data %>%
                  add_row(!!redcap_vars$identifier_field := subject_id(), !!redcap_vars$reviewer_field := redcap_vars$reviewer ) %>% # Add default data, with reviewer info, if present
                  mutate_all(replace_na, replace = '') %>% # replace all NA values with blank character vectors, so that shiny radio buttons without a previous response will display empty
                  pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>%
                  separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name  
                  select(-.data$checkbox_value) %>% # remove checkbox value variable. Here, we know that nothing has been entered, so it is preferrable to end up with a blank character list
                  pivot_wider(names_from = .data$checkbox_questions, values_from = .data$value_present, values_fn = list(value_present = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
                  pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'previous_value', values_transform = list(previous_value = as.list), values_ptypes = list(previous_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
                } else {
                  redcap_instrument$previous_data %>%
                    add_row(!!redcap_vars$identifier_field := subject_id(), !!redcap_vars$reviewer_field := redcap_vars$reviewer ) %>% 
                    pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'previous_value', values_transform = list(previous_value = as.list), values_ptypes = list(previous_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
                }
              }
        })
      
      ### Determine the REDCap Record ID. If entering new data, generate a new REDCap record id. ----
      observeEvent(redcap_instrument$previous_instrument_formatted_data, {
        # browser()
        temp_redcap_record_id <- redcap_instrument$previous_instrument_formatted_data %>% 
          filter(.data$field_name == redcap_vars$rc_record_id_field) %>% 
          rename(inputID = .data$field_name, current_value = .data$previous_value)
        redcap_instrument$current_record_id <- if(temp_redcap_record_id %>% unnest(.data$current_value) %>% pull(.data$current_value) == '') {
          tibble(inputID = redcap_vars$rc_record_id_field,
                 current_value = list(redcapAPI::exportNextRecordName(redcap_vars$rc_con))
                 )
          } else {
            temp_redcap_record_id
            }
        })
      ## Create REDCap Instrument ---- 
      ### Create a Shiny tagList for each question type present in the instrument
      rc_instrument_ui <- reactive({
        req(redcap_vars$is_connected == 'yes', redcap_vars$is_configured == 'yes', redcap_instrument$selected_instrument_meta)
        redcap_instrument$selected_instrument_meta %>%
          left_join(redcap_instrument$previous_instrument_formatted_data ) %>% #### add current subject info, if present, to the mix
          mutate( ## mutate shiny tags/inputs
            shiny_header = map(.data$section_header, h3),
            shiny_field_label = case_when(is.na(.data$required_field) ~ .data$field_label,
                                          TRUE ~ paste(.data$field_label,"<br/><font color='#FC0020'>* must provide value</font>")
                                          ),
            shiny_input = pmap(list(reviewr_type = .data$reviewr_redcap_widget_function,
                                    field_name = ns(.data$field_name),
                                    field_label = .data$shiny_field_label,
                                    required = .data$required_field,
                                    choices = .data$select_choices_or_calculations,
                                    current_subject_data = .data$previous_value 
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
      
       ## Collect/Process User Entered Instrument data ----
      redcap_module_inputs <- reactive({reactiveValuesToList(input)}) ### This collects all inputs in the module
      instrumentData <- reactive({
        req(redcap_module_inputs(), redcap_instrument$current_record_id)
        tibble(inputID = names(redcap_module_inputs() ),
               current_value = unname(redcap_module_inputs() )
               ) %>%
          filter(.data$inputID %in% redcap_instrument$selected_instrument_meta$field_name) %>%  ### Limit to only instrument inputs
          add_row(redcap_instrument$current_record_id) ### Add REDCap Record id
        })
      
      ## Process User Entered Data for REDCap Upload ----
      observeEvent(c(instrumentData(), input$survey_complete), {
        # browser()
        req(redcap_vars$is_connected == 'yes', redcap_vars$is_configured == 'yes', redcap_instrument$selected_instrument_meta) ### Wait for more instrument selection.
        redcap_instrument$current_data <- redcap_instrument$selected_instrument_meta %>%
          select(.data$reviewr_redcap_widget_function, .data$field_name, .data$select_choices_or_calculations) %>% ## Include select_choices_or_calculations so that all columns can be sent back to REDCap. This allows for overwriting old data with blank ''
          add_row(field_name = redcap_vars$rc_record_id_field) %>% ## Add REDCap record ID field back into the instrument, so it can be joined with any previous data.
          left_join(instrumentData(), by = c('field_name' = 'inputID')) %>% ## Join the instrument inputs with the selected instrument. This ensures inputs are collected only for the active instrument
          modify_depth(2, as.character) %>% ## the input values are all lists at this moment. Dive into each list (depth = 2) and make sure that the values within the list are coded as characters
          separate_rows(.data$select_choices_or_calculations, sep = '\\|') %>% ## Expand select_choices_or_calculations
          mutate(select_choices_or_calculations = str_trim(.data$select_choices_or_calculations)) %>% ## Trim
          separate(.data$select_choices_or_calculations, into = c('rc_val','rc_label'), sep = ',') %>% ## Separate
          ## This mutate adds additional column names to hold values for checkbox questions
          mutate(rc_label = str_trim(.data$rc_label), ## Trim
                 inputID = pmap(list(x = .data$reviewr_redcap_widget_function, y = .data$field_name, z = .data$rc_val ),  function(x,y,z) case_when(str_detect(string = x, pattern = 'reviewr_checkbox') ~ paste0(y, '___', z), ## Create additional column names for inputs where multiple inputs are allowed
                                                                                                                                                    TRUE ~ y)
                                ),
                 current_value = pmap(list(x = .data$reviewr_redcap_widget_function, y = .data$rc_val, z = .data$current_value), function(x,y,z) case_when(str_detect(string = x, pattern = 'reviewr_checkbox') & y == z ~ '1',
                                                                                                                                                           str_detect(string = x, pattern = 'reviewr_checkbox') & y != z ~ '',
                                                                                                                                                           TRUE ~ z)
                                      ),
                 inputID = flatten_chr(.data$inputID)
                 ) %>%
          select(.data$inputID, .data$current_value) %>%
          unnest(cols = .data$current_value, keep_empty = T) %>% ## in the case that all checkbox questions are de-selected, this keeps empty values, but stores them as NA.
          ## This mutate modifys values to blanks, except for the special case when the record ID is NA. We would like to trop this value, if NA.
          mutate(current_value = map2_chr(.x = .data$inputID, .y = .data$current_value, ~ case_when(str_detect(string = .x,pattern = !!redcap_vars$rc_record_id_field) & is.na(.y) ~ .y,
                                                                                                    is.na(.y) ~ '',
                                                                                                    TRUE ~ .y)
                                          )
                 ) %>%
          arrange(desc(.data$current_value)) %>%
          distinct(.data$inputID,.keep_all = T) %>%
          tidyr::drop_na() %>%
          pivot_wider(names_from = .data$inputID, values_from = .data$current_value) %>%
          select(!!redcap_vars$rc_record_id_field, everything() ) %>% ## RedCAP API likes the record identifier in the first column
          flatten_dfr() %>% 
          ## Add Instrument complete value
          add_column(!!redcap_instrument$selected_instrument_complete_field := input$survey_complete)
        })
      
      ## Determine Changes from previously entered data ----
      observeEvent(c(redcap_instrument$current_data, input$survey_complete), {
        # browser()
        req(input$survey_complete)
        redcap_instrument$current_instrument_formatted_data <- if(ncol(redcap_instrument$current_data %>% select(contains('___')) ) > 0 ) {
          redcap_instrument$current_data %>%
            # Turn wide data from RedCAP to long, collapsing checkbox type questions along the way
            pivot_longer(cols = contains('___'),names_to = 'checkbox_questions',values_to = 'value_present') %>%
            arrange(.data$checkbox_questions) %>% ## Ensure that order is correct so that list variables will have responses in the correct place.
            separate(.data$checkbox_questions, into = c('checkbox_questions','checkbox_value'), sep = '___') %>% # Separate value from column name
            mutate(checkbox_value = map2_chr(.x = .data$checkbox_value, .y = .data$value_present, ~ case_when(.y == '' ~ '',
                                                                                                              TRUE ~ .x)
                                             )
                   ) %>%
            select(-.data$value_present) %>% # remove value presence variable
            pivot_wider(names_from = .data$checkbox_questions, values_from = .data$checkbox_value, values_fn = list(checkbox_value = list)) %>% # pivot wider, utilizing list to preserve column types. Having collapsed the checkbox quesions, we now have a the original field_name as a joinable variable
            pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'current_value', values_transform = list(current_value = as.list), values_ptypes = list(current_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
          } else {
            redcap_instrument$current_data %>% 
              pivot_longer(cols = everything(), names_to = 'field_name', values_to = 'current_value', values_transform = list(current_value = as.list), values_ptypes = list(current_value = list())) # Pivot longer, utilizing a list as the column type to avoid variable coercion
            } 
        })
      
      observeEvent(c(redcap_instrument$previous_instrument_formatted_data, redcap_instrument$current_instrument_formatted_data), {
        # browser()
        req(redcap_instrument$previous_instrument_formatted_data, redcap_instrument$current_instrument_formatted_data)
        ### Add labels to previous data
        redcap_instrument$previous_instrument_formatted_data_labels <- redcap_instrument$previous_instrument_formatted_data %>%
          unnest(.data$previous_value) %>%
          ### This column allows us to determine if no previous data has been entered (New REDCap Record).
          mutate(is_empty = case_when(.data$previous_value == '' ~ 1,
                                      TRUE ~ 0)
                 ) %>% 
          left_join(redcap_vars$rc_meta_exploded,
                    by = c('field_name' = 'field_name', 'previous_value' = 'value')
                    ) %>% 
          mutate(previous_value_label = case_when(is.na(.data$value_label) ~ .data$previous_value,
                                                  TRUE ~ .data$value_label
                                                  )
                 ) %>% 
          select(-.data$field_type, -.data$value_label) %>% 
          group_by(.data$field_name) %>% 
          summarise(previous_value = paste(.data$previous_value, collapse = ','),
                    is_empty = min(.data$is_empty, na.rm = T),
                    previous_html = paste(.data$previous_value_label, collapse = '<br><br>'),
                    .groups = 'keep'
                    ) %>%
          distinct(.data$previous_html, .keep_all = T)
        
        ### Add labels to current data  
        redcap_instrument$current_instrument_formatted_data_labels <- redcap_instrument$current_instrument_formatted_data %>%
          unnest(.data$current_value) %>%
          left_join(redcap_vars$rc_meta_exploded,
                    by = c('field_name' = 'field_name', 'current_value' = 'value')
                    ) %>% 
          mutate(current_value_label = case_when(is.na(.data$value_label) ~ .data$current_value,
                                                 TRUE ~ .data$value_label
                                                 )
                 ) %>% 
          select(-.data$field_type, -.data$value_label) %>% 
          group_by(.data$field_name) %>% 
          mutate(current_value = paste(.data$current_value, collapse = ','),
                 current_html = paste(.data$current_value_label, collapse = '<br><br>')) %>%
          distinct(.data$current_html, .keep_all = T)
        
        ### Combine previous and current data to determine what, if anything, has changed   
        redcap_instrument$data_comparison <- redcap_instrument$previous_instrument_formatted_data_labels %>% 
          inner_join(redcap_instrument$current_instrument_formatted_data_labels, by = c('field_name' = 'field_name')) %>% 
          mutate(diff = case_when(.data$previous_value != .data$current_value ~ T,
                                  TRUE ~ F
                                  )
                 ) %>% 
          filter(.data$field_name != redcap_vars$rc_record_id_field & diff == TRUE) ## This will be different when entering new data
        
        ### Create modal for displaying changes
        redcap_instrument$overwrite_modal <- redcap_instrument$data_comparison %>% 
          ungroup() %>% 
          left_join(redcap_instrument$selected_instrument_meta %>% select(.data$field_name, .data$field_label)) %>% 
          select('Question' = .data$field_label, 'Previous Value' = .data$previous_html, 'Current Value' = .data$current_html) %>%
          ## Here, we don't have a good existing data structure to label the instrument complete field, so we do some work.
          mutate(Question = case_when(is.na(Question) ~ redcap_instrument$selected_instrument_complete_field %>% snakecase::to_sentence_case(),
                                      TRUE ~ Question
                                      )
                 ) %>% 
          DT::datatable(
            extensions = list('Scroller' = NULL),
            options = list(scrollX = TRUE,
                           deferRender = TRUE,
                           scrollY = '450px',
                           scroller = TRUE,
                           sDom  = '<"top">lrt<"bottom">ip'
                           ),
            rownames = F, 
            escape = F,
            class = 'cell-border strip hover'
            )
        })
      
      ## Display the REDCap Upload button if user has changed values ----
      shinyjs::hide('redcap_upload_btn_div') ### Start hidden
      observeEvent(redcap_instrument$data_comparison, {
        # browser()
        req(redcap_instrument$data_comparison)
        if(nrow(redcap_instrument$data_comparison) > 0) {
          shinyjs::show('redcap_upload_btn_div')
          } else {
            shinyjs::hide('redcap_upload_btn_div')
            }
        })
      
      ## Check to see if all required questions have been answered ----
      observeEvent(c(redcap_instrument$selected_instrument_meta_required, redcap_instrument$current_instrument_formatted_data), {
        # browser()
        req(redcap_instrument$current_instrument_formatted_data_labels)
        redcap_instrument$qty_required <- nrow(redcap_instrument$selected_instrument_meta_required)
        redcap_instrument$qty_required_answered <- suppressWarnings(redcap_instrument$current_instrument_formatted_data %>% 
                                                                      left_join(redcap_instrument$selected_instrument_meta_required) %>% 
                                                                      filter(.data$required_field == 'y') %>%
                                                                      unnest(.data$current_value) %>% 
                                                                      mutate(answered = case_when(.data$current_value != '' ~ 1,
                                                                                                  TRUE ~ 0)
                                                                             ) %>% 
                                                                      group_by(.data$field_name) %>% 
                                                                      summarise(answered = max(.data$answered,na.rm = T),.groups = 'drop') %>% 
                                                                      filter(.data$answered > 0 ) %>% 
                                                                      nrow()
                                                                    )
        })
      shinyjs::hide('instrument_status_select_div')
      
      observeEvent(c(redcap_instrument$qty_required, redcap_instrument$qty_required_answered), {
        req(redcap_instrument$qty_required, redcap_instrument$qty_required_answered)
        if(redcap_instrument$qty_required == redcap_instrument$qty_required_answered) {
          shinyjs::show('instrument_status_select_div')
          } else {
            shinyjs::hide('instrument_status_select_div')
            }
        })
      
      ## Upload Data to REDCap ----
      ### Here, we decide what to do. 
      observeEvent(input$upload, {
        # browser() ### Pause before upload. Evaluate your life choices up until this point.
        overwrite_existing <- redcap_instrument$data_comparison %>% 
          filter(.data$is_empty != 1) %>% ### if the previous data is empty (0), nothing is overwritten. Just new abstraction data!
          nrow()
        if(overwrite_existing > 0) {
          ### Are we overwriting existing REDCap data? Notify the user, else upload to redcap
          confirmSweetAlert(
            session = session,
            inputId = ns('confirm_overwrite'),
            title = 'Warning! Overwriting existing REDCap data:',
            text = DT::dataTableOutput(ns('redcap_overwrite')),
            type = "warning",
            btn_labels = c("Cancel", "Upload to REDCap"),
            btn_colors = NULL,
            closeOnClickOutside = FALSE,
            showCloseButton = FALSE,
            html = TRUE
            )
          } else {
            message('Uploading abstraction data to REDCap')
            ## WIP Add instrument complete status to uploadData
            rc_uploadData <- redcap_instrument$current_data %>%
              # select(redcap_vars$rc_record_id_field, contains(redcap_instrument$data_comparison$field_name)) %>% ### Only upload fields that have changed.
              ### Only upload non-empty data. REDCap hates empty data. Turn empty to NA to 'reset' in REDCap
              pivot_longer(cols = everything(),
                           names_to = 'field_name',
                           values_to = 'value'
                           ) %>% 
              mutate(value = case_when(.data$value == '' ~ NA_character_,
                                       TRUE ~ .data$value)
                     ) %>% 
              pivot_wider(names_from = .data$field_name, values_from = .data$value)
            ## Check whether all required inputs are answered. If so, upload data as is. If not, change status to incomplete.
            if(redcap_instrument$qty_required == redcap_instrument$qty_required_answered) {
              redcap_instrument$upload_data <- rc_uploadData
              } else {
                redcap_instrument$upload_data <- rc_uploadData %>% 
                  mutate(!!redcap_instrument$selected_instrument_complete_field := 0)
                }
            redcap_instrument$upload_status <- NULL ## Clear previous upload status, then upload new data
            # redcap_instrument$upload_status <- redcapAPI::importRecords(rcon = redcap_vars$rc_con, data = rc_uploadData, overwriteBehavior = 'overwrite', returnContent = 'ids' )
            redcap_instrument$upload_status <- REDCapR::redcap_write(ds_to_write = redcap_instrument$upload_data, 
                                                                     redcap_uri = redcap_vars$rc_con$url,
                                                                     token = redcap_vars$rc_con$token,
                                                                     verbose = F,
                                                                     config_options = httr::config( ssl_verifypeer = 1L )
                                                                     )
            upload_message <- paste('REDCap', redcap_vars$rc_record_id_label, redcap_instrument$upload_status$affected_ids, 'uploaded successfully.')
            sendSweetAlert(
              session = session,
              title = "Success!!",
              text = upload_message,
              btn_labels = NA,
              type = "success"
            )
            }
        })
      
      ### Overwrite confirmation confirmed, write to REDCap, else don't
      observeEvent(input$confirm_overwrite, {
        if(input$confirm_overwrite == TRUE) {
          message('Overwriting existing abstraction data in REDCap')
          ### WIP Add instrument complete status
          rc_overwriteData <- redcap_instrument$current_data %>% 
            # select(redcap_vars$rc_record_id_field, redcap_instrument$data_comparison$field_name) %>% ### Only upload fields that have changed.
            ### Only upload non-empty data. REDCap hates empty data. Turn empty to NA to 'reset' in REDCap
            pivot_longer(cols = everything(),
                         names_to = 'field_name',
                         values_to = 'value'
                         ) %>% 
            mutate(value = case_when(.data$value == '' ~ NA_character_,
                                     TRUE ~ .data$value)
                   ) %>% 
            pivot_wider(names_from = .data$field_name, values_from = .data$value)
          ## Check whether all required inputs are answered. If so, upload data as is. If not, change status to incomplete.
          if(redcap_instrument$qty_required == redcap_instrument$qty_required_answered) {
            redcap_instrument$overwrite_data <- rc_overwriteData
            } else {
              redcap_instrument$overwrite_data <- rc_overwriteData %>% 
                mutate(!!redcap_instrument$selected_instrument_complete_field := 0)
              }
          redcap_instrument$upload_status <- NULL ## Clear previous upload status, then upload new data
          # redcap_instrument$upload_status <- redcapAPI::importRecords(rcon = redcap_vars$rc_con, data = rc_overwriteData, overwriteBehavior = 'overwrite', returnContent = 'ids' )
          redcap_instrument$upload_status <- REDCapR::redcap_write(ds_to_write = redcap_instrument$overwrite_data, 
                                                                   redcap_uri = redcap_vars$rc_con$url,
                                                                   token = redcap_vars$rc_con$token,
                                                                   verbose = F, 
                                                                   config_options = httr::config( ssl_verifypeer = 1L )
                                                                   )
          overwrite_message <- paste('REDCap', redcap_vars$rc_record_id_label, redcap_instrument$upload_status$affected_ids, 'modified successfully.')
          sendSweetAlert(
            session = session,
            title = "Success!!",
            text = overwrite_message,
            btn_labels = NA,
            type = "info"
            )
          } else {
            message('Canceled upload.')
            }
        })
      
      ## REDCap Instrument UI Outputs ----
      output$instrument_select <- renderUI({ instrument_select() })
      output$instrument_ui <- renderUI({ rc_instrument_ui()$shiny_taglist })
      output$redcap_overwrite <- DT::renderDataTable({ redcap_instrument$overwrite_modal })
      
      return(redcap_instrument)
      }
  )
}
