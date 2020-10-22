## code to prepare `DATASET` dataset goes here

library(tibble)
redcap_field_type <- c('text','text','text','dropdown','truefalse','yesno','radio','checkbox','notes')
redcap_field_val <- c(NA,'date_mdy','integer',NA,NA,NA,NA,NA,NA)
shinyREDCap_widget_function <- c('shinyREDCap_text','shinyREDCap_date','shinyREDCap_integer','shinyREDCap_dropdown','shinyREDCap_truefalse','shinyREDCap_yesno','shinyREDCap_radio','shinyREDCap_checkbox','shinyREDCap_notes')
redcap_widget_map <- tibble(redcap_field_type, redcap_field_val, shinyREDCap_widget_function)
usethis::use_data(redcap_widget_map, overwrite = T)

# REDCap survey complete choices
redcap_survey_complete_names <- c('Incomplete', 'Unverified', 'Complete')
redcap_survey_complete_values <- c(0,1,2)
names(redcap_survey_complete_values) <-redcap_survey_complete_names
redcap_survey_complete <- tibble(redcap_survey_complete_names, redcap_survey_complete_values)
usethis::use_data(redcap_survey_complete)

