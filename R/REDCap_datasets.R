# Datasets ----
#' REDCap Survey Complete
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
"redcap_survey_complete"

#' REDCap Widget Map
#'
#' A dataset that maps REDCap question types and common validations
#'  to native shiny widgets through custom functions. 
#'
#' @docType data
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{redcap_field_type}{A REDCap Question Type}
#'   \item{redcap_field_val}{Custom REDCap Question Type Validation}
#'   \item{reviewr_redcap_widget_function}{ReviewR function to use when mapping to native Shiny widget}
#'   ...
#' }
"redcap_widget_map"