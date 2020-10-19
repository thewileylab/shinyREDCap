
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyREDCap

<!-- badges: start -->

<!-- badges: end -->

The goal of shinyREDCap is to allow you to interact with your REDCap
Projects from within a Shiny application. REDCap instruments are
translated into native Shiny controls/widgets and allow for the capture
of abstracted information from within the R Shiny environment.
Additionally, error prone fields such as MRN and reviewer information
are populated automatically, based on user configured information, thus
reducing the potential for error in abstracted information.

## Installation

You can install the released version of shinyREDCap from
[GitHub](https://github.com/thewileylab/shinyREDCap) with:

``` r
# install.packages("devtools")
devtools::install_github("thewileylab/shinyREDCap")
```

## Usage

To connect shinyREDCap to your REDCap Instruments, users must have read
and write access to a previously created REDCap project. Additionally,
users must request an API key.

To get a feel for the modules, you may run the demo application:

``` r
shinyREDCap::run_app()
```

To integrate shinyREDCap into your own Shiny application place
`redcap_setup_ui()`, `redcap_instrument_ui()` and corresponding server
modules into the application’s ui and server functions as in the example
below:

``` r
library(shiny)
library(shinyREDCap)
ui <- fluidPage(
    column(width = 6,
           tags$h2("Setup REDCap Instrument"),
           # Call the setup UI function
           redcap_setup_ui(id = 'redcap_namespace')
           ),
    column(width = 6,
           tags$h2("Interact with REDCap Project Instruments"),
           tags$h3("Select a patient from the list"),
           # Create a subject ID selector. This can come from anywhere, 
           selectInput(inputId = 'subject_id',label = 'Subject ID',choices = c('922873','922874', '922875','922876','922877','922878')),
           # Call the redcap instrument UI function
           redcap_instrument_ui(id = 'redcap_namespace')
           )
  )

server <- function(input, output, session) {
  # Encapsulate the subject ID selector as a reactive
  subject_id <- reactive({ input$subject_id }) ## Pass to instrument function
  # Call the REDCap server function
  instrument_vars <- redcap_server(id = 'redcap_namespace', subject_id = subject_id )
}

if (interactive())
  shinyApp(ui = ui, server = server)
```

## Disclaimer

This is a work in progress and thus there are no guarantees of
functionality or accuracy. Use at your own risk.

## Getting help

If you encounter bugs, errors, issues or other general unpleasantness,
please let us know on
[GitHub](https://github.com/thewileylab/shinyREDCap/issues).

## Code of Conduct

Please note that the shinyREDCap project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
