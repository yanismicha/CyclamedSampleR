#' dbBody UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyjs
#' @import reactable
#' @import shinyWidgets
#' @import plotly

mod_dbBody_ui <- function(id){
  ns <- NS(id)
  dashboardBody(mod_dbBodyStyle_ui("dbBodyStyle_1"), #style du corps de l'interface
      tabItems( # les différents onglets
        mod_TabData_ui("TabData_1"),
        mod_TabSummary_ui("TabSummary_1"),
        mod_TabVisu_ui("TabVisu_1"),
        mod_TabRandom_ui("TabRandom_1")
      )
  )
}

#' dbBody Server Functions
#'
#' @noRd
mod_dbBody_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_dbBody_ui("dbBody_1")

## To be copied in the server
# mod_dbBody_server("dbBody_1")
