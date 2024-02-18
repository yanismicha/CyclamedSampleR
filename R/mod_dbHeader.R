#' dbHeader UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinydashboard
mod_dbHeader_ui <- function(id){
  ns <- NS(id)
  dashboardHeader(title = "Cyclamed",
                  tags$li(a(href = 'https://www.cyclamed.org/',
                            img(src = "www/logo.gif", height = 30, width = 30,
                              title = ""),
                            style = "padding-top:10px; padding-bottom:10px;"),
                            class = "dropdown")
                  )
}

#' dbHeader Server Functions
#'
#' @noRd
mod_dbHeader_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_dbHeader_ui("dbHeader_1")

## To be copied in the server
# mod_dbHeader_server("dbHeader_1")
