#' dbSidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinydashboard
mod_dbSidebar_ui <- function(id){
  ns <- NS(id)
  dashboardSidebar(
    sidebarMenu(
      menuItem("DATA", tabName = "data"),
      menuItem("Partie Random", tabName = "rand"),
      menuItem("Statistiques ", tabName = "analyse", startExpanded = FALSE, menuName = "Analyse",
               menuSubItem("Résumés statistiques", tabName = "resume"),
               menuSubItem("Visualisations", tabName = "visu")
      )
    )
  )
}

#' dbSidebar Server Functions
#'
#' @noRd
mod_dbSidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_dbSidebar_ui("dbSidebar_1")

## To be copied in the server
# mod_dbSidebar_server("dbSidebar_1")
