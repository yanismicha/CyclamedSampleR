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
      menuItem(" Accueil",tabName = "accueil",icon = icon("house")),
      menuItem("Les données", tabName = "data",icon = icon("table")),
      menuItem("Séléction des sites", tabName = "rand",icon = icon("shuffle")),
      menuItem("Statistiques ", tabName = "analyse", startExpanded = FALSE, menuName = "Analyse",icon = icon("chart-line"),
               menuSubItem("Résumés statistiques", tabName = "resume",icon = icon("dashboard")),
               menuSubItem("Tableau de bord", tabName = "visu",icon = icon("square-poll-horizontal"))
      )
    ),
    div(
      textOutput(ns("clock")),
      style = "position: absolute; bottom: 40px; width: 100%; text-align: center;"
    )
  )

}

#' dbSidebar Server Functions
#'
#' @noRd
mod_dbSidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$clock <- renderText({
      invalidateLater(1000)  # Mettre à jour toutes les secondes
      format(Sys.time(), "%H:%M:%S")  # Formater l'heure
    })
  })
}

## To be copied in the UI
# mod_dbSidebar_ui("dbSidebar_1")

## To be copied in the server
# mod_dbSidebar_server("dbSidebar_1")
