#' TabVisu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import plotly
mod_TabVisu_ui <- function(id){
  ns <- NS(id)
  tabItem(tabName="visu",
          sidebarPanel("",
                       pickerInput("plot_type","choix du graphique:",choices = c("histogram","barplot","boxplot"),multiple = TRUE),
                       pickerInput("variables","choix des variables",choices= c("Région","Maison Mere","Site","Tonnages","rotations","type de stockage"),multiple = TRUE)
          ),
          mainPanel(h1("Graphique"),
                    plotlyOutput("plotly")
          )
  )
}

#' TabVisu Server Functions
#'
#' @noRd
mod_TabVisu_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_TabVisu_ui("TabVisu_1")

## To be copied in the server
# mod_TabVisu_server("TabVisu_1")
