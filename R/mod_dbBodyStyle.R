#' dbBodyStyle UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dbBodyStyle_ui <- function(id){
  ns <- NS(id)
  tags$style(HTML("
      .content-wrapper {
        height: 100vh; /* Ajuste la hauteur à 100% de la hauteur de la vue du navigateur */
        overflow-y: auto; /* Active la barre de défilement verticale si nécessaire */
      }
      "))
}

#' dbBodyStyle Server Functions
#'
#' @noRd
mod_dbBodyStyle_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_dbBodyStyle_ui("dbBodyStyle_1")

## To be copied in the server
# mod_dbBodyStyle_server("dbBodyStyle_1")
