#' SubCadreClasse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SubCadreClasse_ui <- function(id){
  ns <- NS(id)
  div(id =ns("compacteur1"),style = "margin-top: 10px; padding:5px;float: right;width: 140px; background-color:#ffaca3; border:2px solid #ff3924; -moz-border-radius:9px; -khtml-border-radius:9px; -webkit-border-radius:9px; border-radius:9px;",
      h3(style = "margin-top: 0; color: #ff3924; float:right;",
         "Compacteur"
      )
  )
}

#' SubCadreClasse Server Functions
#'
#' @noRd
mod_SubCadreClasse_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_SubCadreClasse_ui("SubCadreClasse_1")

## To be copied in the server
# mod_SubCadreClasse_server("SubCadreClasse_1")
