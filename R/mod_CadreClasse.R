#' CadreClasse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_CadreClasse_ui <- function(id,nb){
  ns <- NS(id)
  div(id = ns("cadre1"),style = "padding:4px; border:4px solid #e0e0e0;",
      div(style = "padding:3px; background-color:#D7EAB6;",
          shinyjs::hidden(
            mod_SubCadreClasse_ui(paste0("SubCadreClasse_",nb))
          ),
          h3(style = "margin-top: 0;", "Classe 1:"),
          textOutput(paste0("site",nb)),
          shinyjs::hidden(
            pickerInput(paste0("choix",nb), "Choix d'un autre site:", choices = NULL,
                        options = list(title= "Sites: "), multiple = TRUE)
          ),
          prettySwitch(
            inputId = paste0("Id",nb),
            label = "garder le site",
            status = "success",
            value = FALSE,
            fill = TRUE
          )
      )
  )

}

#' CadreClasse Server Functions
#'
#' @noRd
mod_CadreClasse_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_CadreClasse_ui("CadreClasse_1")

## To be copied in the server
# mod_CadreClasse_server("CadreClasse_1")
