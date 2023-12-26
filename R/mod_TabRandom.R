#' TabRandom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyjs
mod_TabRandom_ui <- function(id){
  ns <- NS(id)
  tabItem(tabName="rand",
          sidebarPanel(style = "width: 150px;",
                       # boutton guide
                       actionBttn(inputId = "guide2",label = "Guide", style = "stretch",color = "primary"),
                       #prettyRadioButtons(
                       # inputId = "ref",label ="voulez vous un site de référence?", choices = c("Oui", "Non"),
                       #icon = icon("check"), bigger = TRUE,status = "info",animation = "jelly"
                       #),
                       #conditionalPanel(
                       # condition = "input.ref == 'Oui'",
                       #pickerInput(inputId = "siteRef",label = "Choix du site:", choices = c("Site 1", "Site 2", "Site 3", "Site 4"),
                       #           options = list(title = "Sites"),inline = TRUE)
                       #),


                       # boutton de tirage
                       actionBttn(inputId = ns("randall"),label = "Piocher aléatoirement :", style = "unite",size = "xs",color = "royal")
          ),
          mainPanel(h1("Sites:"),shinyjs::useShinyjs(),
                    # cadres
                    h3(style = "margin-top: 0;", "Classe 1:"),
                    mod_divClasse_ui("cadre1"),
                    h3(style = "margin-top: 0;", "Classe 2:"),
                    mod_divClasse_ui("cadre2"),
                    h3(style = "margin-top: 0;", "Classe 3:"),
                    mod_divClasse_ui("cadre3"),
                    h3(style = "margin-top: 0;", "Classe 4:"),
                    mod_divClasse_ui("cadre4"),
                    h3(style = "margin-top: 0;", "Classe 5:"),
                    mod_divClasse_ui("cadre5")

          )

  )
}

#' TabRandom Server Functions
#'
#' @noRd
mod_TabRandom_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # tout ce qui est relatif aux cadres est dans un module à part nomme div_classe
    observe({
      # on stocke la valeur du popup confirmation pour l'utiliser dans le module divClasse
      r$random <- input$myconfirmation2
    })

    # création d'un popup confirmation lorsque l'on appui sur le bouton
    observeEvent(input$randall,{
      confirmSweetAlert(
        session = session, inputId = "myconfirmation2", type = "info",
        title = "Etes vous sur de vouloir réaliser un tirage des sites?", danger_mode = TRUE
      )

    })








  })
}

## To be copied in the UI
# mod_TabRandom_ui("TabRandom_1")

## To be copied in the server
# mod_TabRandom_server("TabRandom_1")
