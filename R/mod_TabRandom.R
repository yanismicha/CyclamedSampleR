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
                    tags$h3(
                      HTML("Classe 1: <span id='info_icon1' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                    ),
                    mod_divClasse_ui("cadre1"),
                    tags$h3(
                      HTML("Classe 2: <span id='info_icon2' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                    ),
                    #h3(style = "margin-top: 0;", "Classe 2:"),
                    mod_divClasse_ui("cadre2"),
                    tags$h3(
                      HTML("Classe 3: <span id='info_icon3' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                    ),
                    #h3(style = "margin-top: 0;", "Classe 3:"),
                    mod_divClasse_ui("cadre3"),
                    tags$h3(
                      HTML("Classe 4: <span id='info_icon4' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                    ),
                    #h3(style = "margin-top: 0;", "Classe 4:"),
                    mod_divClasse_ui("cadre4"),
                    tags$h3(
                      HTML("Classe 5: <span id='info_icon5' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                    ),
                    #h3(style = "margin-top: 0;", "Classe 5:"),
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

      ## informations popups pour chaque classe ##
      for(i in 1:5){
        minTonnage <- min(r$data[r$data$Site %in% r$classe[[paste0("classe",i)]],]$Tonnages.DIM)
        maxTonnage <- max(r$data[r$data$Site %in% r$classe[[paste0("classe",i)]],]$Tonnages.DIM)
        minSite <- r$data[r$data$Tonnages.DIM == minTonnage,"Site"][[1]]
        maxSite <- r$data[r$data$Tonnages.DIM == maxTonnage,"Site"][[1]]
        runjs(
          paste0("$('#info_icon", i, "').popover({
            content: 'Information sur la classe ", i, ":<br>minTonnage: ", minSite, ":", minTonnage, "<br>maxTonnage: ", maxSite, ":", maxTonnage, "',
            placement: 'right',
            trigger: 'hover',
            html: true
          });")
        )
      }
      })

    # création d'un popup confirmation lorsque l'on appui sur le bouton
    observeEvent(input$randall,{
      confirmSweetAlert(
        session = session, inputId = "myconfirmation2", type = "info",
        title = "Etes vous sur de vouloir réaliser un tirage des sites?",
        danger_mode = TRUE,btn_labels = c("Non", "Oui")
      )

    })








  })
}

## To be copied in the UI
# mod_TabRandom_ui("TabRandom_1")

## To be copied in the server
# mod_TabRandom_server("TabRandom_1")
