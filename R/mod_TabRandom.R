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
#' @import shinyBS

div_style = "padding: 10px 20px; border-radius: 15px; box-shadow: 0 0 0 transparent, 0 0 0 transparent, 6px 4px 25px #d6d6d6;margin-bottom: 20px"
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


                       # bouton de tirage
                       tags$div(
                         style = "margin-top: 10px;",
                         actionBttn(inputId = ns("randall"), label = "Tirage des sites :", style = "unite",size = "xs", color = "royal")
                       ),
                       bsPopover(ns("randall"), "Tirage aléatoire", content = "Cliquez ici pour obenir un tirage aléatoire des sites.", placement = "right", trigger = "hover",
                                 options = NULL),
                       tags$div(
                         style = "margin-top: 50px;",
                         actionBttn(inputId = ns("save_random"),label = "Enregistrer modification:",style = "gradient",size = "xs",color = "primary")
                       )
          ),
          mainPanel(
            h1("Sites:"),
            shinyjs::useShinyjs(),
            # cadres
            fluidRow(
              column(
                width = 6,
                div(
                  style = div_style,
                  tags$h3(
                    HTML("Classe 1:<span id='info_icon1' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                  ),
                  mod_divClasse_ui("cadre1")
                )
              ),
              column(
                width = 6,
                div(
                  style = div_style,
                  tags$h3(
                    HTML("Classe 2:<span id='info_icon2' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                  ),
                  mod_divClasse_ui("cadre2")
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                div(
                  style = div_style,
                  tags$h3(
                    HTML("Classe 3:<span id='info_icon3' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                  ),
                  mod_divClasse_ui("cadre3")
                )
              ),
              column(
                width = 6,
                div(
                  style = div_style,
                  tags$h3(
                    HTML("Classe 4:<span id='info_icon4' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                  ),
                  mod_divClasse_ui("cadre4")
                )
              )
            ),
            div(
              style = div_style,
                tags$h3(
                  HTML("Classe 5: <span id='info_icon5' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                ),
                mod_divClasse_ui("cadre5")
            )

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
      r$random <- input$confirmation_random

      ## informations popups pour chaque classe ##
      for(i in 1:5){
        minTonnage <- min(r$data[r$data$Site %in% r$classe[[paste0("classe",i)]],]$Tonnages.DIM)
        maxTonnage <- max(r$data[r$data$Site %in% r$classe[[paste0("classe",i)]],]$Tonnages.DIM)
        minSite <- r$data[r$data$Tonnages.DIM == minTonnage,"Site"][[1]]
        maxSite <- r$data[r$data$Tonnages.DIM == maxTonnage,"Site"][[1]]
        nbOutreMer <- sum(isOutreMer(r$data,r$classe,i))
        nbCompacteur <-sum(r$data[r$data$Site %in% r$classe[[paste0("classe",i)]],]$Compacteur==1)
        runjs(
          paste0("$('#info_icon", i, "').popover({
            content: '<div class=\"custom-header\">Information classe ", i, ":</div><div class=\"custom-body\"><b>MinTonnage: </b>", minSite, ":", minTonnage,
                         "<br><b>MaxTonnage: </b>", maxSite, ":", maxTonnage, "<br><b>Nombre de sites outre mers: </b>",nbOutreMer,
                         "<br><b>Nombre de sites avec compacteur: </b>",nbCompacteur,"</div>',
            placement: 'right',
            trigger: 'hover',
            html: true,
          });")
        )

      }
    })




    # création d'un popup confirmation lorsque l'on appui sur le bouton random
    observeEvent(input$randall,{
      confirmSweetAlert(
        session = session, inputId = "confirmation_random", type = "info",
        title = "Etes vous sur de vouloir réaliser un tirage des sites?",
        btn_labels = c("Non", "Oui")
      )

    })

    # création d'un popup confirmation lorsque l'on appui sur le bouton enregistrer

    observeEvent(input$save_random,{
      confirmSweetAlert(
        session = session, inputId = "confirmation_save", type = "info",
        title = "Etes vous sur de vouloir enregistrer votre tirage?",
        btn_labels = c("Non", "Oui")
      )

    })







  })
}

## To be copied in the UI
# mod_TabRandom_ui("TabRandom_1")

## To be copied in the server
# mod_TabRandom_server("TabRandom_1")
