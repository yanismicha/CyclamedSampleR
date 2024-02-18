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

mod_TabRandom_ui <- function(id){
  ns <- NS(id)
  tabItem(tabName="rand",
          sidebarPanel(class = "custom-sidebar",
                       # boutton guide
                       tags$div(
                          style = "float: right;", # Aligne à droite
                          actionBttn(inputId = "guide2", label = "Guide", style = "unite", color = "primary")
                       ),
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
                         style = "text-align: center; margin-top: 100px;", # Alignement et décalage vers le haut
                         actionBttn(inputId = ns("randall"), label = "Tirage aléatoire", style = "unite", size = "lg", color = "royal")
                       ),
                       bsPopover(ns("randall"), "Tirage aléatoire", content = "Cliquez ici pour obenir un tirage aléatoire des sites.", placement = "right", trigger = "hover",
                                 options = NULL),
                       tags$div(
                         style = "margin-top:100px", # Aligne au centre
                         actionBttn(inputId = ns("save_random"),label = "Enregistrer",style = "unite",size = "sm",color = "success")
                       )
          ),
          mainPanel(class = "custom-main",
                h1("Sites", style = "font-family: 'Open Sans', sans-serif; font-weight: bold; text-align: center;"),
            shinyjs::useShinyjs(),
            # cadres
            fluidRow(
              column(
                width = 6,
                div(
                  class = "custom-div",
                  tags$h3(
                    class="custom-title",
                    HTML("Classe 1 <span id='info_icon1' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                  ),
                  mod_divClasse_ui("cadre1")
                )
              ),
              column(
                width = 6,
                div(
                  class = "custom-div",
                  tags$h3(
                    class="custom-title",
                    HTML("Classe 2 <span id='info_icon2' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                  ),
                  mod_divClasse_ui("cadre2")
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                div(
                  class = "custom-div",
                  tags$h3(
                    class="custom-title",
                    HTML("Classe 3 <span id='info_icon3' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                  ),
                  mod_divClasse_ui("cadre3")
                )
              ),
              column(
                width = 6,
                div(
                  class = "custom-div",
                  tags$h3(
                    class="custom-title",
                    HTML("Classe 4 <span id='info_icon4' class='glyphicon glyphicon-info-sign custom-icon'></span>")
                  ),
                  mod_divClasse_ui("cadre4")
                )
              )
            ),
            div(
                class = "custom-div",
                tags$h3(
                  class="custom-title",
                  HTML("Classe 5 <span id='info_icon5' class='glyphicon glyphicon-info-sign custom-icon'></span>")
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
            content: '<div class=\"custom-header\">Information classe ", i,
                 ":</div><div class=\"custom-body\"><b>Intervalle de tonnage: </b> [",round(minTonnage),",",round(maxTonnage),"]",
                          "<br><b>MinTonnage: </b>", minSite, ":", minTonnage,
                          "<br><b>MaxTonnage: </b>", maxSite, ":", maxTonnage,
                          "<br><b>Nombre de sites: </b>",length(r$classe[[paste0("classe",i)]]),
                          "<br><b>Dont outre mers: </b>",nbOutreMer,
                          "<br><b>Dont compacteur: </b>",nbCompacteur,"</div>',
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
