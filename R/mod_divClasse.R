#' divClasse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_divClasse_ui <- function(id){
  ns <- NS(id)
  div(style = "margin-bottom: 20px;", #espacement entre cadres
      div(
        id = ns("cadre1"),
        style = "padding:4px; border:4px solid #e0e0e0;", #esthetique cadre exterieur
        div(
          style = "padding:3px; background-color:#D7EAB6;", # esthetique cadre interieur
          shinyjs::hidden( #permet de cacher le cadre en amont
            h3(id = ns("trash-icon"),
              style = "margin-top: 0; color: #ff3924; float:right;",#emplacement et style icone
              span(class = "glyphicon glyphicon-trash") #icone compacteur
            )
          ),
          textOutput(ns("site1")), # SITE tire
          shinyjs::hidden(
            pickerInput( # choix d'un site parmi la classe
              ns("choix1"),
              "Choix d'un autre site:",
              choices = NULL,
              options = list(title = "Sites: "),
              multiple = TRUE
            )
          ),
          prettySwitch(# bouton pour garder le site ou non
            inputId = ns("Id1"),
            label = "garder le site",
            status = "success",
            value = FALSE,
            fill = TRUE
          )
        )
      )
  )
}

#' divClasse Server Functions
#'
#' @noRd
mod_divClasse_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #stocke le numero id du cadre
    nb <- substr(id, nchar(id), nchar(id))

    observe({
      if(input$Id1 == FALSE) # affichage du choix de site
        shinyjs::show("choix1")
      else
        shinyjs::hide("choix1")

      # maj du choix des sites par classe
      updatePickerInput(session,"choix1",choices =  r$classe[[paste0("classe",nb)]],choicesOpt = list(
        icon = ifelse(r$data[r$data$Site %in% r$classe[[paste0("classe",nb)]],"Compacteur"]==1,"glyphicon-trash","")))


      # affichage de l'icone compacteur
      if(r[[paste0("site",nb)]]!= "" && r$data[r$data$Site == r[[paste0("site",nb)]],"Compacteur"]==1){
        shinyjs::show("trash-icon")
      }
      else{
        shinyjs::hide("trash-icon")
      }

    })

    # affichage du site tire
     output$site1 <- renderText({
       r[[paste0("site",nb)]]
     })


     # tirage
      observeEvent(r$random, {
        if (isTRUE(r$random)){
          if(input$Id1){r[[paste0("site",nb)]]}
          else {
            nouvelle_valeur <- sample(r$classe[[paste0("classe",nb)]], 1, replace = FALSE)
            r[[paste0("site",nb)]] <- nouvelle_valeur
          }
          updatePrettySwitch(session,"Id1",value=TRUE)
        }
      })




  })
}

## To be copied in the UI
# mod_divClasse_ui("divClasse_1")

## To be copied in the server
# mod_divClasse_server("divClasse_1")
