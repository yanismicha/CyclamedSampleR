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
  #div(style = "margin-bottom: 20px;", #espacement entre cadres
      # tags$h3(id = ns("iconpop",
      #   HTML(paste("Classe",substr(id, nchar(id), nchar(id))),": <span id='", ns("info_icon"), "' class='glyphicon glyphicon-info-sign custom-icon'></span>")
      # ),
      #div(
       # style = "padding: 10px 20px; border-radius: 15px; box-shadow: 0 0 0 transparent, 0 0 0 transparent, 6px 4px 25px #d6d6d6;", #esthetique cadre exterieur
      div(
        id = ns("cadre1"),
        #style = "padding: 10px 20px; border-radius: 40px; box-shadow: 0 0 0 transparent, 0 0 0 transparent, 6px 4px 25px #d6d6d6;", # esthetique cadre interieur
        # shinyjs::hidden( #permet de cacher le cadre en amont
        #   h3(id = ns("trash-icon"),
        #     style = "margin-top: 0; color: #ff3924; float:right;",#emplacement et style icone
        #     span(class = "glyphicon glyphicon-trash") #icone compacteur
        #   )
        # ),
        fluidRow(
          column(
            width = 12,
            align = "center",
            htmlOutput(ns("site1"), style = "font-size: 20px;margin-bottom: 30px;") # Vous pouvez ajuster la taille de la police selon vos besoins
          )
        ),
        #htmlOutput(ns("site1")), # SITE tire
        #shinyjs::hidden(
        fluidRow(
          column(
            width = 2,
            pickerInput(
              inputId = ns("choix1"),
              #label = "Choix d'un autre site:",
              choices = NULL,
              options = pickerOptions(
                  # title = "Sites:  ",
                  maxOptions = 1,
                  noneSelectedText = "Choix site",
                  header = paste("Choix Site classe",substr(id, nchar(id), nchar(id)),":"),
                  liveSearch = TRUE,
                  liveSearchNormalize = TRUE,
                  noneResultsText= "Aucun résultat pour: {0}",
                  showContent=FALSE,
                  showIcon=FALSE,
                  size=10,
                  style = "dropdown"
              ),
              multiple = TRUE,
              width = "100px"
            )
          ),
          column(
            width = 2,
            offset = ifelse(id == "cadre5",8,6),
            shinyjs::disabled(
              prettySwitch(
                # bouton pour garder le site ou non
                inputId = ns("Id1"),
                label = NULL,
                status = "success",
                value = FALSE,
                fill = TRUE,
              )
            ),
            "garder le site",
          )
        )
      )

      #)
  #)
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


      # maj du choix des sites par classe
      if(input$Id1){
        #shinyjs::hide("choix1")
      updatePickerInput(session,"choix1",choices =  r$classe[[paste0("classe",nb)]],choicesOpt = list(
        content = ifelse(
          # on regarde si le site est outre mer
          isOutreMer(Tonnage, r$classe, nb),
          sprintf("<span class='label label-%s'>%s</span>", "info",paste(r$classe[[paste0("classe", nb)]],"(Outre Mer)")),
          # on regarde si le site a un compacteur
          ifelse(r$data[r$data$Site %in% r$classe[[paste0("classe", nb)]], "Compacteur"] == 1,
                 sprintf("<span class='label label-%s'><span class='glyphicon glyphicon-alert'></span> %s</span>", "danger", r$classe[[paste0("classe", nb)]]),
                 sprintf("<span class='label label-%s'>%s</span>", "primary",r$classe[[paste0("classe", nb)]])
          )
        )
      ),
      #choix du site tiré
      selected =  ifelse(r[[paste0("site",nb)]]== "",NULL,r[[paste0("site",nb)]])
      )
      }
      # else
      #   shinyjs::show("choix1")


      # affichage de l'icone compacteur
      # if(r[[paste0("site",nb)]]!= "" && r$data[r$data$Site == r[[paste0("site",nb)]],"Compacteur"]==1){
      #   shinyjs::show("trash-icon")
      # }
      # else{
      #   shinyjs::hide("trash-icon")
      # }



    })

    # affichage du site tire
     output$site1 <- renderUI({
       if(is.null(input$choix1))
          ""
       else{
         # on recupere l'indice du site tiré
         indice_site <- match(input$choix1, r$classe[[paste0("classe",nb)]])
         #  cas du site situé en outre mer
         if (!is.na(indice_site) && isOutreMer(Tonnage, r$classe, nb)[indice_site])
           res <- paste("<span class='label label-info'>",input$choix1, "(Outre Mer)</span>")

         # cas du site avec compacteur
         else if (r$data[r$data$Site ==input$choix1,"Compacteur"]== 1)
           res <- paste("<span class='label label-danger'><span class='glyphicon glyphicon-alert'></span>", input$choix1, "</span>")

         #cas du site classique
         else
           res <- paste("<span class='label label-primary'>",input$choix1, "</span>")
         HTML(res)
       }

        })

     # tirage
      observeEvent(r$random, {
        if (isTRUE(r$random)){
          shinyjs::enable("Id1")
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
