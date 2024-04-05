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
            htmlOutput(ns("site1"), style = "font-size: 20px;margin-bottom: 30px;")
          )
        ),
        #htmlOutput(ns("site1")), # SITE tire
        #shinyjs::hidden(
        fluidRow(
          column(
            width = 2,
            pickerInput(
              inputId = ns("choix1"),
              choices = NULL,
              options = pickerOptions(
                  maxOptions = 1,
                  noneSelectedText = "Choix site",
                  header = paste("Choix Site classe",substr(id, nchar(id), nchar(id)),":"),
                  liveSearch = TRUE,
                  liveSearchNormalize = TRUE,
                  noneResultsText= "Aucun résultat pour: {0}",
                  showContent=FALSE,
                  showIcon=FALSE,
                  size=10,
                  style = "custom-dropdown"
              ),
              multiple = TRUE,
              width = "100px"
            )
          ),
          column(
            width = 2,
            offset = ifelse(id == "cadre5",8,6),
              switchInput(
                inputId = ns("Id1"),
                label = NULL,
                onLabel = icon("lock"),
                offLabel = icon("lock-open"),
                value = FALSE,
                disabled = TRUE,
                size='small'
              )
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



      # on stock les valeurs des switchs dans r$switch
      r[[paste0("switch",nb)]] <- input$Id1

      # maj du choix des sites par classe
      if(input$Id1) {
        # Si le SwitchInput est TRUE
        updatePickerInput(session, "choix1",
                          choices =  r$classe[[paste0("classe", nb)]],
                          choicesOpt = list(
                            content = ifelse(
                              # on regarde si le site est outre mer
                              isOutreMer(Tonnage, r$classe, nb),
                              sprintf("<span class='label label-%s'>%s</span>", "info",paste(r$classe[[paste0("classe", nb)]],"(Outre Mer)")),
                              # on regarde si le site a un compacteur
                              ifelse(r$data[r$data$Site %in% r$classe[[paste0("classe", nb)]], "Compacteur"] == 1,
                                     sprintf("<span class='label label-%s'><img src='www/compact_cyclamed.png' alt='Votre image' style='max-width: 24px; max-height: 24px;'> %s</span>", "danger", r$classe[[paste0("classe", nb)]]),
                                     sprintf("<span class='label label-%s'>%s</span>", "primary",r$classe[[paste0("classe", nb)]])
                              )
                            )
                          ),
                          selected =  ifelse(r[[paste0("site",nb)]]== "",NULL,r[[paste0("site",nb)]])
        )
      }

    })



    # affichage du site tire
     output$site1 <- renderUI({
       if(r[[paste0("site",nb)]]=="")
          ""
       else{
         # on recupere l'indice du site tiré
         indice_site <- match(r[[paste0("site",nb)]], r$classe[[paste0("classe",nb)]])
         #  cas du site situé en outre mer
         if (!is.na(indice_site) && isOutreMer(Tonnage, r$classe, nb)[indice_site])
           res <- paste("<span class='label label-info'>",r[[paste0("site",nb)]], "(Outre Mer)</span>")

         # cas du site avec compacteur
         else if (r$data[r$data$Site ==r[[paste0("site",nb)]],"Compacteur"]== 1)
           res <- paste("<span class='label label-danger'><img src='www/compact_cyclamed.png' alt='Votre image' style='max-width: 24px; max-height: 24px;'>", r[[paste0("site",nb)]], "</span>")

         #cas du site classique
         else
           res <- paste("<span class='label label-primary'>",r[[paste0("site",nb)]], "</span>")
         HTML(res)
       }

        })

     # on recupère le choix du site dans le menu déroulant
     observeEvent(input$choix1, {
       r[[paste0("site",nb)]] <- input$choix1
     })
     # tirage
      observeEvent(r$random, {
        if (isTRUE(r$random)){
          updateSwitchInput(session,"Id1",disabled = FALSE,value = TRUE) # on active l"interrupteur
          if(input$Id1){r[[paste0("site",nb)]] # on garde le meme tirage

          }
          else {# on tire un site au hasard pour la classe nb
            nouvelle_valeur <- sample(r$classe[[paste0("classe",nb)]], 1, replace = FALSE)
            r[[paste0("site",nb)]] <- nouvelle_valeur
          }
        }

      })




  })
}

## To be copied in the UI
# mod_divClasse_ui("divClasse_1")

## To be copied in the server
# mod_divClasse_server("divClasse_1")
