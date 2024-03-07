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
#' @import DT
#' @import utils
require(shinyBS)
utils::globalVariables("historique")
mod_TabRandom_ui <- function(id){
  ns <- NS(id)
  tabItem(tabName="rand",
          sidebarPanel(class = "custom-sidebar",
                       shinyjs::useShinyjs(),
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
                       bsPopover(id = ns("randall"), title = "Tirage aléatoire", content = "Cliquez ici pour obenir un tirage aléatoire des sites.", placement = "right", trigger = "hover",
                                 options = NULL),
                       tags$div(
                         style = "margin-top:100px;margin-bottom:100px;", # Aligne au centre
                         shinyjs::hidden(
                            actionBttn(inputId = ns("save_random"),label = "Enregistrer",style = "unite",size = "sm",color = "success")
                         )
                       ),
                       tags$details(
                         tags$summary("Afficher l'historique",style = "display:revert;font-weight: bold; color: #72afd2;"),
                         DT::dataTableOutput(ns("history"))
                       ),
                       actionBttn(inputId = ns("popup_history"),label = "Historique",size="xs",style = "material-flat")

          ),
          mainPanel(class = "custom-main",
                h1("Sites", style = "font-family: 'Open Sans', sans-serif; font-weight: bold; text-align: center;"),
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
            ),
            tags$details(
              tags$summary("Afficher l'historique",style = "display:revert;font-weight: bold; color: #72afd2;"),
              DT::dataTableOutput(ns("history2"))
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

      # on affiche le boutton de sauvegarde du tirage si tout les switchs sont a TRUE
      if(r$switch1&&r$switch2&&r$switch3&&r$switch4&&r$switch5){
        shinyjs::show("save_random",anim = TRUE)
      }
      else{
        shinyjs::hide("save_random",anim = TRUE)
      }

      ####################################Popup information Classes####################################
      for(i in 1:5){
        # on récupères les bornes d'intervalles
        borne_inf <- ifelse(i == 1, 0, round(r$classe[[paste0("bornes", i - 1)]],1))
        borne_sup <- ifelse(i == 5, "\u221E", round(r$classe[[paste0("bornes", i)]],1))
        IC <- ifelse(i==1,paste0("\u2264",borne_sup),ifelse(i==5,paste0("\u2265",borne_inf),paste0("[",borne_inf,",",borne_sup,"]")))
        minTonnage <- min(r$data[r$data$Site %in% r$classe[[paste0("classe",i)]],]$Tonnages.DIM)
        maxTonnage <- max(r$data[r$data$Site %in% r$classe[[paste0("classe",i)]],]$Tonnages.DIM)
        minSite <- r$data[r$data$Tonnages.DIM == minTonnage,"Site"][[1]]
        maxSite <- r$data[r$data$Tonnages.DIM == maxTonnage,"Site"][[1]]
        nbSites <- length(r$classe[[paste0("classe",i)]])
        nbOutreMer <- sum(isOutreMer(r$data,r$classe,i))
        nbCompacteur <-sum(r$data[r$data$Site %in% r$classe[[paste0("classe",i)]],]$Compacteur==1)
        # construction du popup lorsque l'on passe la souris
        session$sendCustomMessage(
          type = "initializePopover",
          message = list(
            i = i,
            IC = IC,
            minSite = minSite,
            minTonnage = minTonnage,
            maxSite = maxSite,
            maxTonnage = maxTonnage,
            nbSites = nbSites,
            nbOutreMer = nbOutreMer,
            nbCompacteur = nbCompacteur
          )
        )

      }
    })





    ####################################Historique####################################
    # Afficher le dataframe dans une table
    output$history <- DT::renderDataTable(DT::datatable(r$hist,
                                          options = list(pageLength = 5,
                                                         dom = "t",
                                                         scrollX = TRUE,
                                                         scrollY = TRUE
                                          ),
                                          editable = list(target = "row", disable = list(columns = c(0,1,2,3,4,5,6))))%>%
                                            formatStyle(columns = 2:6, backgroundColor = "lightblue")%>%
                                            formatStyle(columns = 7,backgroundColor = styleEqual(c("Valide","En attente de validation"),c("#0FD918", "#C4C3B5")))
    )
    output$history2 <- DT::renderDataTable(DT::datatable(r$hist,
                                                         options = list(pageLength = 5,
                                                                        dom = "t",
                                                                        scrollX = TRUE,
                                                                        scrollY = TRUE
                                                         ),
                                                         editable = list(target = "row", disable = list(columns = c(0,1,2,3,4,5,6))))%>%
                                             formatStyle(columns = 2:6, backgroundColor = "lightblue")%>%
                                             formatStyle(columns = 7,backgroundColor = styleEqual(c("Valide","En attente de validation"),c("#0FD918", "#C4C3B5")))
    )
    output$history3 <- DT::renderDT({
      DT::datatable(r$hist,
                                                         options = list(pageLength = 5,
                                                                        dom = "t",
                                                                        scrollX = TRUE,
                                                                        scrollY = TRUE
                                                         ),
                                                         editable = list(target = "row",disable = list(columns = c(0,1,2,3,4,5,6))))%>%
                                             formatStyle(columns = 2:6, backgroundColor = "lightblue")%>%
                                             formatStyle(columns = 7,backgroundColor = styleEqual(c("Valide","En attente de validation"),c("#0FD918", "#C4C3B5")))
    })


    ## stockage des informations ecrites dans l'historique directement depuis l'app ##
    observeEvent(input$history3_cell_edit, {
      info <- input$history3_cell_edit
      for (i in 2:(ncol(historique)+1)) { # les infos sont stockés de 2 a ncol(historique)+1
        row <- info$row[i]
        col <- info$col[i]
        new_value <- info$value[i]
        r$hist[row,col]<- new_value
      }
      write.csv(r$hist,"historique.csv",row.names = FALSE)
      historique <- read.csv("historique.csv")
      usethis::use_data(historique, overwrite = TRUE)
    })


    observeEvent(input$popup_history, {

      # Afficher le popup lorsque le bouton est cliqué
      showModal(modalDialog(
        title = "Historique des tirages",
        DTOutput(ns("history3")),
        easyClose = TRUE, footer = tagList(
          downloadBttn(outputId = ns("save_history"),label = "Télécharger l'historique",size = "sm",style = "material-flat")
        )
      ))
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

    # observeEvent(input$save_random,{
    #   confirmSweetAlert(
    #     session = session, inputId = "confirmation_save", type = "info",
    #     title = "Etes vous sur de vouloir enregistrer votre tirage?",
    #     btn_labels = c("Non", "Oui")
    #   )
    #
    # })


    # popup pour ajouter un commentaire dans l'historique

    observeEvent(input$save_random, {
        showModal(modalDialog(
          div(class = "alert alert-info", role = "alert",
              tags$div(
                class = "alert-primary d-flex align-items-center",  # Suppression de la classe "alert" pour éviter les marges supplémentaires
                HTML('<svg xmlns="http://www.w3.org/2000/svg" width="48" height="48" fill="currentColor" class="bi bi-info-fill" viewBox="0 0 16 16">
             <path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16zm.93-9.412-1 4.705c-.07.34.029.533.304.533.194 0 .487-.07.686-.246l-.088.416c-.287.346-.92.598-1.465.598-.703 0-1.002-.422-.808-1.319l.738-3.468c.064-.293.006-.399-.287-.47l-.451-.081.082-.381 2.29-.287zM8 5.5a1 1 0 1 1 0-2 1 1 0 0 1 0 2z"/>
           </svg>'),
                tags$h2('Est-ce votre choix final pour votre tirage?', class = "ms-3")  # Ajout de la classe "ms-3" pour ajouter un espace à gauche du titre
              )
          ),
          awesomeRadio(
            inputId = ns("Id2"),
            label = "",
            choices = c("Oui","Non"),
            selected = "Oui",
            inline = TRUE,
            checkbox = TRUE
          ),
          textInput(ns('Comment'),"Ajouter un commentaire (optionnel)"),
          footer=tagList(
            modalButton('Annuler'),
            actionButton(ns('save_comments'),label = "Valider")
          ),
          easyClose = TRUE
        ))
    })

    # Ajouter une ligne à l'historique
    observeEvent(input$save_comments, {
      r$hist <- rbind(data.frame(Date=format(Sys.time(), "%d/%m/%Y %H:%M"),
                                        Site1=r$site1,
                                        Site2=r$site2,
                                        Site3=r$site3,
                                        Site4=r$site4,
                                        Site5=r$site5,
                                        Etat = ifelse(input$Id2=="Oui","Valide","En attente de validation"),
                                        Commentaire=input$Comment,
                                        Annee=format(Sys.Date(),"%Y")),r$hist)
      write.csv(r$hist,"historique.csv",row.names = FALSE)
      historique <- read.csv("historique.csv")
      usethis::use_data(historique, overwrite = TRUE)
      removeModal()
    })




    #sauvegarde de l'historique au format csv
    output$save_history <- downloadHandler(
      filename <- function(){
        paste0("Historique",Sys.Date(), ".csv")
      },
      content <- function(file){
        write.csv(r$hist,file,row.names = FALSE)
      }
    )


  })
}

## To be copied in the UI
# mod_TabRandom_ui("TabRandom_1")

## To be copied in the server
# mod_TabRandom_server("TabRandom_1")
