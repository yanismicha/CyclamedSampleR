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
utils::globalVariables("historique")
mod_TabRandom_ui <- function(id){
  ns <- NS(id)
  tabItem(tabName="rand",
          sidebarPanel(class = "custom-sidebar",
                       shinyjs::useShinyjs(),
                       # boutton guide
                       # tags$div(
                       #    style = "float: right;", # Aligne à droite
                       #    actionBttn(inputId = "guide2", label = "Guide", style = "unite", color = "primary")
                       # ),
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
                       tags$div(
                         style = "margin-top:100px;margin-bottom:100px;", # Aligne au centre
                         shinyjs::hidden(
                            actionBttn(inputId = ns("save_random"),label = "Enregistrer",style = "unite",size = "sm",color = "success")
                         )
                       ),
                       actionBttn(inputId = ns("popup_history"),label = "Historique",size="xs",style = "material-flat")
          ),
          mainPanel(class = "custom-main",
            # popup information sur le code couleur utilisé pour les sites
            HTML("<div style='display: flex; justify-content: center; align-items: center;'> <h1 style='font-weight: bold; margin-right: 200px; margin-left: 220px'>Sites</h1> <i id='info_icon' class='fa-solid fa-circle-info fa-beat-fade custom-icon' style='--fa-animation-duration: 4s; font-weight: bold;'></i> </div>"),
            # cadres
            fluidRow(
              column(
                width = 6,
                div(
                  class = "custom-div",
                  tags$h3(
                    class="custom-title",
                    HTML("Classe 1 <i id='info_icon1' class='fa-solid fa-circle-info fa-beat-fade custom-icon' style='--fa-animation-duration: 4s'></i>")
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
                    HTML("Classe 2 <i id='info_icon2' class='fa-solid fa-circle-info fa-beat-fade custom-icon' style='--fa-animation-duration: 4s'></i>")
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
                    HTML("Classe 3 <i id='info_icon3' class='fa-solid fa-circle-info fa-beat-fade custom-icon' style='--fa-animation-duration: 4s'></i>")
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
                    HTML("Classe 4 <i id='info_icon4' class='fa-solid fa-circle-info fa-beat-fade custom-icon' style='--fa-animation-duration: 4s'></i>")
                  ),
                  mod_divClasse_ui("cadre4")
                )
              )
            ),
            div(
                class = "custom-div",
                tags$h3(
                  class="custom-title",
                  HTML("Classe 5 <i id='info_icon5' class='fa-solid fa-circle-info fa-beat-fade custom-icon' style='--fa-animation-duration: 4s'></i>")
                ),
                mod_divClasse_ui("cadre5")
            ),
            actionBttn(inputId = ns("load_last_random"),label = "Charger le dernier tirage",size="xs",style = "material-flat")
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

      # on stocke la valeur du popup last_confirmation pour l'utiliser dans le module divClasse
      r$last_random <- input$confirmation_last_random

      # on affiche le boutton de sauvegarde du tirage si tout les switchs sont a TRUE
      if(r$switch1&&r$switch2&&r$switch3&&r$switch4&&r$switch5){
        shinyjs::show("save_random",anim = TRUE)
      }
      else{
        shinyjs::hide("save_random",anim = TRUE)
      }

      # popup information code couleurs des sites
      session$sendCustomMessage(
        type = "initPopColor",
        message = list(
        )
      )




      for(i in 1:5){
        ####################################Popup information Sites####################################
        region <- r$data[r$data$Site == r[[paste0("site",i)]], "Region"]
        maisonMere <- r$data[r$data$Site == r[[paste0("site",i)]], "Maison.Mere"]
        tonnageSite <- r$data[r$data$Site == r[[paste0("site",i)]], "Tonnages.DIM"]
        rotation <- r$data[r$data$Site == r[[paste0("site",i)]], "Nbre.de.rotation"]
        compacteur <- ifelse(r$data[r$data$Site == r[[paste0("site",i)]], "Compacteur"] == 1, "Oui", "Non")

        session$sendCustomMessage(
          type = "updatePopSite",
          message = list(
            i = i,
            region = region,
            maisonMere = maisonMere,
            tonnage = tonnageSite,
            rotation = rotation,
            compacteur = compacteur
          )
        )
        ####################################Popup information Classes####################################
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


    ####################################Tirage####################################


    # création d'un popup confirmation lorsque l'on appui sur le bouton random
    observeEvent(input$randall,{
      confirmSweetAlert(
        session = session, inputId = "confirmation_random", type = "info",
        title = "Etes vous sur de vouloir réaliser un tirage des sites?",
        btn_labels = c("Non", "Oui")
      )

    })



    ## Charger le dernier tirage sauvegarder ##
    observeEvent(input$load_last_random,{
      confirmSweetAlert(
        session = session, inputId = "confirmation_last_random", type = "info",
        title = "Etes vous sur de vouloir récupérer le dernier tirage des sites?",
        btn_labels = c("Non", "Oui")
      )
    })



    ####################################Historique####################################
    # Afficher le dataframe dans une table
    output$history <- DT::renderDT({
      # on attribue une couleur à chaque site selon sa spécificité (compacteur,outremer,classique)
      color <- ifelse(Tonnage$Compacteur==1,"#dd4b39",ifelse(isOutreMer(Tonnage),"#00c0ef","#3c8dbc"))
      DT::datatable(r$hist,
                    options = list(pageLength = 5,
                                   lengthMenu = c(5,10,length(r$hist)),
                                   dom = "lfit",
                                   scrollX = TRUE,
                                   scrollY = TRUE,
                                   language = list(
                                     info = "Tirages _START_ à _END_ sur un total de _TOTAL_ tirages",
                                     lengthMenu = "Afficher _MENU_ tirages."
                                   )
                    ),
                    editable = list(target = "row",disable = list(columns = c(0,1,2,3,4,5,6))))%>%
      formatStyle(columns = 2:6,backgroundColor = "lightgray",color = styleEqual(Tonnage$Site,values = color))%>%
      formatStyle(columns = 7,backgroundColor = styleEqual(c("Valide","En attente de validation","Non valide"),c("#CCFFCC", "#FFFFCC","#FFAAAA")))
    })


    ## stockage des informations ecrites dans l'historique directement depuis l'app ##
    observeEvent(input$history_cell_edit, {
      info <- input$history_cell_edit
      for (i in 2:(ncol(historique)+1)) { # les infos sont stockés de 2 a ncol(historique)+1
        row <- info$row[i]
        col <- info$col[i]
        new_value <- info$value[i]
        if(i==8){
          if (!new_value%in% c("Valide","En attente de validation","Non valide"))
            r$hist[row,col]<- "Non valide"
          else
            r$hist[row,col]<- new_value
        }
        else if(i == 10){
          if(is.na(as.numeric(new_value))||as.numeric(new_value)<2024)
            r$hist[row,col]<- as.numeric(format(Sys.Date(),"%Y"))
          else
            r$hist[row,col]<- new_value
        }
        else
          r$hist[row,col]<- new_value
      }
      write.csv(r$hist,"historique.csv",row.names = FALSE)
      historique <- read.csv("historique.csv")
      usethis::use_data(historique, overwrite = TRUE)

    })

    ## Popup permettant d'afficher l'historique ##

    observeEvent(input$popup_history, {

      # Afficher le popup lorsque le bouton est cliqué
      showModal(modalDialog(
        title = "Historique des tirages",
        DTOutput(ns("history")),
        size = "l",
        easyClose = TRUE, footer = tagList(
          downloadBttn(outputId = ns("save_history"),label = "Télécharger l'historique",size = "sm",style = "material-flat")
        )
      ))
    })








    # popup pour ajouter un commentaire dans l'historique

    observeEvent(input$save_random, {
        showModal(modalDialog(
          div(class = "alert alert-info", role = "alert",
              tags$div(
                class = "alert-primary d-flex align-items-center",  # Suppression de la classe "alert" pour éviter les marges supplémentaires
                HTML('<i class="fa-solid fa-circle-exclamation fa-flip" style="font-size: 56px; --fa-animation-duration: 4s"></i>'), # icone anime
                tags$h2('Est-ce votre choix final pour votre tirage?', class = "ms-3")  # Ajout de la classe "ms-3" pour ajouter un espace à gauche du titre
              )
          ),
          awesomeRadio(
            inputId = ns("Id2"),
            label = "",
            choices = c("Oui","Non"),
            selected = "Oui",
            inline = TRUE,
            status = "info",
            checkbox = TRUE
          ),
          #textInput(ns('Comment'),"Ajouter un commentaire (optionnel)"),
          textInputIcon(ns('Comment'),"Ajouter un commentaire (optionnel)",placeholder = "Exemple:présence compacteur",icon = icon("comment-dots")),
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
                                        Caracterisation=as.numeric(format(Sys.Date(),"%Y"))),r$hist)
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
