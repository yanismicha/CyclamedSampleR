#' TabSummary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
utils::globalVariables("Tonnage")



mod_TabSummary_ui <- function(id){

  # Réecriture des noms en les liant à leur valeurs
  valeurs1 <- c(names(Tonnage[,-6]))
  cles1 <- c("Région", "Maison Mère", "Site", "Tonnage DIM", "Nombre Rotation")

  valeurs2 <- c(names(Tonnage[,c(-4,-5)]))
  cles2 <- c("Région", "Maison Mère", "Site", "Compacteur")

  ns <- NS(id)

  tabItem(tabName = "resume",
          sidebarPanel("Informations requises",shinyjs::useShinyjs(),
                       ## selection d'une variable ##
                       #selectInput(ns("var1"), " Choisissez une variable", choices = c(names(Tonnage[,-6]),'Classe')),

                       selectInput(ns("var1"), " Choisissez une variable", choices = c(setNames(valeurs1, cles1),'Classe')),


                       shinyjs::hidden(
                         sliderInput(ns("classe_slider"), "Sélectionnez la classe", min =1, max = 5, value = 1)
                         #radioButtons(ns("classe_boutton"), "Souhaitez vous regarder une partie de la population?", choices = c('Oui', 'Non'),selected = 'Non'),

                       ),

                       radioButtons(ns("bool1"), "Souhaitez vous regarder une partie de la population?", choices = c('Oui', 'Non'),selected = 'Non'),
                       shinyjs::hidden( # s'affiche uniquement lorsque l'on souhaite regarder une partie de la pop
                         # selectInput(ns("var_quali"), "Variable à discriminer:", choices = names(Tonnage[,c(-4,-5)])),
                         selectInput(ns("var_quali"), "Variable à discriminer:", choices = setNames(valeurs1, cles1)),

                         ## choix de la modalité à regarder ##
                         selectInput(ns("cat1"), "Quel partie de la population souhaitez vous regarder?", choices = NULL)
                       ),
                       ## boutton pour lancer le résumé statistique ##
                       actionBttn(inputId = ns("run"),label = "Lancer", style = "unite",size = "xs",color = "royal")
          ),


          mainPanel(
            HTML("<div style='text-align: center; margin-top: 20px;'> <h1 style='font-weight: bold;'>Résumé Statistiques</h1> </div>"),
            div(
              class = "custom-box",
              verbatimTextOutput(ns("summary"))
            )
          )



  )
}

#' TabSummary Server Functions
#'
#' @noRd
#' TabSummary Server Functions
#'
#' @noRd
mod_TabSummary_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      ## mise à jour du choix des modalités ##
      if (!is.null(input$var_quali)) {
        levels <- levels(as.factor(r$data[,input$var_quali]))
        if (input$var_quali == "Compacteur") {
          levels <- ifelse(levels == "0", "Sans compacteur", "Avec compacteur")
        }
        updateSelectInput(session, "cat1", choices = levels)
      }
      if(input$bool1 == "Oui"){
        shinyjs::show("var_quali")
        shinyjs::show("cat1")
      }
      else{
        shinyjs::hide("var_quali")
        shinyjs::hide("cat1")
      }


      if(input$var1 == "Classe"){
        shinyjs::show("classe_slider")
      }else{
        shinyjs::hide("classe_slider")
      }


    })

    resume <- eventReactive(input$run, {

      if (input$var1 == 'Classe'){ # Si la variable est Classe
        i <- input$classe_slider
        df<-r$data[r$data$Site %in% r$classe[[paste0("classe",i)]],]



        if(input$bool1 == "Oui"){ # On rentre dans le bloc si c'est Classe et oui
          if(input$var_quali == 'Compacteur'){ # On differencie compacteur et les autres variables
            data_filtre <- df[df[,input$var_quali] == ifelse(input$cat1 == "Avec compacteur", "1", "0"), ]

          }else{
            data_filtre <- df[df[,input$var_quali] == input$cat1, ]
          }
          # Traitements pour les classes avec condition
          if(nrow(data_filtre) == 0){
            "Il n'existe pas de données selon vos recherches."
          }else{
            ton <- data_filtre[4]
            rot <- data_filtre[5]
            nbrcomp <- sum(data_filtre[6])
            nbrcont <- sum(data_filtre[6] == 0)
            Ston <- summary(ton)
            Srot <- summary(rot)
            TR <- data.frame( Tonnage = as.vector(Ston), Rotatation = as.vector(Srot))
            CP <- data.frame(Avec_Compacteur = nbrcomp, Sans_Compacteur = nbrcont)
            tp <- list(Données = as.data.frame(data_filtre), Analyse = TR, Compacteur = CP)
            tp
          }


        }else{
          # Traitements pour les classes sans condition
          info_classe <- r$data[r$data$Site %in% r$classe[[paste0("classe",i)]],]
          ton <- info_classe[4]
          rot <- info_classe[5]
          nbrcomp <- sum(info_classe[6])
          nbrcont <- sum(info_classe[6] == 0)
          Ston <- summary(ton)
          Srot <- summary(rot)
          TR <- data.frame( Tonnage = as.vector(Ston), Rotatation = as.vector(Srot))
          CP <- data.frame(Avec_Compacteur = nbrcomp, Sans_Compacteur = nbrcont)
          tp <- list(Données = as.data.frame(info_classe), Analyse = as.data.frame(TR), Compacteur = as.data.frame(CP))

          tp


        }
      }
      else{ v1 <- input$var1

      x1 <- r$data[,v1]
      if (typeof(x1) %in% c("integer", "double")) { ## cas où c'est une variable quantitative
        if(input$bool1 == "Oui"){## on regarde une sous partie
          if(input$var_quali == "Compacteur") {
            data_filtre <- r$data[r$data[,input$var_quali] == ifelse(input$cat1 == "Avec compacteur", "1", "0"), ]
          } else {
            data_filtre <- r$data[r$data[,input$var_quali] == input$cat1, ]
          }
          n_observations <- length(data_filtre[,v1])
          frequency <- n_observations / length(x1) #/73000 normalement
          pop <- round(frequency * 100, 2)
          # Création du résumé personnalisé
          custom_summary <- summary(data_filtre[,v1])
          custom_summary <- c(custom_summary, Nombre = round(n_observations,0), Pourcentage = pop)
          round(custom_summary, 2)
        } else { # on regarde la variable
          summary(r$data[,input$var1])
        }
      } else { # cas d'une variable qualitative
        if(input$bool1 == "Oui"){## on regarde une sous partie
          if(input$var_quali == "Compacteur") {
            data_filtre <- r$data[r$data[,input$var_quali] == ifelse(input$cat1 == "Avec compacteur", "1", "0"), ]
          } else {
            data_filtre <- r$data[r$data[,input$var_quali] == input$cat1, ]
          }
          subx1 <- data_filtre[,v1]
          effectifs <- table(subx1)
          effectifsCumulés <- cumsum(effectifs)
          frequence <- round(effectifs/length(subx1) * 100, 2)
          frequence_cumulés <- cumsum(frequence)
          table_data <- data.frame(Effectif = as.vector(effectifs), EffectifsCumulés = effectifsCumulés, Pourcentage = as.vector(frequence))#, "Frequence Cumulees" = frequence_cumulés)
          table_data
        } else {
          if(input$var_quali == "Compacteur") {
            x1 <- ifelse(x1 == "1", "Avec compacteur", "Sans compacteur")
          }
          effectifs <- table(x1)
          effectifsCumulés <- cumsum(effectifs)
          frequence <- round(effectifs/length(x1) * 100, 2)
          frequence_cumulés <- cumsum(frequence)
          table_data <- data.frame(Effectif = as.vector(effectifs), EffectifsCumulés = effectifsCumulés, Pourcentage = as.vector(frequence))#, "Frequence Cumulees" = frequence_cumulés)
          table_data
        }
      }
      }
    }, ignoreNULL = FALSE) # ignoreNULL=false, permet d'afficher sans cliquer sur run

    output$summary <- renderPrint({
      resume()
    })
  })
}

## To be copied in the UI
# mod_TabSummary_ui("TabSummary_1")

## To be copied in the server
# mod_TabSummary_server("TabSummary_1")
