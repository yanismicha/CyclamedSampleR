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
  ns <- NS(id)
  tabItem(tabName = "resume",
          sidebarPanel("Informations requises",shinyjs::useShinyjs(),
                       ## selection d'une variable ##
                       selectInput(ns("var1"), " Choisissez une variable", choices = names(Tonnage)),
                       radioButtons(ns("bool1"), "Souhaitez vous regarder une partie de la population?", choices = c('non', 'oui')),
                       shinyjs::hidden( #s'affiche uniquement lorsque l'on souhaiter regarder une partie de la pop
                         selectInput(ns("var_quali"), "Variable à discriminer:", choices = names(Tonnage[,c(-4,-5)])),
                         ## choix de la modalité à regarder ##
                         selectInput(ns("cat1"), "Quel partie de la population souhaitez vous regarder?", choices = NULL)
                       ),
                       ## boutton pour lancer le résumé statistique ##
                       actionBttn(inputId = ns("run"),label = "run", style = "unite",size = "xs",color = "royal")
          ),
          mainPanel(
            h1("Résumé statistiques"),
            verbatimTextOutput(ns("summary"))
          )
  )
}

#' TabSummary Server Functions
#'
#' @noRd
mod_TabSummary_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      ## mise à jour du choix des modalités ##
      updateSelectInput(session,"cat1",choices=levels(as.factor(r$data[,input$var_quali])))
      if(input$bool1 == "oui"){
        shinyjs::show("var_quali")
        shinyjs::show("cat1")
      }
      else{
        shinyjs::hide("var_quali")
        shinyjs::hide("cat1")
      }
    })

    resume <- eventReactive(input$run, {
      v1 <- input$var1
      x1 <- r$data[,v1]
      if(typeof(x1) == "integer"||typeof(x1) == "double"){##cas ou c'est un variable quanti
        if(input$bool1 == "oui"){##on regarde une sous partie
          data_filtre <- r$data[r$data[,input$var_quali] == input$cat1, ]
          n_observations <- length(data_filtre[,v1])
          frequency <- n_observations / length(x1)#/73000 normalement
          pop <- round(frequency*100,2)
          # Création du résumé personnalisé
          custom_summary <- summary(data_filtre[,v1])
          custom_summary <- c(custom_summary, N_Observations = n_observations, Pourcentage_population = pop)
          round(custom_summary,2)
        }
        else #on regarde la variable
          summary(r$data[,input$var1])
      }
      else{#cas d'une variable qualitative#
        if(input$bool1 == "oui"){##on regarde une sous partie
          data_filtre <- r$data[r$data[,input$var_quali] == input$cat1, ]
          subx1 <- data_filtre[,v1]
          effectifs <- table(subx1)
          effectifsCumulés <- cumsum(effectifs)
          frequence <- round(effectifs/length(subx1)*100,2)
          frequence_cumulés <- cumsum(frequence)
          table_data <- data.frame(Effectif = as.vector(effectifs),EffectifsCumules= effectifsCumulés, Frequence = as.vector(frequence), Frequence_Cumulees= frequence_cumulés)
          table_data
        }
        else{
          effectifs <- table(x1)
          frequence <- round(effectifs/length(x1)*100,2)
          frequence_cumulés <- cumsum(frequence)
          table_data <- data.frame(Effectif = as.vector(effectifs), Frequence = as.vector(frequence), Frequence_Cumulees= frequence_cumulés)
          table_data
        }
      }
    },ignoreNULL = FALSE) #ignoreNull=false, permet d'afficher sans cliquer sur run



    output$summary <- renderPrint({
      resume()
    })

  })
}

## To be copied in the UI
# mod_TabSummary_ui("TabSummary_1")

## To be copied in the server
# mod_TabSummary_server("TabSummary_1")
