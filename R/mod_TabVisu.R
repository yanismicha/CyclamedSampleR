#' TabVisu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import plotly
mod_TabVisu_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "visu",

          fluidRow(
            column(width = 8,
                   div(style = "margin-top: 40px;",
                     pickerInput(ns("viewType"),
                                 "Type de Vue:",
                                 choices = c("Proportion de Compacteurs par Classe",
                                             "Tonnage par classe",
                                             "Tonnage par Régions",
                                             "Tonnage par Maison Mère",
                                             "Nombre de Sites en Outre-Mer par Classe"),
                                 options = pickerOptions(size=15, style = "custom-dropdown")
                                 )
                   )
            ),
            column(width = 4,
                   div(style = "text-align: right; margin-top: 80px;",
                       actionBttn(ns("reset_btn"), "Réinitialiser la sélection", size="xs", style="unite")
                   )
            )
          )

          ,
          # Graphiques
          fluidRow(
            column(
              width = 6,
              div(
                class = "custom-div",
                plotlyOutput(ns("mainPlot"), height = "250px")
              )
            )
            ,
            column(
              width = 6,
              div(
                class = "custom-div",
                plotlyOutput(ns("pie"), height = "250px")
              )
            )
          ),

          fluidRow(
            column(
              width = 6,
              div(
                class = "custom-div",
                plotlyOutput(ns("scatterPlot"), height = "250px")
              )
            )
            ,
            column(
              width = 6,
              div(
                class = "custom-div",
                plotlyOutput(ns("boxPlot"), height = "250px")
              )
            )
          )
  )
}









#' TabVisu Server Functions
#'
#' @noRd
mod_TabVisu_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #data frame contenant le fichier tonnage et les classe
    updatedData <- reactive({
      data_copy <- r$data
      data_copy$classe <- NA
      data_copy$classe <- sapply(data_copy$Site, function(site) {
        for(i in 1:5){
          if (site %in% r$classe[[paste0("classe",i)]])
            return(i)
        }
      })
      data_copy
    })
    ########################Filtres##################################
    selected_region <- reactiveVal()
    selected_maison <- reactiveVal()
    selectedCompacteur <- reactiveVal()

    ######Filtre compacteur
    observeEvent(event_data("plotly_click", source = "pieChart"), {
      selectedCompacteur(event_data("plotly_click", source = "pieChart")$key)
      ###### On récupères les labels grace à key lors d'un clique

    })
    ######Filtre Région
    observeEvent(event_data("plotly_click", source = "regionSelect"), {
      selected_region(event_data("plotly_click", source = "regionSelect")$y)
      # On récupère les région avec la donnée y

      #Boîte de dialogue contenant un graph plotly des sites selon une région
      showModal(modalDialog(
        plotlyOutput(ns("modalPlot")),
        size = "m",
        easyClose = TRUE,
        style = "background-color: #ecf0f5;",
        footer = modalButton("Fermer")
      ))
      output$modalPlot <- renderPlotly({
        data = updatedData()
        data_region <- data %>%
          filter(Region == selected_region()) %>%
          mutate(
            TotalTonnageRegion = sum(Tonnages.DIM, na.rm = TRUE),
            Proportion = Tonnages.DIM / TotalTonnageRegion
          ) %>%
          arrange(desc(Proportion))
        scaled_tonnage <- scales::rescale(data_region$Tonnages.DIM, to = c(0, 1))
        colors <- colorRampPalette(c("#0c8fce", "#0a5f9c", "#08306b"))(100)[as.integer(scaled_tonnage * 99) + 1]
        plot_ly(data = data_region, x = ~Tonnages.DIM, y = ~Site, type = 'bar', orientation = 'h', marker = list(color = colors),
                hovertext = ~paste(Site, ": ", Tonnages.DIM, "tonnes",
                                   '<br>Proportion:', round(Proportion,3)*100,"%"),
                hoverinfo = 'text') %>%
          layout(title = paste("Tonnages des Sites pour", selected_region()),paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5',xaxis = list(title = "Tonnages DIM"),
                 yaxis = list(title = "Site",categoryorder = "total ascending",showgrid = FALSE))
      })
    })

    ######Filtre Maison mère
    observeEvent(event_data("plotly_click", source = "maisonSelect"), {
      # On récupère les maison mères avec la donnée y
      selected_maison(event_data("plotly_click", source = "maisonSelect")$y)
      selected_region(NULL) #Pour pouvoir selectioné une maison mère apres qu'une région a été selectioné
      #Boîte de dialogue contenant un graph plotly des sites selon une maison mère
      showModal(modalDialog(
        plotlyOutput(ns("modalPlot")),
        size = "m",
        easyClose = TRUE,
        style = "background-color: #ecf0f5;",
        footer = modalButton("Fermer")
      ))
      output$modalPlot <- renderPlotly({
        data = updatedData()
        data_maison <- data %>%
          filter(Maison.Mere == selected_maison()) %>%
          mutate(
            TotalTonnageMaison = sum(Tonnages.DIM, na.rm = TRUE),
            Proportion = Tonnages.DIM / TotalTonnageMaison
          ) %>%
          arrange(desc(Proportion))
        scaled_tonnage <- scales::rescale(data_maison$Tonnages.DIM, to = c(0, 1))
        colors <- colorRampPalette(c("#0c8fce", "#0a5f9c", "#08306b"))(100)[as.integer(scaled_tonnage * 99) + 1]
        plot_ly(data = data_maison, x = ~Tonnages.DIM, y = ~Site, type = 'bar', orientation = 'h', marker = list(color = colors),
                hovertext = ~paste(Site, ": ", Tonnages.DIM, "tonnes",
                                   '<br>Proportion:', round(Proportion,3)*100,"%"),
                hoverinfo = 'text') %>%
          layout(title = paste("Tonnages des Sites pour", selected_maison()),paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5',xaxis = list(title = "Tonnages DIM"),
                 yaxis = list(title = "Site",categoryorder = "total ascending",showgrid = FALSE))
      })
    })
    #data frame tibble pour le graphique en secteur
    proportions_compacteurs <- reactive({
      updatedData() %>%
        #Si compacteur est 1 updatedData prend la valeur Avec Compacteur sinon Sans Compacteur
        mutate(Compacteur = ifelse(Compacteur == 1, "Avec Compacteur", "Sans Compacteur")) %>%
        group_by(Compacteur) %>%
        summarise(Count = dplyr::n()) %>%
        mutate(Proportion = Count / sum(Count))
    })

    #data frame tibble pour le premier graphique en barre de la liste déroulante
    proportions <- reactive({
      updatedData() %>%
        group_by(classe) %>%
        summarise(Count = dplyr::n(),
                  ProportionCompacteur = mean(Compacteur),
                  MinTonnage = min(Tonnages.DIM, na.rm = TRUE),
                  MaxTonnage = max(Tonnages.DIM, na.rm = TRUE)) %>%
        mutate(
          #Donne les bornes selon les classes.
          class_name = dplyr::case_when(
            classe == 1 ~ paste("\u2264", r$classe$bornes1, " tonnes"),
            classe == 2 ~ paste("[", r$classe$bornes1, ";", r$classe$bornes2, ") tonnes"),
            classe == 3 ~ paste("[", r$classe$bornes2, ";", r$classe$bornes3, ") tonnes"),
            classe == 4 ~ paste("[", r$classe$bornes3, ";", r$classe$bornes4, ") tonnes"),
            classe == 5 ~ paste("\u2265", r$classe$bornes4, " tonnes")
          )
        )
    })

    output$pie <- renderPlotly({

      pieCy(proportions_compacteurs())
    })

    output$mainPlot <- renderPlotly({
      data <- updatedData()
      data$IsOutreMer <- isOutreMer(data)

      sites_outre_mer <- summarise_data(data %>% filter(IsOutreMer), classe, r)
      total_tonnage_par_classe <- summarise_data(data, classe, r)
      total_tonnage_par_Region <- summarise_data(data, Region, r)
      total_tonnage_par_Maison <- summarise_data(data, Maison.Mere, r)
      #Si le graphique en secteur a été cliqué
      if(!is.null(selectedCompacteur())){
        data_filtered <- data %>%
          filter(Compacteur == ifelse(selectedCompacteur() == "Avec Compacteur", 1, 0))
        #filtré les donnée selon compacteur ou non

        total_tonnage_par_classe <- summarise_data(data_filtered, classe, r)
        sites_outre_mer <- summarise_data(data_filtered %>% filter(IsOutreMer), classe, r)
        total_tonnage_par_Region <- summarise_data(data_filtered, Region, r)
        total_tonnage_par_Maison <- summarise_data(data_filtered, Maison.Mere, r)
      }
      # Création gradient de couleur bleu.
      scaled_tonnageM <- scales::rescale(total_tonnage_par_Maison$TotalTonnage, to = c(0, 1))
      colorsM <- colorRampPalette(c("#0c8fce", "#0a5f9c", "#08306b"))(100)[as.integer(scaled_tonnageM * 99) + 1]
      scaled_tonnageR <- scales::rescale(total_tonnage_par_Region$TotalTonnage, to = c(0, 1))
      colorsR <- colorRampPalette(c("#0c8fce", "#0a5f9c", "#08306b"))(100)[as.integer(scaled_tonnageR * 99) + 1]


      BarCy(input$viewType, colorsM, colorsR, proportions(), total_tonnage_par_classe, total_tonnage_par_Region, total_tonnage_par_Maison, sites_outre_mer)
    })


    output$scatterPlot <- renderPlotly({
      data <- updatedData()
      scatterCy(data,selected_maison(),selected_region(),selectedCompacteur())

    })


    output$boxPlot <- renderPlotly({
      data <- updatedData()
      BoxCy(data,selected_region(),selected_maison(),selectedCompacteur())

    })
    observeEvent(input$reset_btn, {
      selected_region(NULL)
      selected_maison(NULL)
      selectedCompacteur(NULL)
    })

  })
}

## To be copied in the UI
# mod_TabVisu_ui("TabVisu_1")

## To be copied in the server
# mod_TabVisu_server("TabVisu_1")
