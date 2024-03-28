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
            column(width = 12,
                   div(style = "text-align: right;margin-bottom: 10px;", # Aligner les contrôles à droite
                       pickerInput(ns("viewType"),
                                   "Type de Vue:",
                                   choices = c("Proportion de Compacteurs par Classe",
                                               "Tonnage par classe",
                                               "Tonnage par Régions",
                                               "Tonnage par Maison Mère",
                                               "Nombre de Sites en Outre-Mer par Classe"),
                                   options = pickerOptions(size=15, style = "custom-dropdown")),
                       actionBttn(ns("reset_btn"), "Réinitialiser la sélection",size="xs",style = "unite")
                   )
            )
          ),
          # Graphiques
          fluidRow(
            column(
              width = 6,
              div(
                class = "custom-div",
                plotlyOutput(ns("mainPlot"), height = "200px")
              )
            )
            ,
            column(
              width = 6,
              div(
                class = "custom-div",
                plotlyOutput(ns("pie"), height = "200px")
              )
            )
          ),

          fluidRow(
            column(
              width = 6,
              div(
                class = "custom-div",
                plotlyOutput(ns("scatterPlot"), height = "200px")
              )
            )
            ,
            column(
              width = 6,
              div(
                class = "custom-div",
                plotlyOutput(ns("boxPlot"), height = "200px")
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
    updatedData <- reactive({
      data_copy <- r$data
      data_copy$classe <- NA
      data_copy$classe <- sapply(data_copy$Site, function(site) {
        if (site %in% r$classe$classe1) {
          return(1)
        } else if (site %in% r$classe$classe2) {
          return(2)
        } else if (site %in% r$classe$classe3) {
          return(3)
        } else if (site %in% r$classe$classe4) {
          return(4)
        } else if (site %in% r$classe$classe5) {
          return(5)
        } else {
          return(NA)
        }
      })
      data_copy
    })

    selected_region <- reactiveVal()
    selected_maison <- reactiveVal()
    selectedCompacteur <- reactiveVal()

    observeEvent(event_data("plotly_click", source = "pieChart"), {
      selectedCompacteur(event_data("plotly_click", source = "pieChart")$key)

    })

    observeEvent(event_data("plotly_click", source = "regionSelect"), {
      selected_region(event_data("plotly_click", source = "regionSelect")$y)
      showModal(modalDialog(
        title = paste("Tonnages des Sites pour", selected_region()),
        plotlyOutput(ns("modalPlot")),
        size = "m",
        easyClose = TRUE,
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


        plot_ly(data = data_region, x = ~Tonnages.DIM, y = ~Site, type = 'bar', orientation = 'h',
                hovertext = ~paste(Site, ": ", Tonnages.DIM, "tonnes",
                                   '<br>Proportion:', round(Proportion,3)*100,"%"),
                hoverinfo = 'text') %>%
          layout(title = paste("Tonnages des Sites pour", selected_region()),xaxis = list(title = "Tonnages DIM"),
                 yaxis = list(title = "Site",categoryorder = "total ascending",showgrid = FALSE))
      })
    })


    observeEvent(event_data("plotly_click", source = "maisonSelect"), {
      selected_maison(event_data("plotly_click", source = "maisonSelect")$y)
      showModal(modalDialog(
        title = paste("Tonnages des Sites pour", selected_maison()),
        plotlyOutput(ns("modalPlot")),
        size = "m",
        easyClose = TRUE,
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

        plot_ly(data = data_maison, x = ~Tonnages.DIM, y = ~Site, type = 'bar', orientation = 'h',
                hovertext = ~paste(Site, ": ", Tonnages.DIM, "tonnes",
                                   '<br>Proportion:', round(Proportion,3)*100,"%"),
                hoverinfo = 'text') %>%
          layout(title = paste("Tonnages des Sites pour", selected_maison()),xaxis = list(title = "Tonnages DIM"),
                 yaxis = list(title = "Site",categoryorder = "total ascending",showgrid = FALSE))
      })
    })
    proportions_compacteurs <- reactive({
      updatedData() %>%
        mutate(Compacteur = ifelse(Compacteur == 1, "Avec Compacteur", "Sans Compacteur")) %>%
        group_by(Compacteur) %>%
        summarise(Count = dplyr::n()) %>%
        mutate(Proportion = Count / sum(Count))
    })

    proportions <- reactive({
      updatedData() %>%
        group_by(classe) %>%
        summarise(Count = dplyr::n(),
                  ProportionCompacteur = mean(Compacteur),
                  MinTonnage = min(Tonnages.DIM, na.rm = TRUE),
                  MaxTonnage = max(Tonnages.DIM, na.rm = TRUE)) %>%
        mutate(
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
      plot_ly(proportions_compacteurs(), labels = ~Compacteur, values = ~Count, type = 'pie', source = "pieChart",key = ~Compacteur,
              hoverinfo = "text",
              text = ~Compacteur,
              hovertext = ~paste("<b> Compacteur :",Compacteur,"<br>",
                                 "Nombre :",Count,"<br>",
                                 "Pourcentage: ", round(Proportion,3)*100,"%"),
              textposition = 'inside',
              insidetextorientation = 'radial',
              marker = list(colors = c('Avec Compacteur' = 'red', 'Sans Compacteur' = 'blue'))
      ) %>%
        layout(title = 'Proportion de Compacteurs',paper_bgcolor = '#ecf0f5',
               margin = list(l = 0, r = 0, b = 5, t = 50),showlegend = FALSE )
    })
    output$mainPlot <- renderPlotly({
      data <- updatedData()
      data$IsOutreMer <- isOutreMer(data)
      sites_outre_mer <- data %>%
        filter(IsOutreMer) %>%
        group_by(classe)%>%
        summarise(
          TotalTonnage = sum(Tonnages.DIM, na.rm = TRUE),
          MinTonnage = min(Tonnages.DIM, na.rm = TRUE),
          MaxTonnage = max(Tonnages.DIM, na.rm = TRUE),
          NombreSites = dplyr::n()
        )%>%
        mutate(
          class_name = dplyr::case_when(
            classe == 1 ~ paste("\u2264", r$classe$bornes1, " tonnes"),
            classe == 2 ~ paste("[", r$classe$bornes1, ";", r$classe$bornes2, ") tonnes"),
            classe == 3 ~ paste("[", r$classe$bornes2, ";", r$classe$bornes3, ") tonnes"),
            classe == 4 ~ paste("[", r$classe$bornes3, ";", r$classe$bornes4, ") tonnes"),
            classe == 5 ~ paste("\u2265", r$classe$bornes4, " tonnes")
          ),
          Proportion = TotalTonnage / sum(TotalTonnage)
        )


      total_tonnage_par_classe <- data %>%
        group_by(classe) %>%
        summarise(
          TotalTonnage = sum(Tonnages.DIM, na.rm = TRUE),
          MinTonnage = min(Tonnages.DIM, na.rm = TRUE),
          MaxTonnage = max(Tonnages.DIM, na.rm = TRUE),
          NombreSites = dplyr::n()
        ) %>%
        mutate(
          class_name = dplyr::case_when(
            classe == 1 ~ paste("\u2264", r$classe$bornes1, " tonnes"),
            classe == 2 ~ paste("[", r$classe$bornes1, ";", r$classe$bornes2, ") tonnes"),
            classe == 3 ~ paste("[", r$classe$bornes2, ";", r$classe$bornes3, ") tonnes"),
            classe == 4 ~ paste("[", r$classe$bornes3, ";", r$classe$bornes4, ") tonnes"),
            classe == 5 ~ paste("\u2265", r$classe$bornes4, " tonnes")
          ),
          Proportion = TotalTonnage / sum(TotalTonnage)
        )



      total_tonnage_par_Region <- data %>%
        group_by(Region) %>%
        summarise(TotalTonnage = sum(Tonnages.DIM, na.rm = TRUE),
                  NombreSites = dplyr::n()
        ) %>%
        arrange(TotalTonnage)%>%
        mutate(
          Proportion = TotalTonnage / sum(TotalTonnage),
        )

      total_tonnage_par_Maison <- data %>%
        group_by(Maison.Mere) %>%
        summarise(
          TotalTonnage = sum(Tonnages.DIM, na.rm = TRUE),
          NombreSites = dplyr::n()
        ) %>%
        arrange(TotalTonnage)%>%
        mutate(
          Proportion = TotalTonnage / sum(TotalTonnage)
        )

      if(!is.null(selectedCompacteur())){
        data_filtered <- data %>% filter(Compacteur == ifelse(selectedCompacteur() == "Avec Compacteur", 1, 0))

        total_tonnage_par_classe <- data_filtered %>%
          group_by(classe) %>%
          summarise(
            TotalTonnage = sum(Tonnages.DIM, na.rm = TRUE),
            MinTonnage = min(Tonnages.DIM, na.rm = TRUE),
            MaxTonnage = max(Tonnages.DIM, na.rm = TRUE),
            NombreSites = dplyr::n()
          ) %>%
          mutate(
            class_name = dplyr::case_when(
              classe == 1 ~ paste("\u2264", r$classe$bornes1, " tonnes"),
              classe == 2 ~ paste("[", r$classe$bornes1, ";", r$classe$bornes2, ") tonnes"),
              classe == 3 ~ paste("[", r$classe$bornes2, ";", r$classe$bornes3, ") tonnes"),
              classe == 4 ~ paste("[", r$classe$bornes3, ";", r$classe$bornes4, ") tonnes"),
              classe == 5 ~ paste("\u2265", r$classe$bornes4, " tonnes")
            ),
            Proportion = TotalTonnage / sum(TotalTonnage)
          )
        sites_outre_mer <- data_filtered %>%
          filter(IsOutreMer) %>%
          group_by(classe)%>%
          summarise(
            TotalTonnage = sum(Tonnages.DIM, na.rm = TRUE),
            MinTonnage = min(Tonnages.DIM, na.rm = TRUE),
            MaxTonnage = max(Tonnages.DIM, na.rm = TRUE),
            NombreSites = dplyr::n()
          )%>%
          mutate(
            class_name = dplyr::case_when(
              classe == 1 ~ paste("\u2264", r$classe$bornes1, " tonnes"),
              classe == 2 ~ paste("[", r$classe$bornes1, ";", r$classe$bornes2, ") tonnes"),
              classe == 3 ~ paste("[", r$classe$bornes2, ";", r$classe$bornes3, ") tonnes"),
              classe == 4 ~ paste("[", r$classe$bornes3, ";", r$classe$bornes4, ") tonnes"),
              classe == 5 ~ paste("\u2265", r$classe$bornes4, " tonnes")
            ),
            Proportion = TotalTonnage / sum(TotalTonnage)
          )

        total_tonnage_par_Region <- data_filtered %>%
          group_by(Region) %>%
          summarise(TotalTonnage = sum(Tonnages.DIM, na.rm = TRUE),
                    NombreSites =  dplyr::n()
          ) %>%
          arrange(TotalTonnage)%>%
          mutate(
            Proportion = TotalTonnage / sum(TotalTonnage),
          )

        total_tonnage_par_Maison <- data_filtered %>%
          group_by(Maison.Mere) %>%
          summarise(
            TotalTonnage = sum(Tonnages.DIM, na.rm = TRUE),
            NombreSites =  dplyr::n()
          ) %>%
          arrange(TotalTonnage)%>%
          mutate(
            Proportion = TotalTonnage / sum(TotalTonnage)
          )

      }
      switch(input$viewType,
             "Proportion de Compacteurs par Classe" = {
               plot_ly(data = proportions(), x = ~classe, y = ~ProportionCompacteur, type = 'bar', name = 'ProportionCompacteur',
                       hovertext = ~paste('Intervalle de tonnage:',class_name,'<br>',
                                          'Nombre de Sites:', Count,
                                          '<br>Proportion de compacteur :', round(ProportionCompacteur,3)*100,"%"),
                       hoverinfo = 'text')%>%
                 layout(margin = list(l = 50, r = 0, b = 5, t = 50),paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5',title = "Proportion de Compacteurs par Classe", xaxis = list(title = "Classe",categoryorder = "total ascending"), yaxis = list(title = "Proportion compacteur",showline = TRUE,showgrid = FALSE))
             },
             "Tonnage par classe" = {
               plot_ly(data = total_tonnage_par_classe, x = ~classe, y = ~TotalTonnage, type = 'bar', name = 'Tonnage par classe'
                       ,hovertext = ~paste('Intervalle de tonnage:',class_name,'<br>',
                                           'Tonnage Total:', TotalTonnage, 'tonnes<br>',
                                           'Nombre de Sites:', NombreSites,
                                           '<br>Proportion du tonnage total :', round(Proportion,3)*100,"%"),
                       hoverinfo = 'text') %>%
                 layout(margin = list( r = 0, b = 5, t = 50),paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5',title = "Tonnage par classe",xaxis = list(title = "Classe",categoryorder = "total ascending"),
                        yaxis = list(title = "Tonnages DIM tonnes",showgrid = FALSE,showline = TRUE))
             },
             "Tonnage par Régions" = {
               plot_ly(data = total_tonnage_par_Region, x = ~TotalTonnage, y = ~Region, type = 'bar', orientation = 'h', name = 'Tonnage par Région'
                       ,hovertext = ~paste('Nombre de Sites:', NombreSites,
                                           '<br>Proportion du tonnage total:', round(Proportion,3)*100,"%"),
                       hoverinfo = 'text', source = "regionSelect") %>%
                 layout(margin = list(r = 0, b = 5, t = 50),paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5',title = "Tonnage par Région",xaxis = list(title = "Tonnages DIM"),
                        yaxis = list(title = "Région",showgrid = FALSE,categoryorder = "total ascending"))
             },
             "Tonnage par Maison Mère" = {
               plot_ly(data = total_tonnage_par_Maison, x = ~TotalTonnage, y = ~Maison.Mere, type = 'bar', orientation = 'h', name = 'Tonnage par Maison Mère',
                       hovertext = ~paste('Classe:[',total_tonnage_par_classe$MinTonnage, ',', total_tonnage_par_classe$MaxTonnage,'] tonnes<br>',
                                          'Nombre de Sites:', NombreSites,
                                          '<br>Proportion de tonnage total :', round(Proportion,3)*100,"%"),
                       hoverinfo = 'text', source = "maisonSelect") %>%
                 layout(margin = list(l = 100,r = 0, b = 5, t = 50),paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5',title = "Tonnage par Maison Mère",xaxis = list(title = "Tonnages DIM"),
                        yaxis = list(title = "Maison Mère",showline = TRUE,showgrid = FALSE,categoryorder = "total ascending"))
             },
             "Nombre de Sites en Outre-Mer par Classe" = {
               plot_ly(data = sites_outre_mer, x = ~classe, y = ~NombreSites, type = 'bar', name = 'Nombre de Sites en Outre-Mer',
                       hovertext = ~paste('Intervalle de tonnage:',class_name,'<br>',
                                          'Total tonnage :',TotalTonnage,'tonnes<br>',
                                          'Nombre de Sites:', NombreSites,
                                          '<br>Proportion de site:', round(Proportion,3)*100,"%"),
                       hoverinfo = 'text') %>%
                 layout(margin = list(l = 50, r = 0, b = 5, t = 50),paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5',title = "Nombre de Sites en Outre-Mer par Classe",xaxis = list(title = "classe"),
                        yaxis = list(title = "site",showgrid = FALSE,showline = TRUE))
             }
      )
    })
    output$scatterPlot <- renderPlotly({
      data <- updatedData()
      moyenne_DIM <- mean(data$Tonnages.DIM, na.rm = TRUE)
      moyenne_DIM_pas_compacteur <- mean(data$Tonnages.DIM[data$Compacteur == 0], na.rm = TRUE)
      moyenne_DIM_compacteur <- mean(data$Tonnages.DIM[data$Compacteur == 1], na.rm = TRUE)

      data_avec_compacteur <- data %>%
        filter(Compacteur == 1)

      data_sans_compacteur <- data %>%
        filter(Compacteur == 0)

      if (!is.null(selected_region())) {
        data_filteredA <- data_avec_compacteur %>% filter(Region == selected_region())
        data_filteredS <- data_sans_compacteur %>% filter(Region == selected_region())
      }
      else if (!is.null(selected_maison())) {
        data_filteredA <- data_avec_compacteur %>% filter(Maison.Mere == selected_maison())
        data_filteredS <- data_sans_compacteur %>% filter(Maison.Mere == selected_maison())
      }
      else {
        data_filteredA <- data_avec_compacteur
        data_filteredS <- data_sans_compacteur
      }
      if(!is.null(selectedCompacteur())){
        data_filteredA <- data_filteredA %>% filter(Compacteur == ifelse(selectedCompacteur() == "Avec Compacteur", 1, 0))
        data_filteredS <- data_filteredS %>% filter(Compacteur == ifelse(selectedCompacteur() == "Avec Compacteur", 1, 0))
      }
      p_scatter <- plot_ly() %>%

        add_trace(data = data_filteredA, x = ~Nbre.de.rotation, y = ~Tonnages.DIM,
                  type = "scatter", mode = "markers",
                  text = ~paste("Site:", data_filteredA$Site, "<br>Tonnages DIM:", data_filteredA$Tonnages.DIM,"<br>Compacteur: Avec"),
                  hoverinfo = "text",
                  marker = list(size = 4, color = 'red', symbol = 'circle'),
                  name = "Avec Compacteur") %>%
        add_trace(data = data_filteredS, x = ~Nbre.de.rotation, y = ~Tonnages.DIM,
                  text = ~paste("Site:", data_filteredS$Site, "<br>Tonnages DIM:", data_filteredS$Tonnages.DIM,"<br>Compacteur: Sans"),
                  hoverinfo = "text",
                  marker = list(size = 4, color = 'blue', symbol = 'circle'),
                  name = "Sans Compacteur")
      p_scatter <- add_trace(p_scatter, x = c(0, max(data$Nbre.de.rotation, na.rm = TRUE)), y = rep(moyenne_DIM, 2),
                             type = "scatter", mode = "lines",
                             line = list(dash = 'dot', color = 'black', width = 2),
                             name = "Moyenne DIM",
                             hoverinfo = "none") %>%
        add_trace(x = c(0, max(data$Nbre.de.rotation, na.rm = TRUE)), y = rep(moyenne_DIM, 2),
                  type = "scatter", mode = "lines",
                  line = list(dash = 'dot', color = 'red', width = 2),
                  name = "Moyenne avec Compacteur",
                  hoverinfo = "none") %>%
        add_trace(x = c(0, max(data$Nbre.de.rotation, na.rm = TRUE)), y = rep(moyenne_DIM, 2),
                  type = "scatter", mode = "lines",
                  line = list(dash = 'dot', color = 'blue', width = 2),
                  name = "Moyenne sans Compacteur",
                  hoverinfo = "none")
      if (!is.null(selected_region())) {
        p_scatter <- layout(p_scatter,margin = list(l = 50, r = 0, b = 5, t = 50),paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5',
                            yaxis = list(title = "Tonnage",showline = TRUE,range = c(0, max(data$Tonnages.DIM)+10)), xaxis = list(title = "Nombres rotation",range = c(0, max(data$Nbre.de.rotation)+10))
                            ,title =list( text = ~paste("Tonnage DIM en fonction du nombre de rotations \nDétails pour la région:", selected_region())
                                          ,font = list(size = 15), xanchor = "center",
                                          yanchor = "top"),
                            legend = list(
                              orientation = "v",
                              x = 1,
                              y = 1,
                              xanchor = 'right',
                              yanchor = 'right',
                              font = list(size = 8)
                            ))
      }
      else if (!is.null(selected_maison())){
        p_scatter <- layout(p_scatter,paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5', yaxis = list(title = "Tonnage",showline = TRUE,range = c(0, max(data$Tonnages.DIM)+10)), xaxis = list(title = "Nombres rotations",range = c(0, max(data$Nbre.de.rotation)+10))
                            ,title =list( text = ~paste("Tonnage DIM en fonction du nombre de rotations\nDétails pour la maison mère:", selected_maison())
                                          ,font = list(size = 15), xanchor = "center",
                                          yanchor = "top"),
                            margin = list(l = 50, r = 0, b = 5, t = 50),
                            legend = list(
                              orientation = "v",
                              x = 1,
                              y = 1,
                              xanchor = 'right',
                              yanchor = 'right',
                              font = list(size = 8)
                            ))
      }
      else{
        p_scatter <- layout(p_scatter,paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5',
                            yaxis = list(title = "Tonnage",showline = TRUE,range = c(0, max(data$Tonnages.DIM)+10)),
                            xaxis = list(title = "Nombres rotations",range = c(0, max(data$Nbre.de.rotation)+10))
                            ,title =list(text = ~paste("Tonnages DIM en fonction du nombre de rotations")
                                         ,font = list(size = 15), xanchor = "center",
                                         yanchor = "top")
                            ,margin = list(l = 50, r = 0, b = 5, t = 50)
                            ,legend = list(
                              orientation = "v",
                              x = 1,
                              y = 1,
                              xanchor = 'right',
                              yanchor = 'right',
                              font = list(size = 8)
                            ))
      }
      p_scatter
    })


    output$boxPlot <- renderPlotly({
      data <- updatedData()
      start_color <- "#00FF00" #Lime
      end_color <-"#006400"  #DarkGreen
      couleurs_verts_fonces <- colorRampPalette(c(start_color, end_color))
      palette_verte <- couleurs_verts_fonces(length(unique(data$classe)))
      datab <-  data %>%
        group_by(classe) %>%
        mutate(
          NombreSites = dplyr::n(),
          MinTonnage = min(Tonnages.DIM, na.rm = TRUE),
          MaxTonnage = max(Tonnages.DIM, na.rm = TRUE)
        ) %>%
        mutate(
          class_name = dplyr::case_when(
            classe == 1 ~ paste("\u2264", r$classe$bornes1, " tonnes"),
            classe == 2 ~ paste("[", r$classe$bornes1, ";", r$classe$bornes2, ") tonnes"),
            classe == 3 ~ paste("[", r$classe$bornes2, ";", r$classe$bornes3, ") tonnes"),
            classe == 4 ~ paste("[", r$classe$bornes3, ";", r$classe$bornes4, ") tonnes"),
            classe == 5 ~ paste("\u2265", r$classe$bornes4, " tonnes")
          )
        )
      if (!is.null(selected_region())) {
        data_filtered <- datab %>% filter(Region == selected_region())
      }
      else if (!is.null(selected_maison())) {
        data_filtered <- datab %>% filter(Maison.Mere == selected_maison())
      }
      else {
        data_filtered <- datab
      }
      if(!is.null(selectedCompacteur())){
        data_filtered <- data_filtered %>% filter(Compacteur == ifelse(selectedCompacteur() == "Avec Compacteur", 1, 0))
      }
      last_class <- max(data_filtered$classe)
      data_last_class <- filter(data_filtered, classe == last_class)
      Q1 <- quantile(data_last_class$Tonnages.DIM, 0.25)
      Q3 <- quantile(data_last_class$Tonnages.DIM, 0.75)
      IQR <- Q3 - Q1
      upper_fence <- Q3 + 1.5 * IQR
      data_last_class$outlier <- ifelse(data_last_class$Tonnages.DIM > upper_fence, TRUE, FALSE)


      p <- plot_ly(data = data_filtered, y = ~Tonnages.DIM, x = ~factor(classe), type = "box",
                   color = ~factor(classe), colors =  palette_verte)

      p <- add_trace(p, data = data_last_class[data_last_class$outlier == TRUE, ], y = ~Tonnages.DIM,
                     x = ~factor(classe), type = 'scatter', mode = 'markers',
                     marker = list(color = 'red', size = 5), hoverinfo = 'text',
                     text = ~paste("Site:", Site, "<br>Tonnage:", Tonnages.DIM)
      )
      if (!is.null(selected_region())) {
        p <- layout(p,paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5', title = paste("Boxplot du tonnage des classes \nDétails pour la région:", selected_region()),
                    yaxis = list(title = "Tonnage",showline = TRUE,showgrid = FALSE), xaxis = list(title = "Classe",categoryorder = "total ascending"),
                    margin = list(l = 50, r = 0, b = 5, t = 50))
      }
      else if (!is.null(selected_maison())){
        p <- layout(p,paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5', title = paste("Boxplot du tonnage des classes \nDétails pour la maison mère:", selected_maison()),
                    yaxis = list(title = "Tonnage",showline = TRUE,showgrid = FALSE,), xaxis = list(title = "Classe",categoryorder = "total ascending"),
                    margin = list(l = 50, r = 0, b = 5, t = 50))
      }
      else{
        p <- layout(p,margin = list(l = 50, r = 0, b = 5, t = 50), title = "Boxplot du tonnage des classes",paper_bgcolor = '#ecf0f5',plot_bgcolor = '#ecf0f5',
                    yaxis = list(title = "Tonnage",showline = TRUE,showgrid = FALSE,range = c(0, max(data$Tonnages.DIM)+10)), xaxis = list(title = "Classe",categoryorder = "total ascending")
        )
      }
      p
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
