#' TabData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TabData_ui <- function(id){
  ns <- NS(id)
  tabItem(tabName = "data",
          sidebarPanel(
            actionBttn(inputId = "guide1",label = "Guide", style = "stretch",color = "primary"),
            # fileInput(
            #   inputId = "file",
            #   label = "Choisir un fichier CSV",
            #   accept = c(".csv", ".tsv", ".txt"),
            #   multiple = FALSE,
            #   buttonLabel = "Sélectionner un fichier",
            #   placeholder = "Aucun fichier sélectionné"
            # ),
            dropdown(inputId = "modifyDataButton",
                     animate = TRUE,
                     style = "unite",
                     tags$h3("Remplissez les champs suivants:"),
                     selectInput(ns("c1"),"Région:",choices=levels(as.factor(Tonnage$Region))),
                     selectInput(ns("c2"),"Maison mère:",choices = NULL),
                     textInput(ns("c3"),"Site:"),
                     numericInput(ns("c4"),"Tonnages dim",value=50,min=0,max=300),
                     numericInput(ns("c5"),"Nombre de rotations:",value=20,min=0,max=100),
                     selectInput(ns("c6"),"Type de stockage:",choices=c("conteneur/benne","compacteur")),
                     actionBttn(inputId = ns("newRow"),label = "Nouvelle ligne", style = "unite",size = "sm",color = "royal"),
                     circle = TRUE, status = "danger",
                     icon = icon("table"),
                     tooltip = tooltipOptions(title = "Cliquer pour ajouter une nouvelle ligne")
            )
          ),
          mainPanel(
            h1("Tonnages des DIM"),
            reactableOutput(ns('table_data'),width = "900px"),
            #downloadButton('save_data', 'Save to CSV')
            downloadBttn(outputId = ns("save_data"),label = "Save to CSV",
                         color = "success",size="xs",style="gradient")
          )
  )
}

#' TabData Server Functions
#'
#' @noRd
mod_TabData_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####### esthetique de la data #########
    theme <-reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px",
      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
      searchInputStyle = list(width = "100%")
    )
    output$table_data <- renderReactable({
      reactable(r$data,theme = theme,striped = TRUE,bordered = TRUE,
                columns = list(Compacteur = colDef(align = "center", filterable = FALSE,cell = function(value) {
                  if (value == 0) "\u274c false" else "\u2714\ufe0f true"}),
                  Tonnages.DIM = colDef(cell = data_bars(r$data,text_position = "above",fill_color = viridis::mako(4),fill_gradient = TRUE,round_edges = TRUE,bar_height = 15))
                ),
                fullWidth = TRUE,defaultColDef = colDef(style = "font-style: italic;"),searchable = TRUE,
                filterable = TRUE,highlight = TRUE, showPageSizeOptions = TRUE,defaultPageSize = 10,pageSizeOptions = c(10, 50, 100),selection = "multiple",showSortIcon = TRUE)
    })

    ######## ajouter une ligne dans la data #########
    observeEvent(input$newRow,{
      confirmSweetAlert(
        session = session, inputId = "myconfirmation", type = "info",
        title = "êtes vous sur de vouloir rajouter cette nouvelle ligne?", danger_mode = TRUE
      )

    })

    observeEvent(input$myconfirmation, {
      if (isTRUE(input$myconfirmation)) {
        currentData <- r$data
        newRow <- data.frame(Region=input$c1,Maison.Mere=input$c2,Site=input$c3,Tonnages.DIM=input$c4,Nbre.de.rotation=input$c5,Compacteur=ifelse(input$c6=="compacteur",1,0))
        currentData <- rbind(newRow,currentData)
        r$data<-currentData
      }
    })


    #sauvegarde de data au format csv
    output$save_data <- downloadHandler(
      filename <- function(){
        paste("data",Sys.Date(), ".csv", sep = ',')
      },
      content <- function(file){
        write.csv(r$data,file)
      }
    )

  })
}

## To be copied in the UI
# mod_TabData_ui("TabData_1")

## To be copied in the server
# mod_TabData_server("TabData_1")
