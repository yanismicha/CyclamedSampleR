#' TabData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TabData_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "data",
          fluidPage(
            tags$head(
              tags$style(HTML("
    /* CSS pour améliorer le tableau DT */
    .dataTable {
      border-collapse: collapse;
    }
    table.dataTable thead th, table.dataTable tbody td {
      border: 1px solid #ddd;
    }
    table.dataTable thead th {
      background-color: #f9f9f9;
    }
    /* Ajout de bordure et ombre pour la rangée éditable */
    table.dataTable tr.shiny-input-container {
      border: 2px solid #ccc;
      box-shadow: 0 2px 3px #ccc;
    }
    /* Style pour les boutons */
    .btn {
      margin-right: 10px;
    }
    /* Styles personnalisés pour les notifications */
    .shiny-notification {
      background-color: #cccccc !important; /* Couleur de fond gris */
      color: black; /* Couleur de texte */
      border-left-color: #999999 !important; /* Couleur de la bordure à gauche */
    }
  "))
            ),
            fluidRow(
              column(width = 1,
                     div(style = "margin-top: 15rem;",
                         dropdown(inputId = ns("modifyDataButton"),
                                  animate = TRUE, style = "pill", color = "success",
                                  tags$h4("Veuillez remplir chaque champ avant de rajouter une nouvelle ligne :"),
                                  selectInput(ns("c1"), "Région:", choices = levels(as.factor(Tonnage$Region))),
                                  selectizeInput(ns("c2"), "Maison mère:", choices = levels(as.factor(Tonnage$Maison.Mere)), options = list(create = TRUE, placeholder = 'Choisissez ou ajoutez')),
                                  textInput(ns("c3"), "Site:", placeholder = "Entrez le nom du site"),
                                  numericInput(ns("c4"), "Tonnages DIM", value = 0, min = 0),
                                  numericInput(ns("c5"), "Nombre de rotations:", value = 0, min = 0),
                                  selectInput(ns("c6"), "Type de stockage:", choices = c("Conteneur/Benne", "Compacteur")),
                                  actionBttn(inputId = ns("newRow"), label = "Nouvelle ligne", style = "unite", size = "sm", color = "success"),
                                  circle = TRUE, status = "success",
                                  icon = icon("plus"),
                                  tooltip = tooltipOptions(title = "Cliquer pour ajouter un nouveau site")
                         )
                     )
              ),
              column(width = 7,
                     h1("Tableaux des grossistes répartiteurs"),
                     DTOutput(ns('table_data'), width = "100%"),
                     actionButton(ns("edit_btn"), "Edit Selected Row", icon = icon("edit"), class = "btn-success"),
                     actionButton(ns("deleteBtn"), "Delete Selected Rows", icon = icon("trash"), class = "btn-danger")
              )
            ),
            fluidRow(
              column(width = 9,
                     div(style = "text-align: right;",
                         downloadBttn(outputId = ns("save_data"), label = "Sauvegarder en CSV", color = "success", size = "md", style = "gradient")
                     )
              )
            )
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
    output$table_data <- DT::renderDataTable({
      DT::datatable(
        data = r$data,
        editable = TRUE,
        selection = 'multiple',
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 50, 100),
          autoWidth = TRUE,
          searchHighlight = TRUE,
          columnDefs = list(
            list(targets = c(5), # Indice de la colonne 'Compacteur'
                 render = JS(
                   "function(data, type, row) {",
                   "  return data == 0 ? '\u274c false' : '\u2714\ufe0f true';",
                   "}"
                 )),
            list(targets = "_all",
                 className = "dt-center")
          )
        ),
        filter = 'top',
        rownames = FALSE
      ) %>%
        formatStyle(
          columns = c('Tonnages.DIM'),
          backgroundColor = styleEqual(c(0, 1), c('#f6f8fa', '#dfe2e5')),
          fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
          width = "100%"
        )
    })


    ######## ajouter une ligne dans la data #########
    observeEvent(input$newRow, {
      error_message <- NULL

      if (input$c4 < 0) {
        error_message <- "La valeur du tonnages de DIM doit être positive."
      }
      if (input$c5 < 0) {
        error_message <- paste0(error_message, if (!is.null(error_message)) " " else "", "La valeur du nombre de rotations doit être positive.")
      }

      if (!is.null(error_message)) {
        showNotification(error_message, type = "error")
      } else if (isTruthy(input$c1) && isTruthy(input$c2) && isTruthy(input$c3)) {
        confirmSweetAlert(
          session = session, inputId = "myconfirmation", type = "info",
          title = "Êtes-vous sûr de vouloir rajouter cette nouvelle ligne ?",
          danger_mode = TRUE, btn_labels = c("NON", "OUI")
        )
      } else {
        showNotification("Veuillez remplir tous les champs correctement avant d'ajouter une nouvelle ligne.", type = "error")
      }
    })




    observeEvent(input$myconfirmation, {
      if (isTRUE(input$myconfirmation)) {
        currentData <- r$data
        newRow <- data.frame(Region=input$c1,Maison.Mere=input$c2,Site=input$c3,Tonnages.DIM=input$c4,Nbre.de.rotation=input$c5,Compacteur=ifelse(input$c6=="Compacteur",1,0))
        currentData <- rbind(newRow,currentData)
        r$data <- currentData
      }
    })


    observeEvent(input$deleteBtn, {
      selectedRows <- input$table_data_rows_selected
      if (length(selectedRows) > 0) {
        showModal(modalDialog(
          title = "Confirmation de la suppression",
          "Êtes-vous sûr de vouloir supprimer la sélèction ?",
          footer = tagList(
            modalButton("Annuler"),
            actionButton(ns("confirmDelete"), "Confirmer", class = "btn-primary")
          )
        ))
      }
    })

    observeEvent(input$confirmDelete, {
      removeModal()
      if (length(input$table_data_rows_selected) > 0) {
        newData <- r$data[-input$table_data_rows_selected, ]
        r$data <- newData
      }
      saveRDS(r$data,"Tonnage.rda")
      Tonnage <- readRDS("Tonnage.rda")
      usethis::use_data(Tonnage, overwrite = TRUE)
    })


    observeEvent(input$edit_btn, {
      selected <- input$table_data_rows_selected
      if (length(selected) == 1) {
        selectedData <- r$data[selected, ]
        showModal(modalDialog(
          title = "Éditer la ligne",
          textInput(ns("edit_region"), "Region", selectedData$Region),
          textInput(ns("edit_maisonMere"), "Maison Mere", selectedData$Maison.Mere),
          textInput(ns("edit_site"), "Site", selectedData$Site),
          numericInput(ns("edit_tonnages"), "Tonnages", value = selectedData$Tonnages.DIM),
          numericInput(ns("edit_rotations"), "Rotations", value = selectedData$Nbre.de.rotation),
          selectInput(ns("edit_compacteur"), "Compacteur", choices = c("Oui" = 1, "Non" = 0), selected = selectedData$Compacteur),
          footer = tagList(
            modalButton("Annuler"),
            actionButton(ns("save_edit"), "Sauvegarder", class = "btn-primary")
          )
        ))
      }

    })

    observeEvent(input$save_edit, {
      error_message <- NULL

      if (as.numeric(input$edit_tonnages) < 0) {
        error_message <- "La valeur du tonnages de DIM doit être positive."
      }
      if (as.numeric(input$edit_rotations) < 0) {
        error_message <- paste0(error_message, if (!is.null(error_message)) " " else "", "La valeur du nombre de rotations doit être positive.")
      }

      if (!is.null(error_message)) {
        showNotification(error_message, type = "error")
      } else {
        removeModal()
        selected <- input$table_data_rows_selected
        if (length(selected) == 1) {
          r$data[selected, ]$Region <- input$edit_region
          r$data[selected, ]$Maison.Mere <- input$edit_maisonMere
          r$data[selected, ]$Site <- input$edit_site
          r$data[selected, ]$Tonnages.DIM <- as.numeric(input$edit_tonnages)
          r$data[selected, ]$Nbre.de.rotation <- as.numeric(input$edit_rotations)
          r$data[selected, ]$Compacteur <- as.numeric(input$edit_compacteur)
        }

        saveRDS(r$data,"Tonnage.rda")
        Tonnage <- readRDS("Tonnage.rda")
        usethis::use_data(Tonnage, overwrite = TRUE)
      }
    })



    # Sauvegarde de data au format csv

    output$save_data <- downloadHandler(
      filename = function() {
        paste("Tonnage", Sys.Date(), ".csv", sep = '')
      },
      content = function(file) {
        write.csv(r$data(), file, row.names = FALSE)
      }
    )

  })
}


