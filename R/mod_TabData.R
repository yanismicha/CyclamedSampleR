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
  # définition de l'interface utilisateur pour afficher et interagir avec les données dans un tableau.
  ns <- NS(id)
  tabItem(tabName = "data",

          # Première rangée de l'interface utilisateur avec une marge en haut pour l'espacement.
          fluidRow(
            column(width = 1,
                   div(style = "margin-top: 15rem;",
                       # Dropdown pour ajouter de nouvelles données avec différents champs de saisie.
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
            column(
              width = 7,
              shinyjs::useShinyjs(),
              # Titre principal du tableau.
              h1("Tableaux des sites de grossistes répartiteurs"),
              div(style= "margin-bottom :17px",
                  shinyjs::hidden (
                    actionButton(ns("edit_btn"), "Éditer", icon = icon("edit"), class = "btn-success")
                  ),

                  shinyjs::hidden (
                    actionButton(ns("deleteBtn"), "Supprimer", icon = icon("trash"), class = "btn-danger")
                  )
              ),

              # Sortie du tableau de données interactif.
              DTOutput(ns('table_data'), width = "100%")
            )
          ),
          # Deuxième rangée de l'interface utilisateur pour l'import de fichiers et la sauvegarde des données.
          fluidRow(
            column(
              width = 6,
              fileInput(ns("fileInput"), "Import de nouvelles données", accept = ".csv", buttonLabel = "Parcourir...", placeholder = "Aucun fichier sélectionné")
            ),
            column(width = 4,
                   div(style = "text-align: right;",
                       downloadBttn(outputId = ns("save_data"), label = "Sauvegarder", color = "success", size = "md", style = "gradient")
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


    observe({
      # permet d'afficher ou de cacher les boutons editer et supprimer selon les séléctions.
      selected_rows <- input$table_data_rows_selected


      if (length(selected_rows) > 1) {

        shinyjs:: show("deleteBtn", anim=TRUE )

        shinyjs:: hide("edit_btn", anim=TRUE )

      }

      else if (length(selected_rows) == 1){
        shinyjs:: show("deleteBtn", anim=TRUE )

        shinyjs:: show("edit_btn", anim=TRUE )
      }

      else {
        shinyjs:: hide("deleteBtn", anim=TRUE )

        shinyjs:: hide("edit_btn", anim=TRUE )
      }


    })





    # Génère dynamiquement le tableau de données pour l'interface utilisateur.
    output$table_data <- DT::renderDataTable({
      DT::datatable(
        data = r$data,
        editable = TRUE,                # Permet l'édition des cellules du tableau.
        selection = 'multiple',         # Permet de sélectionner plusieurs lignes pour les opérations en lot.
        options = list(
          autoWidth = FALSE,
          language = list(
            info = "Site _START_ à _END_ sur un total de _TOTAL_ Sites",
            lengthMenu = "Afficher _MENU_ sites",
            search= "Recherche : ",
            paginate = list(previous = 'Précédent', `next` = 'Suivant')
          ),
          pageLength = 10,
          lengthMenu = c(10, 50, nrow(r$data)),
          searchHighlight = TRUE,
          columnDefs = list(
            list(
              targets = c(5),
              render = JS(
                "function(data, type, row) {",
                "  return data == 0 ? '\u274c' : '\u2714\ufe0f';",
                "}"
              )
            ),
            list(
              targets = "_all",
              className = "dt-center"
            )
          )
        ),
        filter = 'top',
        rownames = FALSE,
        colnames = c("Region", "Maison mere", "Site", "Tonnage de DIM", "Nombre de rotations", "Compacteur")
      ) %>%
        formatStyle(
          columns = c('Tonnages.DIM'),
          backgroundColor = styleEqual(c(0, 1), c('#f6f8fa', '#dfe2e5')),
          fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
          width = "100%"
        )
    })



    observeEvent(input$fileInput, {
      req(input$fileInput)

      tryCatch({
        newData <- read.csv(input$fileInput$datapath, stringsAsFactors = FALSE)
        lowerCaseNames <- tolower(names(newData))

        # Définition des mots-clés pour identifier les colonnes attendues.
        keywords <- list(
          region = "region|regions",
          maison.mere = "maison|mere|maison.mere",
          site = "site|sites",
          tonnages.dim = "dim|tonnages|tonnes|tonnages.dim",
          nbre.de.rotation = "rotation|rotations|nbre.de.rotation|nbre.de.rotations",
          compacteur = "compacteur|compacteurs|benne|bennes"
        )

        # Fonction pour vérifier la présence des mots-clés.
        match_columns <- function(data_names, keywords) {
          sapply(keywords, function(keyword) {
            any(grepl(keyword, data_names))
          })
        }

        matches <- match_columns(lowerCaseNames, keywords)
        if (!all(matches)) {
          missing_cols <- names(matches)[!matches]
          showNotification(
            paste("Le fichier importé manque des colonnes essentielles ou contient des erreurs de nommage:", paste(missing_cols, collapse=", ")),
            type = "error",
            duration = 10
          )
          return()
        }

        # Fonction pour identifier si une colonne correspond à une des expressions régulières
        column_matches_any_keyword <- function(column_name, keywords) {
          any(sapply(keywords, function(keyword) grepl(keyword, column_name)))
        }

        # Application de cette fonction à chaque nom de colonne et inversion du résultat pour trouver les non-correspondances
        unmatched_columns <- lowerCaseNames[!sapply(lowerCaseNames, function(name) column_matches_any_keyword(name, keywords))]

        if (length(unmatched_columns) > 0) {
          showNotification(
            paste("Le fichier importé contient des colonnes non attendues:", paste(unmatched_columns, collapse=", ")),
            type = "warning",
            duration = 10
          )
          return()
        }

        # Mise à jour des données
        names(newData) <- c("Region", "Maison.Mere", "Site", "Tonnages.DIM", "Nbre.de.rotation", "Compacteur")
        r$data <- newData
        write.csv(r$data, "Tonnage.csv", row.names = FALSE)
        Tonnage <- r$data
        usethis::use_data(Tonnage, overwrite = TRUE)

        # Notification de succès

        showNotification(
          "Le fichier a été importé avec succès et les données ont été mises à jour.",
          type = "message",
          duration = 10,
          closeButton = TRUE,
        )

      }, error = function(e) {
        showNotification("Une erreur est survenue en lisant le fichier: ", e$message, type = "error", duration = 10)
      }, warning = function(w) {
        showNotification("Message d'attention: ", w$message, type = "warning")
      })
    })


    ########  bouton pour ajouter une nouvelle ligne de données.
    observeEvent(input$newRow, {
      error_message <- NULL

      # Vérifie que la valeur entrée pour les tonnages de DIM est positive.
      if (input$c4 < 0) {
        error_message <- "La valeur du tonnages de DIM doit être positive."
      }

      # Vérifie que le nombre de rotations entré est également positif.
      if (input$c5 < 0) {
        error_message <- paste0(error_message, if (!is.null(error_message)) " " else "", "La valeur du nombre de rotations doit être positive.")
      }

      if (!is.null(error_message)) {
        showNotification(error_message, type = "error")
      } else if (isTruthy(input$c1) && isTruthy(input$c2) && isTruthy(input$c3)) {
        # Si toutes les entrées nécessaires sont valides, demande une confirmation avant d'ajouter la ligne.
        confirmSweetAlert(
          session = session,
          inputId = "myconfirmation",
          type = "info",
          title = "Êtes-vous sûr de vouloir rajouter cette nouvelle ligne ?",
          danger_mode = TRUE,
          btn_labels = c("NON", "OUI")
        )
      } else {
        # Si certains champs ne sont pas remplis correctement, informe l'utilisateur.
        showNotification("Veuillez remplir tous les champs correctement avant d'ajouter une nouvelle ligne.", type = "error", duration= 10)
      }
    })



    # confirmation pour ajouter une nouvelle ligne de données.
    observeEvent(input$myconfirmation, {
      if (isTRUE(input$myconfirmation)) {
        # Récupère les données actuelles stockées dans l'objet réactif `r$data`.
        currentData <- r$data

        # Nouvelle ligne de données à partir des entrées de l'utilisateur.
        newRow <- data.frame(
          Region = input$c1,
          Maison.Mere = input$c2,
          Site = input$c3,
          Tonnages.DIM = input$c4,
          Nbre.de.rotation = input$c5,
          Compacteur = ifelse(input$c6 == "Compacteur", 1, 0)
        )
        currentData <- rbind(newRow, currentData)

        r$data <- currentData
      }

      # Sauvegarde le dataframe mis à jour dans un fichier CSV nommé 'Tonnage.csv'.
      write.csv(r$data, "Tonnage.csv", row.names = FALSE)
      # Mise à jour des données dans l'environnement global pour qu'elles soient disponibles partout dans l'application.
      Tonnage <- read.csv("Tonnage.csv")
      usethis::use_data(Tonnage, overwrite = TRUE)
    })


    # bouton de suppression.
    observeEvent(input$deleteBtn, {
      # Vérifie si l'utilisateur a sélectionné au moins une ligne à supprimer.
      if (length(input$table_data_rows_selected) > 0) {
        # Demande de confirmation avant de procéder à la suppression,
        confirmSweetAlert(
          session = session,
          inputId = "confirmDelete",
          title = "Confirmation de la suppression",
          text = "Êtes-vous sûr de vouloir supprimer les lignes sélectionnées ?",
          type = "warning",
          danger_mode = TRUE,
          btn_labels = c("Annuler", "Confirmer")
        )
      } else {
        # Si aucune ligne n'est sélectionnée, informe l'utilisateur via une notification d'erreur.
        showNotification("Aucune ligne sélectionnée pour la suppression.", type = "error", duration = 10)
      }
    })

    # confirmation de suppression.
    observeEvent(input$confirmDelete, {
      # Vérifie si l'utilisateur confirme la suppression.
      if (isTRUE(input$confirmDelete)) {
        removeModal()
        if (length(input$table_data_rows_selected) > 0) {
          newData <- r$data[-input$table_data_rows_selected, ]
          r$data <- newData

          # Sauvegarde le dataframe mis à jour dans un fichier CSV nommé 'Tonnage.csv'.
          write.csv(r$data, "Tonnage.csv", row.names = FALSE)
          # Mise à jour des données dans l'environnement global pour qu'elles soient disponibles partout dans l'application.
          Tonnage <- read.csv("Tonnage.csv")
          usethis::use_data(Tonnage, overwrite = TRUE)

          # Informe l'utilisateur de la réussite de la suppression via une notification.
          showNotification("Les lignes ont été supprimées avec succès.", type = "error", duration = 10)
        }
      }
    })

    # bouton "Éditer".
    observeEvent(input$edit_btn, {
      selected <- input$table_data_rows_selected
      # Vérifie si exactement une ligne a été sélectionnée pour l'édition.
      if (length(selected) == 1) {
        selectedData <- r$data[selected, ]  # Extrait la ligne sélectionnée pour l'édition.
        # Affiche un dialogue modal pour permettre à l'utilisateur de modifier les valeurs de la ligne.
        showModal(modalDialog(
          title = "Éditer la ligne",
          textInput(ns("edit_region"), "Region", selectedData$Region),
          textInput(ns("edit_maisonMere"), "Maison Mere", selectedData$Maison.Mere),
          textInput(ns("edit_site"), "Site", selectedData$Site),
          numericInput(ns("edit_tonnages"), "Tonnages", value = selectedData$Tonnages.DIM),
          numericInput(ns("edit_rotations"), "Rotations", value = selectedData$Nbre.de.rotation),
          selectInput(ns("edit_compacteur"), "Compacteur", choices = c("Oui" = 1, "Non" = 0), selected = selectedData$Compacteur),
          footer = tagList(  # Boutons pour annuler ou sauvegarder les modifications.
            modalButton("Annuler"),
            actionButton(ns("save_edit"), "Sauvegarder", class = "btn-primary")
          )
        ))
      }
    })

    # sauvegarde après l'édition.
    observeEvent(input$save_edit, {
      error_message <- NULL

      # Vérifie les conditions pour assurer que les valeurs éditées sont valides.
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
          # Met à jour les données dans l'objet réactif avec les nouvelles valeurs saisies par l'utilisateur.
          r$data[selected, ]$Region <- input$edit_region
          r$data[selected, ]$Maison.Mere <- input$edit_maisonMere
          r$data[selected, ]$Site <- input$edit_site
          r$data[selected, ]$Tonnages.DIM <- as.numeric(input$edit_tonnages)
          r$data[selected, ]$Nbre.de.rotation <- as.numeric(input$edit_rotations)
          r$data[selected, ]$Compacteur <- as.numeric(input$edit_compacteur)
        }

        # Sauvegarde de la dataframe mis à jour dans un fichier CSV nommé 'Tonnage.csv'.
        write.csv(r$data, "Tonnage.csv", row.names = FALSE)
        # Mise à jour des données dans l'environnement global pour qu'elles soient disponibles partout dans l'application.
        Tonnage <- read.csv("Tonnage.csv")
        usethis::use_data(Tonnage, overwrite = TRUE)

      }
    })

    # Sauvegarde de data au format csv

    output$save_data <- downloadHandler(
      filename = function() {
        paste("Tonnage", Sys.Date(), ".csv", sep = '')
      },
      content = function(file) {
        write.csv(r$data, file, row.names = FALSE)
      }
    )

  })
}
