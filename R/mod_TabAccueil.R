#' TabAccueil UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TabAccueil_ui <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = "accueil",
    mainPanel(
      tags$div(
        style = "text-align: right; margin-bottom: 20px;",
        h1("CyclamedSampleR",
           style = "font-family: 'Open Sans', sans-serif; font-weight: bold;"
        )
      ),
      tags$div(
        style = "margin-left: 70px;",  # Ajuster le niveau d'indentation de la liste à puces
        tags$ul(
          tags$li(
            h3("Explication de l'utilisation de l'application"),
            "L'utilisation de l'application est entièrement décrite dans un guide utilisateur, pouvant être lu et téléchargeable juste ici:",
            tags$div(
              style = "display: inline-block;margin-left:20px;margin-right:20px;", # Pour placer les boutons côte à côte
              actionBttn(inputId = ns("popup_guide"),label = "Guide",size="xs",style = "material-flat")
            )
          ),
          tags$li(
            h3("Objectif de l'application"),
            "Cette application a pour principale vocation de permettre la sélection de cinq sites pour l'étude de caractérisation des MNU (Médicaments Non Utilisés). Elle permet également de visualiser les données et de les modifier directement depuis un onglet, ainsi que de visualiser ces données dans un tableau de bord."
          ),
          tags$li(
            h3("Description des différents onglets et leurs contenus"),
            tags$ul(
              tags$li(h4("Les données:"),
                      "Dans cet onglet, vous retrouverez l'ensemble des sites dans un jeu de données optimisé pour la recherche. Il est possible de trier chaque site par différents facteurs tels que leurs tonnages ou leurs régions, mais également de faire une recherche spécifique si nécessaire. Un bouton est mis à disposition pour permettre l'ajout d'un nouveau site. Une fois appuyé, une fiche de renseignements doit être remplie afin d'ajouter un nouveau site.",
                      tags$div(
                        class="alert alert-warning",
                        tags$h4(
                          HTML('<svg xmlns="http://www.w3.org/2000/svg" width="32" height="32" fill="currentColor" class="bi bi-exclamation-triangle" viewBox="0 0 16 16">
                            <path d="M7.938 2.016A.13.13 0 0 1 8.002 2a.13.13 0 0 1 .063.016.15.15 0 0 1 .054.057l6.857 11.667c.036.06.035.124.002.183a.2.2 0 0 1-.054.06.1.1 0 0 1-.066.017H1.146a.1.1 0 0 1-.066-.017.2.2 0 0 1-.054-.06.18.18 0 0 1 .002-.183L7.884 2.073a.15.15 0 0 1 .054-.057m1.044-.45a1.13 1.13 0 0 0-1.96 0L.165 13.233c-.457.778.091 1.767.98 1.767h13.713c.889 0 1.438-.99.98-1.767z"/>
                            <path d="M7.002 12a1 1 0 1 1 2 0 1 1 0 0 1-2 0M7.1 5.995a.905.905 0 1 1 1.8 0l-.35 3.507a.552.552 0 0 1-1.1 0z"/>
                          </svg>'),
                          class = "alert-heading"
                        ),
                        "Ajouter un site sur le jeu de données impact directement les classes et la sélection de sites!"
                      ),
                      "Il est également possible de sélectionner un ou plusieurs sites et de les supprimer du jeu de données."
              ),
              tags$li(h4("Sélection des sites:"),
                      "Cet onglet permet de réaliser un tirage aléatoire de cinq sites pour permettre l'étude de caractérisation de l'année à venir. Ce tirage choisit un site par classe, construite à l'aide d'un plan de sondage stratifié réalisé lors d'une ",
                      tags$a(href = "https://www.dropbox.com/scl/fi/eq6d5hp2shufl4js1uadk/Cyclamed_EtudeCaracterisation2022.pdf?rlkey=wizxd1ohkoc5gpvg128i8w31l&dl=1", "étude antérieure"),
                      "."
              ),
              tags$li(h4("Statistiques:"),
                      tags$ul(
                        tags$li(h4("Résumés:"),
                                "Cet onglet permet d'explorer les données à l'aide de tables de contingence."
                        ),
                        tags$li(h4("Tableau de bord:"),
                                "Cet onglet comporte un tableau de bord sur les différents sites de MNU. Il permet notamment d'explorer plus en profondeur les données à l'aide de graphiques simples et interactifs."
                        )
                      )
              )
            )
          )

        )
      )
    )# fin du mainPanel
  ) # findu tabItem


}

#' TabAccueil Server Functions
#'
#' @noRd
mod_TabAccueil_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$popup_guide, {

      # Afficher le popup lorsque le bouton est cliqué
      showModal(modalDialog(
        title = "Guide utilisateur",
        tags$iframe(style = "height:600px; width:100%; scrolling=yes",src = "www/Guide_utilisateur.pdf"),
        easyClose = TRUE, footer = NULL
      ))
    })

    output$download_guide <- downloadHandler(
      filename = function() {
        "guide_utilisateur.pdf"
      },
      content = function(file) {
        file.copy(system.file("app/www/Guide_utilisateur.pdf", package = "CyclamedSampleR"), file)
      }
    )

  })
}

## To be copied in the UI
# mod_TabAccueil_ui("TabAccueil_1")

## To be copied in the server
# mod_TabAccueil_server("TabAccueil_1")
