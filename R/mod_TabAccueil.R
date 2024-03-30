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
    # Initialisation du mode sombre
    tagList(
      tags$head(tags$script(src="https://cdn.jsdelivr.net/npm/darkmode-js@1.5.7/lib/darkmode-js.min.js")),
      tags$script("Shiny.addCustomMessageHandler('darkmode_enable', function(data) {
        new Darkmode({
            label: '🌗', // le logo
            time: '0.5s', // Temps de transition
            saveInCookies: false, // Ne pas sauvegarder dans les cookies
            autoMatchOsTheme: false // Ne pas correspondre automatiquement au thème du système d'exploitation
          }).showWidget();
      });"
      ),
      tags$style(".darkmode-layer, .darkmode-toggle {z-index: 500;}")
    ),
    mainPanel(
      tags$div(
        style = "text-align: right; margin-bottom: 20px;",
        h1(HTML("CyclamedSampleR <i class='fa-solid fa-capsules fa-bounce' style='--fa-animation-duration: 2s'></i>"),
           style = "font-family: 'Open Sans', sans-serif; font-weight: bold;"
        )
      ),
      tags$div(
        style = "margin-left: 70px;",  # Ajuster le niveau d'indentation de la liste à puces
        tags$ul(
            "Cette application a pour principale vocation de permettre la sélection de cinq sites pour l'étude de caractérisation des MNU (Médicaments Non Utilisés). Elle permet également de visualiser les données et de les modifier directement depuis un onglet, ainsi que de visualiser ces données dans un tableau de bord.",
            HTML("<br>Un Guide utilisateur détaillé, peut être lu et téléchargé juste ici:"),
            tags$div(
              style = "display: inline-block;margin-left:20px;margin-right:20px;", # Pour placer les boutons côte à côte
              actionBttn(inputId = ns("popup_guide"),label = "Guide",size="xs",style = "material-flat")
            ),
          tags$li(
            h3("Description des onglets et leurs contenus"),
            tags$div(
              class = "container",
              tags$div(
                class = "row",
                tags$div(
                  class = "col-sm-4",
                  tags$ul(
                    tags$li(h4("Les données:"),
                            "Dans cet onglet, vous retrouverez l'ensemble des sites dans un jeu de données optimisé pour la recherche. Il est possible de trier chaque site par différents facteurs tels que leurs tonnages ou leurs régions, mais également de faire une recherche spécifique si nécessaire. Un bouton est mis à disposition pour permettre l'ajout d'un nouveau site. Une fois appuyé, une fiche de renseignements doit être remplie afin d'ajouter un nouveau site.",
                            tags$div(
                              class="alert alert-warning",
                              tags$h4(
                                HTML('<i class="fa-solid fa-circle-exclamation fa-flip" style="font-size: 36px; --fa-animation-duration: 4s"></i>'),
                                class = "alert-heading"
                              ),
                            h4("Ajouter un site à l'ensemble de données a un impact direct sur les classes et la sélection des sites")
                            ),
                            "Il est également possible de sélectionner un ou plusieurs sites et de les supprimer du jeu de données."
                    )
                  )
                ),
                tags$div(
                  class = "col-sm-4",
                  tags$ul(
                    tags$li(h4("Sélection des sites:"),
                            "Cet onglet permet de réaliser un tirage aléatoire de cinq sites pour permettre l'étude de caractérisation de l'année à venir. Ce tirage choisit un site par classe, construite à l'aide d'un plan de sondage stratifié réalisé lors d'une ",
                            tags$a(href = "https://www.dropbox.com/scl/fi/eq6d5hp2shufl4js1uadk/Cyclamed_EtudeCaracterisation2022.pdf?rlkey=wizxd1ohkoc5gpvg128i8w31l&dl=1", "étude antérieure"),
                            HTML(".<br> Un code couleur a été défini afin de facilement différencier les sites selon leurs spécificités:"),
                            HTML("<br><ul>
                                    <li style='margin-top: 10px;'><span class='label label-danger'>les sites possédant un compacteur</span></li>
                                    <li style='margin-top: 10px;'><span class='label label-info'>les sites situés en territoires outre mers</span></li>
                                    <li style='margin-top: 10px;'><span class='label label-primary'>les sites classiques</span></li>
                                 </ul>"
                            ),
                            HTML("<br> L'historique des tirages est disponible directement au sein de l'onglet. Un tirage est ajouté dans l'historique à l'aide d'un simple boutton avec la possibilité d'ajouter un commentaire.<br>")
                    )
                  )
                ),
                tags$div(
                  class = "col-sm-4",
                  tags$ul(
                    tags$li(h4("Statistiques:"),
                            tags$ul(
                              tags$li(h4("Résumés:"),
                                      "Permet d'explorer les données à l'aide de tables de contingence."
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


  })
}

## To be copied in the UI
# mod_TabAccueil_ui("TabAccueil_1")

## To be copied in the server
# mod_TabAccueil_server("TabAccueil_1")
