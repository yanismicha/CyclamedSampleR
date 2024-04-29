#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import reactable
#' @import reactablefmtr
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  ## Initialisation des sites et de la data ##
  r <- reactiveValues(data = Tonnage, site1 = "",site2 = "",site3 = "",site4 = "",site5 = "",hist=historique)
  observe({


    ## classes ##
    r$classe <- stratopt(r$data)




  }) ## fin du observe

  # Ajout du mode sombre ( initialisé dans tabAccueil)
  session$sendCustomMessage(
    type = "darkmode_enable", list(message = "const darkmode =  new Darkmode(); darkmode.showWidget();")
  )

  #######################PARTIE Sidebar #######################
  mod_dbSidebar_server("dbSidebar_1")
  #######################PARTIE Accueil #######################
  mod_TabAccueil_server("TabAccueil_1")

  #######################PARTIE DATA #######################

  mod_TabData_server("TabData_1",r)


   #################################Résumés statistiques###########################

   mod_TabSummary_server("TabSummary_1",r)


  #################################TABLEAU DE BORD###########################

   mod_TabVisu_server("TabVisu_1",r)

  #################################PARTIE RANDOM###########################


  mod_divClasse_server("cadre1",r)
  mod_divClasse_server("cadre2",r)
  mod_divClasse_server("cadre3",r)
  mod_divClasse_server("cadre4",r)
  mod_divClasse_server("cadre5",r)
  mod_TabRandom_server("TabRandom_1",r)




}
