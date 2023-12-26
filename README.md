
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CyclamedSampleR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

L’objectif de cette interface est de permettre à **Cyclamed**
d’optimiser leur étude annuelle d’échantillonnage.

## Installation

Vous pouvez installer la version en développement de la façon suivante:

``` r
devtools::install_github("yanismicha/CyclamedSampleR")
```

## chargement

Pour charger le package il suffit d’écrire la commande suivante:

``` r
library(CyclamedSampleR)
```

## Lancement de l’application

Pour lancer l’application, lancez ensuite la commande suivante:

``` r
CyclamedSampleR::run_app()
```

## Interface

L’interface se décompose en trois principaux onglets:

- onglet `data`
- onglet `random`
- onglet `analyse statistique`

### onglet data

cette onglet permet principalement de pouvoir visualiser le jeu de
donnée avec le package `reactable` permettant également de faire des
recherches spécifiques par variables et de trier de manière très
simpliste. Un bouton est également présent permettant d’ajouter
directement un site au jeu de donnée.

### onglet random

cet onglet permet de tirer les cinq sites dans le cadre de l’étude
d’échantillonnage. Quelques fonctionnalités sont disponibles en plus du
tirage:

- la possibilité de relancer le tirage plusieurs fois avec remise pour
  tous les sites.
- Possibilité de relancer le tirage plusieurs fois avec remise pour un
  ou plusieurs sites spécifiques
- Possibilité de choisir directement dans la liste des classes.(Non
  conseillé)
- Information sur les sites choisis
- Indications des sites avec ou sans compacteurs.
- Indication des sites Outre-Mer.
- Affichage d’informations sur les classes: intervalles de tonnage des
  DIM

les différentes classes sont récupérés avec la fonction `stratopt`:

``` r
library(CyclamedSampleR)
#> Warning: remplacement de l'importation précédente 'shinyWidgets::alert' par
#> 'shinyjs::alert' lors du chargement de 'CyclamedSampleR'
#> Warning: remplacement de l'importation précédente 'reactablefmtr::html' par
#> 'shinyjs::html' lors du chargement de 'CyclamedSampleR'
#> Warning: remplacement de l'importation précédente 'shiny::runExample' par
#> 'shinyjs::runExample' lors du chargement de 'CyclamedSampleR'
classes <- stratopt(Tonnage)
```

    #> [1] "Nombre de sites pour la classe  1 : 37"
    #> [1] "Nombre de sites pour la classe  2 : 52"
    #> [1] "Nombre de sites pour la classe  3 : 45"
    #> [1] "Nombre de sites pour la classe  4 : 31"
    #> [1] "Nombre de sites pour la classe  5 : 23"
