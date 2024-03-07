#' Data of waste from medicines site information.
#'
#'
#' @format ## `Tonnage`
#' A data frame with 188 rows and 6 columns:
#' \describe{
#'   \item{Region}{Region name}
#'   \item{Maison.Mere}{Maison.mere name}
#'   \item{Site}{Site name}
#'   \item{Tonnages.DIM}{Number of DIM in dumpster (in tons)}
#'   \item{Nbre.de.rotation}{Number of times dumpster is emptied}
#'   \item{Compacteur}{1 if the site has a compactor instead of a dumpster, 0 otherwise}
#' }
#' @source <https://www.cyclamed.org/>
"Tonnage"

#' History of random selection of sites
#'
#'
#' @format ## `historique`
#' A data frame with  rows and 9 columns:
#' \describe{
#'   \item{Date}{Date of selection}
#'   \item{Site1}{site randomly selected from the first class}
#'   \item{Site2}{site randomly selected from the second class}
#'   \item{Site3}{site randomly selected from the third class}
#'   \item{Site4}{site randomly selected from the fourth  class}
#'   \item{Site5}{site randomly selected from the fifth class}
#'   \item{Etat}{selection status}
#'   \item{Commentaire}{comments on the sorting process}
#'   \item{Annee}{year for which the selection was made}


#' }
#' @source <https://www.cyclamed.org/>
"historique"
