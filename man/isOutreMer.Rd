% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isoutremer.R
\name{isOutreMer}
\alias{isOutreMer}
\title{isOutreMer
permet de connaitre les sites situés en territoires outre mers}
\usage{
isOutreMer(data, classes = NULL, nb = 1)
}
\arguments{
\item{data}{un dataframe}

\item{classes}{la liste des classes des sites}

\item{nb}{le numero de la classe}
}
\value{
une liste de boolean, true si le site se situe en territoire outre mer
}
\description{
isOutreMer
permet de connaitre les sites situés en territoires outre mers
}
\examples{
isOutreMer(Tonnage)
classes <- stratopt(Tonnage)
isOutreMer(Tonnage,classes,1)
}
