% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stratopt.R
\name{stratopt}
\alias{stratopt}
\title{Stratopt
Permet de récupérer les classes des sites}
\usage{
stratopt(data)
}
\arguments{
\item{data}{un dataframe}
}
\value{
une liste avec les 5 classes des sites
}
\description{
Stratopt
Permet de récupérer les classes des sites
}
\examples{
data("Tonnage")
stratopt(Tonnage)$classe1
}
