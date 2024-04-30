## code to prepare `Tonnage` dataset goes here

Tonnage <- read.csv("tonnage.csv")
usethis::use_data(Tonnage, overwrite = TRUE)

historique <- read.csv("historique.csv")
usethis::use_data(historique, overwrite = TRUE)
