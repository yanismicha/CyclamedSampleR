# WARNING - Generated by {fusen} from dev/flat_fonctions-metiers.Rmd: do not edit by hand

r <- NULL
r$data <- Tonnage
r$classe <- stratopt(r$data)

test_that("summarise_data correctly summarises and assigns class_name", {
# Utiliser la fonction sur les données de test
summarised <- summarise_data(r$data, Region, r)

# Vérifier que les colonnes attendues sont présentes
expect_true(all(c("TotalTonnage", "NombreSites", "Proportion") %in% names(summarised)))


})
