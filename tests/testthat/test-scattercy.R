# WARNING - Generated by {fusen} from dev/flat_fonctions-metiers.Rmd: do not edit by hand

test_that("scatterCy returns a plotly object", {
  result <- scatterCy(Tonnage,NULL,NULL,"Sans Compacteur")
  # Vérifier que le résultat est un objet plotly
  expect_true("plotly" == class(result)[1])
})
