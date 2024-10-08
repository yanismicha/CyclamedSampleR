# WARNING - Generated by {fusen} from dev/flat_fonctions-metiers.Rmd: do not edit by hand

r <- NULL
r$data <- Tonnage
r$classe <- stratopt(r$data)

data_copy <- Tonnage
data_copy$classe <- NA
data_copy$classe <- sapply(data_copy$Site, function(site) {
  for(i in 1:5){
    if (site %in% r$classe[[paste0("classe",i)]])
    return(i)
  }
  data_copy
})
test_that("BoxCy returns a plotly object", {
  result <- BoxCy(data_copy,NULL,NULL,"Sans Compacteur")
  # Vérifier que le résultat est un objet plotly
  expect_true("plotly" == class(result)[1])
})
