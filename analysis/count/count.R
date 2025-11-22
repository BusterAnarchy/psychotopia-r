analysis_description <- list(
  name = "count",
  help = "Affiche le résumé statistique du dataset",
  args = list()
)

analysis_function <- function(data, args) {
  nrow(data)
}
