analysis_description <- list(
  name = "describe",
  help = "Décris les colones du jeu de donnée",
  args = list()
)

analysis_function <- function(data, args) {
  names(data)
}
