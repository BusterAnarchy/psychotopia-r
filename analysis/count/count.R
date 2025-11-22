analysis_description <- list(
  name = "count",
  help = "Renvoie le nombre d'échantillions",
  args = list()
)

analysis_function <- function(data, args) {
  nrow(data)
}
