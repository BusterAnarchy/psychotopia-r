filter_description <- list(
  name = "Filtre Purity > 0",
  args = list(
    "no-purity" = list(required = FALSE, action = "store_true", help = "Conserve seulement les données avec une pureté > 0", alias = "np")
  ),
  help = "Conserve seulement les données avec une pureté > 0"
)

filter_function <- function(data, args) {
    if ("percent" %in% names(data)) {
      data %>% filter(percent > 0)
    } else {
      stop("Filtre Purity > 0 : colonne 'percent' absente du jeu de données.")
    }
}
