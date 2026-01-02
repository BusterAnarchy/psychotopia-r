filter_description <- list(
  name = "Filtre Pourcentage qui sont Invalid Number",
  args = list(
    "no_invalid_percentage" = list(required = FALSE, action = "store_true", help = "Conserve seulement les données avec un pourcentage qui est un nombre valide", alias = "nip")
  ),
  help = "Conserve seulement les données avec un pourcentage qui est un nombre valide"
)

filter_function <- function(data, args) {
    if ("percent" %in% names(data)) {
      data %>% filter(!is.na(percent))
    } else {
      stop("Filtre Pourcentage : colonne 'percent' absente du jeu de données.")
    }
}
