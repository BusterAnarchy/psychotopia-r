filter_description <- list(
  name = "Filtre Molécule",
  args = list(
    molecule = list(required = TRUE, help = "Molécule", alias = "m")
  ),
  help = "Conserve seulement les données pour cette molécule"
)

filter_function <- function(data, args) {
    molecule <- args$molecule
    molecule <- trimws(tolower(molecule))
    molecule <- stringi::stri_trans_general(molecule, "Latin-ASCII")

    if ("molecule" %in% names(data)) {
      data %>% filter(molecule == !!molecule)
    } else {
      stop("Filtre Molécule : colonne 'molecule' absente du jeu de données.")
    }
}
