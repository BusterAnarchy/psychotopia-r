filter_description <- list(
  name = "Filtre Sur la forme indiquée",
  args = list(
    "form" = list(required = TRUE, action = "Conserve seulement les données sous la forme indiquée : comprimé, Poudre, Cristal, Liquide, Herbe, Résine, e-liquide, gelule, Buvard / Micropointe, Nourriture")
  ),
  help = "Conserve seulement les données sous la forme indiquée : comprimé, Poudre, Cristal, Liquide, Herbe, Résine, e-liquide, gelule, Buvard / Micropointe, Nourriture"
)

filter_function <- function(data, args) {
  forms <- strsplit(args$form, ",")[[1]]
  forms <- trimws(tolower(forms))
  forms <- stringi::stri_trans_general(forms, "Latin-ASCII")

  if ("form" %in% names(data)) {
    data %>% filter(form %in% forms)
  } else {
    stop("Filtre Forme : colonne 'form' absente du jeu de données.")
  }
}
