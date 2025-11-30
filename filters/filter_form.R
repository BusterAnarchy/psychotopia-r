filter_description <- list(
  name = "Filtre Sur la forme indiquée",
  args = list(
    "form" = list(required = TRUE, action = "Conserve seulement les données sous la forme indiquée : comprimé, Poudre, Cristal, Liquide, Herbe, Résine, e-liquide, gelule, Buvard / Micropointe, Nourriture")
  ),
  help = "Conserve seulement les données sous la forme indiquée : comprimé, Poudre, Cristal, Liquide, Herbe, Résine, e-liquide, gelule, Buvard / Micropointe, Nourriture"
)

filter_function <- function(data, args) {
  form <- strsplit(args$form, ",")[[1]]
  data %>% filter(forme %in% form)
}