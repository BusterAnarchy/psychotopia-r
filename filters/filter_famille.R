filter_description <- list(
  name = "Filtre Famille",
  args = list(
    molecule_families = list(required = TRUE, help = "Liste de familles de molécule", alias = "mf")
  ),
  help = "Filtre les lignes d’un data.frame entre deux dates"
)

filter_function <- function(data, args) {
    molecule_families <- strsplit(args$molecule_families, ",")[[1]]
    data %>% filter(famille %in% !! molecule_families)
}