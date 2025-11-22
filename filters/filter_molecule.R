filter_description <- list(
  name = "Filtre Molécule",
  args = list(
    molecule = list(required = TRUE, help = "Molécule", alias = "m")
  ),
  help = "Conserve seulement les données pour cette molécule"
)

filter_function <- function(data, args) {
    molecule <- args$molecule
    data %>% filter(molecule_simp == !!molecule)
}