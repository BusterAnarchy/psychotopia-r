filter_description <- list(
  name = "Tablet mass",
  args = list(
    "tablet_mass" = list(required = FALSE, action = "store_true", help = "Crée une colonne contenant la masse des comprimés", alias = "tm")
  ),
  help = "Crée une colonne contenant la masse des comprimés. Veillez au préalable à avoir appliqué percentage_to_content"
)

filter_function <- function(data, args) {
  library(stringr)  
  data <- data %>%
    mutate(
      # on enlève la notation mg
      coupe = gsub("mg","", coupe),
      # remplacer les virgules par des points
      coupe = gsub(",", ".", coupe),
      # récupère les nombres entiers ou décimaux
      tablet_mass = sapply(
        str_extract_all(coupe, "\\b[0-9]+(?:\\.[0-9]+)?\\b"),
        function(x) {
          if (length(x) == 0) return(NA)  # si pas de nombre
          max(as.numeric(x), na.rm = TRUE)
        }
      )
    )

  data = data %>% mutate(tablet_mass = ifelse(tablet_mass > pourcentage/0.8, tablet_mass, NA)) #permet d'éliminer les erreurs où le poids le plus grand est la masse de MDMA (eq base ou eq HCL)
}