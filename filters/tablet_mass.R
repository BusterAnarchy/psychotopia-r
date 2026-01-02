filter_description <- list(
  name = "Tablet mass",
  args = list(
    "tablet_mass" = list(required = FALSE, action = "store_true", help = "Crée une colonne contenant la masse des comprimés", alias = "tm")
  ),
  help = "Crée une colonne contenant la masse des comprimés. Veillez au préalable à avoir appliqué percentage_to_content"
)

filter_function <- function(data, args) {
  library(stringr)  
  if (!("coupe" %in% names(data))) {
    stop("Tablet mass : colonne 'coupe' absente du jeu de données.")
  }

  percent_col <- if ("percent" %in% names(data)) {
    "percent"
  } else {
    stop("Tablet mass : colonne 'percent' absente du jeu de données.")
  }

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

  data$tablet_mass <- ifelse(data$tablet_mass > data[[percent_col]] / 0.8, data$tablet_mass, NA) #permet d'éliminer les erreurs où le poids le plus grand est la masse de MDMA (eq base ou eq HCL)
  data
}
