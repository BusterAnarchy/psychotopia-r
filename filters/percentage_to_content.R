filter_description <- list(
  name = "Percentage to content",
  args = list(
    "percentage_to_content" = list(required = FALSE, action = "store_true", help = "Transforme la colonne 'percent' en une colonne de teneurs", alias = "ptc")
  ),
  help = "Transforme la colonne 'percent' en une colonne de teneurs"
)

filter_function <- function(data, args) {
  if (!("coupe" %in% names(data))) {
    stop("Percentage to content : colonne 'coupe' absente du jeu de données.")
  }

  molecule_col <- if ("molecule" %in% names(data)) {
    "molecule"
  } else {
    stop("Percentage to content : colonne 'molecule' absente du jeu de données.")
  }

  molecule <- data[[molecule_col]][[1]]

  data = data %>% filter(grepl("mg", coupe))

  extract_number_combined <- function(x) {
    x_clean <- gsub("\\s+", "", x)  # Supprimer tous les espaces
    x_clean <- gsub('-','',x_clean)
    x_clean <- tolower(x_clean)

    molecule_clean = tolower(gsub('-','',molecule))
    
    # Liste des regex à tester
    patterns <- c(
      paste0(".*?(\\d+[\\.,]?\\d*)mgde",molecule_clean,".*"),
      paste0(".*?(\\d+[\\.,]?\\d*)mg",molecule_clean,"/.*"),
      ".*?(\\d+[\\.,]?\\d*)mgeqbase.*"
    )
    
    for (pattern in patterns) {
      match <- sub(pattern, "\\1", x_clean)
      match <- gsub(",", ".", match)
      if (!is.na(suppressWarnings(as.numeric(match)))) {
        return(as.numeric(match))
      }
    }
    
    return(NA_real_)  # Aucun motif trouvé
  }

  target_col <- "percent"

  data[[target_col]] <- sapply(data$coupe, extract_number_combined)
  data <- data[!is.na(data[[target_col]]), ]
}
