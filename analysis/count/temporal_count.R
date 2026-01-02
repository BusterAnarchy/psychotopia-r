analysis_description <- list(
  name = "temporal_count",
  help = "Renvoie le nombre d'échantillions par molécule dans le temps",
  args = list()
)

analysis_function <- function(data, args) {
  
  scale <- args$scale
  lim <- 15 #nombre de produits différents sur le diagramme

  suppressPackageStartupMessages({
    library(dplyr)
    library(jsonlite)
    library(lubridate)
  })   
  
  molecule_col <- if ("molecule" %in% names(data)) {
    "molecule"
  } else {
    stop("temporal_count : colonne 'molecule' absente du jeu de données.")
  }

  data <- data %>%
    mutate(molecule_label = as.character(.data[[molecule_col]])) %>%
    mutate(molecule_label = ifelse(molecule_label %in% c("Problème", "probleme"), "Autres", molecule_label)) %>%
    mutate(molecule_label = as.character(molecule_label))

  data_pre_analysis <- data %>%
    select(molecule_label) %>%
    group_by(molecule_label) %>%
    summarise(somme = n())%>%
    arrange(desc(somme)) %>% 
    mutate(molecule_label = ifelse(row_number()>lim, "Autres", molecule_label))
  
  list_focus <- as.character(data_pre_analysis$molecule_label)

  data_bimestre <- data %>%
    mutate(
      month = month(date),
      bimestre = 1 + (month - 1) %/% 2,  # Diviser le mois pour obtenir un bimestre (1-2, 3-4, etc.)
      date_bimestre = floor_date(date, "year") + months((bimestre - 1) * 2)  # Calculer le premier jour du bimestre
    )

  grille <- expand.grid(
    date_bimestre = unique(data_bimestre$date_bimestre),
    molecule_label = unique(as.character(c(list_focus, "Autres")))
  )

  if (scale == "abs") {

    formula <- function(data) {
      summarise(data, value = n(), .groups = "drop")
    } 

  } else if (scale == "prop") {

    formula <- function(data) {
      summarise(data, value = n() / first(n_total) * 100, .groups = "drop")
    } 

  } else {
    stop("temporal_count : scale isn't abs or prop")
  }

  data_evol_abs <- data_bimestre %>%
    mutate(molecule_label = ifelse(molecule_label %in% list_focus, molecule_label, "Autres")) %>%
    group_by(date_bimestre) %>%
    mutate(n_total = n()) %>%
    ungroup() %>%
    mutate(molecule_label = ifelse(molecule_label %in% list_focus, molecule_label, "Autres")) %>%
    mutate(molecule_label = as.character(molecule_label)) %>%
    group_by(date_bimestre, molecule_label) %>%
    formula %>%
    right_join(grille, by = c("date_bimestre", "molecule_label")) %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    arrange(date_bimestre, molecule_label)

  max_date <- suppressWarnings(max(data_evol_abs$date_bimestre, na.rm = TRUE))
  if (!is.finite(max_date)) {
    return(list(labels_area = character(0), datasets_area = list(), count = nrow(data)))
  }

  order=data_evol_abs %>% 
    filter(date_bimestre==max_date) %>%
    mutate(temp=ifelse(molecule_label=="Autres",-1,value)) %>% 
    arrange(desc(temp)) %>% 
    select(molecule_label)

  data_evol_abs = data_evol_abs %>% mutate(molecule_label = factor(molecule_label, levels = unlist(order)))

  prod_vec=levels(data_evol_abs$molecule_label)

  # Génération de la liste des datasets
  datasets_list <- lapply(prod_vec, function(prod_i) {
    list(
      label = as.character(prod_i),
      data = (data_evol_abs %>% filter(molecule_label == prod_i))$value,
      fill = "origin"
    )
  })

  json_obj <- list(
    labels_area = as.character(unique(data_evol_abs$date_bimestre)),
    datasets_area = datasets_list,
    count = nrow(data)
  )
}
