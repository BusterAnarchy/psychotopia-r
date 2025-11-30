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
  

  data_pre_analysis <- data %>%
    select(molecule_simp) %>%
    mutate(molecule_simp  = ifelse(molecule_simp == "Problème", "Autres",molecule_simp)) %>%
    group_by(molecule_simp) %>%
    summarise(somme = n())%>%
    arrange(desc(somme)) %>% 
    mutate(molecule_simp = ifelse(row_number()>lim, "Autres",molecule_simp))
  
  list_focus <- data_pre_analysis$molecule_simp

  data_bimestre <- data %>%
    mutate(
      month = month(date),
      bimestre = 1 + (month - 1) %/% 2,  # Diviser le mois pour obtenir un bimestre (1-2, 3-4, etc.)
      date_bimestre = floor_date(date, "year") + months((bimestre - 1) * 2)  # Calculer le premier jour du bimestre
    )

  grille <- expand.grid(
    date_bimestre = unique(data_bimestre$date_bimestre),
    molecule_simp = unique(c(list_focus, "Autres"))
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
    mutate(molecule_simp = ifelse(molecule_simp %in% list_focus, molecule_simp, "Autres")) %>%
    group_by(date_bimestre) %>%
    mutate(n_total = n()) %>%
    ungroup() %>%
    mutate(molecule_simp = ifelse(molecule_simp %in% list_focus,molecule_simp,"Autres")) %>%
    group_by(date_bimestre, molecule_simp) %>%
    formula %>%
    right_join(grille, by = c("date_bimestre", "molecule_simp")) %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    arrange(date_bimestre, molecule_simp)

  order=data_evol_abs %>% 
    filter(date_bimestre==max(date_bimestre, na.rm=T)) %>%
    mutate(temp=ifelse(molecule_simp=="Autres",-1,value)) %>% 
    arrange(desc(temp)) %>% 
    select(molecule_simp)

  data_evol_abs = data_evol_abs %>% mutate(molecule_simp = factor(molecule_simp, levels = unlist(order)))

  prod_vec=levels(data_evol_abs$molecule_simp)

  # Génération de la liste des datasets
  datasets_list <- lapply(prod_vec, function(prod_i) {
    list(
      label = as.character(prod_i),
      data = (data_evol_abs %>% filter(molecule_simp == prod_i))$value,
      fill = "origin"
    )
  })

  json_obj <- list(
    labels_area = as.character(unique(data_evol_abs$date_bimestre)),
    datasets_area = datasets_list,
    count = nrow(data)
  )
}
