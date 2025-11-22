analysis_description <- list(
  name = "temporal_count",
  help = "Renvoie le nombre d'échantillions par molécule dans le temps",
  args = list()
)

analysis_function <- function(data, args) {
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(jsonlite)
    library(lubridate)
  })
  
  list_focus <- data$molecule_simp

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

  data_evol_abs <- data_bimestre %>%
    mutate(molecule_simp = ifelse(molecule_simp %in% list_focus,molecule_simp,"Autres")) %>%
    group_by(date_bimestre, molecule_simp) %>%
    summarise(abs = n(), .groups = "drop") %>%
    right_join(grille, by = c("date_bimestre", "molecule_simp")) %>%
    mutate(abs = ifelse(is.na(abs), 0, abs)) %>%
    arrange(date_bimestre, molecule_simp)

  order=data_evol_abs %>% 
    filter(date_bimestre==max(date_bimestre, na.rm=T)) %>%
    mutate(temp=ifelse(molecule_simp=="Autres",-1,abs)) %>% 
    arrange(desc(temp)) %>% 
    select(molecule_simp)

  data_evol_abs = data_evol_abs %>% mutate(molecule_simp = factor(molecule_simp, levels = unlist(order)))

  prod_vec=levels(data_evol_abs$molecule_simp)

  # Génération de la liste des datasets
  datasets_list <- lapply(prod_vec, function(prod_i) {
    list(
      label = as.character(prod_i),
      data = (data_evol_abs %>% filter(molecule_simp == prod_i))$abs,
      fill = "origin"
    )
  })
}
