analysis_description <- list(
  name = "temporal_supply",
  help = "Renvoie l'évolution des modes d'approvissionnement dans le temps",
  args = list()
)

analysis_function <- function(data, args) {

  suppressPackageStartupMessages({
    library(dplyr)
    library(jsonlite)
    library(lubridate)
  })

  black_list=c("Produits de coupe et commentaires :","Revendeur habituel","Revendeur occasionnel","Nous ne détectons rien par HPLC / CCM","")
  df_without_blacklist <- data %>% filter(!provenance %in% black_list)
  
  data_bimestre <- df_without_blacklist %>%
  mutate(
    month = month(date),
    bimestre = 1 + (month - 1) %/% 2,  # Diviser le mois pour obtenir un bimestre (1-2, 3-4, etc.)
    date_bimestre = floor_date(date, "year") + months((bimestre - 1) * 2)  # Calculer le premier jour du bimestre
  )

  grille <- expand.grid(
    date_bimestre = unique(data_bimestre$date_bimestre),
    provenance = unique(data_bimestre$provenance)
  )

  # Calcul des proportions
  data_evol_approvisionnement <- data_bimestre %>%
    filter(!provenance %in% black_list) %>%
    group_by(date_bimestre) %>%
    mutate(n_total = n()) %>%
    ungroup() %>%
    group_by(date_bimestre, provenance) %>%
    summarise(prop = n() / first(n_total), .groups = "drop") %>%
    right_join(grille, by = c("date_bimestre", "provenance")) %>%
    mutate(prop = ifelse(is.na(prop), 0, prop)) %>%
    arrange(date_bimestre, provenance)


  order=data_evol_approvisionnement %>% 
    filter(date_bimestre==max(date_bimestre, na.rm=T)) %>%
    mutate(temp=prop) %>% 
    arrange(desc(temp)) %>% 
    select(provenance)

  data_evol_approvisionnement <- data_evol_approvisionnement %>% 
    mutate(provenance = factor(provenance, levels = unlist(order)))

  prov_vec=levels(data_evol_approvisionnement$provenance)

  # Génération de la liste des datasets
  datasets_list <- lapply(prov_vec, function(prov_i) {
    list(
      label = as.character(prov_i),
      data = (data_evol_approvisionnement %>% filter(provenance == prov_i))$prop,
      fill = "origin"
    )
  })

  N=nrow(df_without_blacklist)

  json_obj <- list(
    labels_area = as.character(unique(data_evol_approvisionnement$date_bimestre)),
    datasets_area = datasets_list,
    count=N
  )
}
