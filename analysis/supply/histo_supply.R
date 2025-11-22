analysis_description <- list(
  name = "histo_supply",
  help = "Renvoie le nombre d'échantillon par mode d'approvissionnement",
  args = list()
)

analysis_function <- function(data, args) {
  
  black_list=c("Produits de coupe et commentaires :","Revendeur habituel","Revendeur occasionnel","Nous ne détectons rien par HPLC / CCM","")
  df_without_blacklist <- data %>% filter(!provenance %in% black_list)

  df_pie <- df_without_blacklist %>% 
    group_by(provenance) %>%
    summarise(somme = n()) %>%
    mutate(
      pourcent = somme / sum(somme) * 100,
      categorie_label = paste0(provenance, " (", round(pourcent, 1), "%)")
    ) %>%
    arrange(somme) %>%
    mutate(categorie_label = factor(categorie_label, levels = categorie_label))


  df_fin = df_pie %>% select(categorie_label, pourcent)

  # Convertir en liste nommée
  df_list <- setNames(as.list(df_fin$pourcent), df_fin$categorie_label)
}
