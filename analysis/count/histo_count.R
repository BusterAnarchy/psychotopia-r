analysis_description <- list(
  name = "histo_count",
  help = "Affiche le résumé statistique du dataset",
  args = list()
)

analysis_function <- function(data, args) {
  
  lim = 15 #nombre de produits différents sur le diagramme

  df_pie <- data %>% 
    mutate(molecule_simp  = ifelse(molecule_simp == "Problème", "Autres",molecule_simp))


  df_pie <- df_pie %>%
    group_by(molecule_simp) %>%
    summarise(somme = n())%>%
    arrange(desc(somme)) %>% 
    mutate(molecule_simp = ifelse(row_number()>lim, "Autres",molecule_simp)) %>% 
    group_by(molecule_simp) %>%
    summarise(somme = sum(somme)) %>%
    arrange(desc(somme))%>% 
    mutate(
      pourcent = somme / sum(somme) * 100,
      categorie_label = paste0(molecule_simp, " (", round(pourcent, 1), "%)")
    )

  df_pie = df_pie %>% 
    mutate(temp=ifelse(molecule_simp=="Autres",-1,somme)) %>% 
    arrange(temp) %>% 
    mutate(categorie_label = factor(categorie_label, levels = categorie_label))

  N=nrow(data)

  df_fin = df_pie %>% select(categorie_label, pourcent)

  # Convertir en liste nommée
  df_list <- setNames(as.list(df_fin$pourcent), df_fin$categorie_label)
}
