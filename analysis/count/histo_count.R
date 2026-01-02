analysis_description <- list(
  name = "histo_count",
  help = "Renvoie le nombre d'échantillions par molécule",
  args = list()
)

analysis_function <- function(data, args) {
  
  lim = 15 #nombre de produits différents sur le diagramme

  molecule_col <- if ("molecule" %in% names(data)) {
    "molecule"
  } else {
    stop("histo_count : colonne 'molecule' absente du jeu de données.")
  }

  df_pie <- data %>%
    mutate(molecule_label = .data[[molecule_col]]) %>%
    mutate(molecule_label = ifelse(molecule_label %in% c("Problème", "probleme"), "Autres", molecule_label))

  df_pie <- df_pie %>%
    group_by(molecule_label) %>%
    summarise(somme = n())%>%
    arrange(desc(somme)) %>% 
    mutate(molecule_label = ifelse(row_number()>lim, "Autres", molecule_label)) %>% 
    group_by(molecule_label) %>%
    summarise(somme = sum(somme)) %>%
    arrange(desc(somme))%>% 
    mutate(
      pourcent = somme / sum(somme) * 100,
      categorie_label = paste0(molecule_label, " (", round(pourcent, 1), "%)")
    )

  df_pie = df_pie %>% 
    mutate(temp=ifelse(molecule_label=="Autres",-1,somme)) %>% 
    arrange(temp) %>% 
    mutate(categorie_label = factor(categorie_label, levels = categorie_label))

  N=nrow(data)

  df_fin = df_pie %>% select(categorie_label, pourcent)

  list(
    labels = as.character(df_fin$categorie_label),
    data = df_fin$pourcent
  )
}
