analysis_description <- list(
  name = "pie_consumption",
  help = "Renvoie la proportion de quels échantillons ont été consommés ou non avant envoi",
  args = list()
)

analysis_function <- function(data, args) {
    
    data_conso = data %>%
        filter(consomme==0 | consomme==1) %>% 
        mutate(consomme = ifelse(consomme==1, "Déjà consommé","Pas encore consommé")) %>%
        group_by(consomme) %>%
        summarise(somme = n()) %>%
        mutate(
            pourcent = somme / sum(somme) * 100,
            categorie_label = paste0(consomme, " (", round(pourcent, 1), "%)")
        ) %>%
        arrange(desc(categorie_label)) %>%
        mutate(categorie_label = factor(categorie_label, levels = categorie_label))

    json_obj <- list(
        labels = as.character(data_conso$categorie_label),
        data = data_conso$pourcent
    )
}
