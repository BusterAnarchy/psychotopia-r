analysis_description <- list(
  name = "count_cut_agents",
  help = "Renvoie le nombre d'échantillions coupé ou non",
  args = list()
)

analysis_function <- function(data, args) {

    suppressPackageStartupMessages({
        library(dplyr)
        library(jsonlite)
        library(lubridate)
    })

    data_presence_coupe = data %>% select(presencecoupe) %>%
        filter(presencecoupe==0 | presencecoupe==1) %>% 
        mutate(presencecoupe = ifelse(presencecoupe==1, "Produit(s) de coupe détecté(s)","Pas de produit de coupe détecté"))

    df_pie_presence_coupe <- data_presence_coupe %>% 
        group_by(presencecoupe) %>%
        summarise(somme = n()) %>%
        mutate(
            pourcent = somme / sum(somme) * 100,
            categorie_label = paste0(presencecoupe, " (", round(pourcent, 1), "%)")
        ) %>%
        arrange(desc(categorie_label)) %>%
        mutate(categorie_label = factor(categorie_label, levels = categorie_label))

    df_fin_presence_coupe = df_pie_presence_coupe %>% select(categorie_label, pourcent)

    N=nrow(data)

    json_obj <- list(
        labels = as.character(df_fin_presence_coupe$categorie_label),
        data = df_fin_presence_coupe$pourcent,
        count = N
    )
}
