analysis_description <- list(
  name = "histo_sub_products",
  help = "Indique, pour chaque sous produit, combien d’échantillons le contiennent",
  args = list()
)

analysis_function <- function(data, args) {

    suppressPackageStartupMessages({
        library(dplyr)
        library(jsonlite)
        library(lubridate)
    })

    liste_prod_coupe = c("X6MAM","noscapine","papaverine","morphine")

    data_coupe = data %>% filter(presencecoupe==1) %>%  select(all_of(liste_prod_coupe),date)

    data_coupe<- data_coupe %>%
        rename("6-MAM" = X6MAM, 
            "Noscapine" = noscapine,
            "Papavérine" = papaverine,
            "Morphine" = morphine)

    pourcentage_non_nuls <- data.frame(
        prod = character(),
        pourcentage_non_nuls = numeric()
    )

    for (col in names(data_coupe)) {
        vec <- data_coupe[[col]]
        if (is.numeric(vec)) {
            pourcentage <- mean(vec != 0, na.rm = TRUE) * 100
            pourcentage_non_nuls <- rbind(pourcentage_non_nuls,
                data.frame(prod = col, pourcentage_non_nul = round(pourcentage, 2)))
        }
    }

    pourcentage_non_nuls <- pourcentage_non_nuls %>%
        arrange(desc(pourcentage_non_nul))

    N=nrow(data)
    json_obj <- list(
        labels = as.character(pourcentage_non_nuls$prod),
        data = pourcentage_non_nuls$pourcentage_non_nul,
        count = N
    )
}
