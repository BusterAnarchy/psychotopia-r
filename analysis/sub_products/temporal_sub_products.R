analysis_description <- list(
  name = "temporal_sub_products",
  help = "Renvoie pour les échantillons l'évolution dans le temps des sous produits",
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

    data_bimestre <- data_coupe %>%
    mutate(
        month = month(date),
        bimestre = 1 + (month - 1) %/% 2,  # Diviser le mois pour obtenir un bimestre (1-2, 3-4, etc.)
        date_bimestre = floor_date(date, "year") + months((bimestre - 1) * 2)  # Calculer le premier jour du bimestre
    )

    cols_coupe_prod <- setdiff(names(data_bimestre), c("date", "date_bimestre", "month", "bimestre"))

    evol_coupe <- lapply(cols_coupe_prod, function(coupe_prod) {
        data_bimestre %>%
            group_by(date_bimestre) %>%
            summarise(
                total_dates = n(),
                dates_present = sum(!!sym(coupe_prod) > 0, na.rm = TRUE),
                pourcentage_presence = 100 * dates_present / total_dates,
                coupe_prod = coupe_prod,
                .groups = "drop"
            )
    }) %>%
    bind_rows()

    order=evol_coupe %>% 
        filter(date_bimestre==max(date_bimestre, na.rm=T)) %>%
        arrange(desc(pourcentage_presence)) %>% 
        select(coupe_prod)

    evol_coupe <- evol_coupe %>% 
        mutate(coupe_prod = factor(coupe_prod, levels = unlist(order)))

    prod_vec=levels(evol_coupe$coupe_prod)

    datasets_list <- lapply(prod_vec, function(prod_i) {
        list(
            label = as.character(prod_i),
            data = (evol_coupe %>% filter(coupe_prod == prod_i))$pourcentage_presence,
            fill = "origin"
        )
    })

    N=nrow(data)
    json_obj <- list(
        labels_area = as.character(unique(evol_coupe$date_bimestre)),
        datasets_area = datasets_list,
        count = N
    )
}
