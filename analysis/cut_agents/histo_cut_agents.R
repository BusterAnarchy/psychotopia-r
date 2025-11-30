analysis_description <- list(
  name = "histo_cut_agents",
  help = "Indique, pour chaque produit de coupe, combien d’échantillons le contiennent",
  args = list()
)

analysis_function <- function(data, args) {

    liste_prod_coupe = c("paracetamol","cafeine","levamisole","phenacetine","hydroxyzine", "lidocaine","procaine")

    data_coupe = data %>% filter(presencecoupe==1) %>%  select(all_of(liste_prod_coupe),date)

    data_coupe<- data_coupe %>%
        rename("Paracétamol" = paracetamol, 
                "Lévamisole" = levamisole,
                "Phénacétine" = phenacetine,
                "Caféine" = cafeine,
                "Hydroxyzine" = hydroxyzine,
                "Lidocaïne" = lidocaine,
                "Procaïne" = procaine)

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
