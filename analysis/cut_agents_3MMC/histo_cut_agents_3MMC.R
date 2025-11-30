analysis_description <- list(
  name = "histo_cut_agents_3MMC",
  help = "Indique, pour chaque produit de coupe, combien d’échantillons le contiennent",
  args = list()
)

analysis_function <- function(data, args) {

    suppressPackageStartupMessages({
        library(dplyr)
        library(jsonlite)
        library(lubridate)
        library(stringr)
    })
    
    white_list <- c("2MMC", "3CMC", "4CMC", "4MMC", "4BMC", "2CMC", "MDMA", "NEP", "4MEC", "DMBDP", "4DMMC")
    data_coupe <- data %>%
        mutate(
            coupe = gsub("-", "", coupe),
            coupe = lapply(
            str_extract_all(coupe, "\\b[A-Z0-9]+\\b"),
            function(x){
                x <- x[!grepl("^[0-9]+$", x)]        # remove pure numbers
                x <- intersect(x, white_list)        # keep only allowed terms
            }
            )
        ) %>%
        subset(sapply(coupe, length) > 0)          # keep rows with at least one match

    # 3ème graphique pour l'évolution temporelle
    data_bimestre <- data_coupe %>%
        mutate(
            month = month(date),
            bimestre = 1 + (month - 1) %/% 2,  # Diviser le mois pour obtenir un bimestre (1-2, 3-4, etc.)
            date_bimestre = floor_date(date, "year") + months((bimestre - 1) * 2)  # Calculer le premier jour du bimestre
        )

    # Un échantillon contient des traces de Kétamine et de cocaine mais on va le négliger
    coupe_bim <- data.frame(
        molecule = unlist(data_bimestre$coupe),
        bimester = rep(data_bimestre$date_bimestre, sapply(data_bimestre$coupe, length))
    )

    # Occurence de chaque produit de coupe par bimestre
    evol_coupe <- as.data.frame(table(coupe_bim$molecule, coupe_bim$bimester))%>%
        rename(coupe_prod = Var1, date_bimestre = Var2, occurence = Freq) %>%
        group_by(date_bimestre) %>%
        mutate(pourcentage_presence = occurence/sum(occurence)*100)

    prod_vec=levels(evol_coupe$coupe_prod)

    coupe_occu <- evol_coupe %>%
        group_by(coupe_prod) %>%
        summarise(occurence = sum(occurence))
  
    coupe_occu <- coupe_occu %>%
        mutate(prop = occurence/sum(occurence)) %>%
        arrange(desc(prop))

    N=nrow(data)
    json_obj <- list(
        labels = as.character(coupe_occu$coupe_prod),
        data = coupe_occu$prop,
        count = N
    )
}
