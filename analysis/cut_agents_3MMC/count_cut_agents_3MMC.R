analysis_description <- list(
  name = "count_cut_agents_3MMC",
  help = "Renvoie le nombre d'échantillions coupé ou non",
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

    N=nrow(data)
    json_obj <- list(
        labels = c("Autre(s) produit(s) détecté(s)","Que de la 3-MMC détectée"),
        data = c(nrow(data_coupe)/N, 1-nrow(data_coupe)/N),
        count = N
    )
}
