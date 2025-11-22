analysis_description <- list(
  name = "geo_count",
  help = "Renvoie le nombre d'échantillions par région",
  args = list(
    mode = list(required = TRUE, help = "Abs / Prop")
  )
)

analysis_function <- function(data, args) {
  
  mode <- args$mode

  data_dep_region = read.csv("csv/departements_region.csv")

  data <- data %>%
    filter(departement != "0") %>%
    mutate(departement = ifelse(nchar(departement)==1, paste0("0", departement), departement))

  data_sum_reg <-data %>%
    left_join(
    data_dep_region,
    by = c("departement" = "code_departement")
    )

  if (mode=="abs"){
    data_sum_reg <- data_sum_reg %>%
      group_by(nom_region) %>%
      summarise(occurence = n())
  }

  if (mode=="prop"){
    data_pop_region = read.csv("csv/population_region.csv", sep = ";") %>%
      mutate(Population = as.double(Population))
      
    data_sum_reg <- data_sum_reg%>%
      left_join(
      data_pop_region,
      by = c("nom_region" = "Région")
      )%>%
      group_by(nom_region) %>%
      summarise(occurence = n() / first(Population/1e6)) #nombre d'échantillons par million d'habitants
  }

  as.list(setNames(data_sum_reg$occurence, data_sum_reg$nom_region))
}
