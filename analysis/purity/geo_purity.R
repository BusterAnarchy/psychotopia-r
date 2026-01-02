analysis_description <- list(
  name = "geo_purity",
  help = "Renvoie la moyenne de la pureté par région",
  args = list()
)

analysis_function <- function(data, args) {

  percent_col <- if ("percent" %in% names(data)) {
    "percent"
  } else {
    stop("geo_purity : colonne 'percent' absente du jeu de données.")
  }

  data = data  %>% filter(supply != "Deep web / dark web") %>% mutate(percent_val = as.double(.data[[percent_col]]))

  data_dep_region = read.csv(file.path(Sys.getenv("ROOT"), "csv/departements_region.csv"))

  data <- data %>%
      mutate(departement = ifelse(nchar(departement)==1, paste0("0", departement), departement))

  data_sum_reg <- left_join(
    data,
    data_dep_region,
    by = c("departement" = "code_departement")
  )%>%
  group_by(nom_region) %>%
  summarise(moyenne = mean(percent_val, na.rm = TRUE))

  as.list(setNames(data_sum_reg$moyenne, data_sum_reg$nom_region))
}
