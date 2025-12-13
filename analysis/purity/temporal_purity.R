analysis_description <- list(
  name = "temporal_purity",
  help = "Renvoie l'évolution de la pureté dans le temps",
  args = list(
    delta = list(required = TRUE, help = "delta"),
    mode = list(required = TRUE, help = "Avg / Med"),
    unit = list(required = TRUE, help = "pourcent / poids")
  )
)

analysis_function <- function(data, args) {

    delta <- as.double(args$delta)
    mode <- args$mode
    unit <- args$unit

    data = data %>% mutate(pourcentage = as.double(pourcentage))

    if (mode == "avg"){
      data_lis <- data %>%
        arrange(date) %>%
        mutate(
          main = sapply(date, function(d) {
            mean(pourcentage[date >= d - delta & date <= d + delta], na.rm = TRUE)
          }),
          ecart_type_glissant = sapply(date, function(d) {
            sd(pourcentage[date >= d - delta & date <= d + delta], na.rm = TRUE)
          }),
          borne_sup = main + ecart_type_glissant,
          borne_inf = main - ecart_type_glissant) %>%
        filter(date >= min(date) + delta, date <= max(date) - delta) %>%
        select(date, main, borne_sup, borne_inf)
      if (unit == "pourcent"){
        data_lis <- data_lis %>% mutate(borne_inf = ifelse(borne_inf < 0, 0, borne_inf), borne_sup = ifelse(borne_sup > 100, 100, borne_sup))
      }
    }
    
    if (mode == "med"){
      data_lis <- data %>%
        arrange(date) %>%
        mutate(
          main = sapply(date, function(d) {
            median(pourcentage[date >= d - delta & date <= d + delta], na.rm = TRUE)
          }),
          borne_inf = sapply(date, function(d) {
            quantile(pourcentage[date >= d - delta & date <= d + delta], 0.25, na.rm = TRUE)
        }),
          borne_sup = sapply(date, function(d) {
            quantile(pourcentage[date >= d - delta & date <= d + delta], 0.75, na.rm = TRUE)
        })) %>%
        filter(date >= min(date) + delta, date <= max(date) - delta) %>%
        select(date, main, borne_sup, borne_inf)
    }

    # Génération de la liste des datasets
    datasets_list <-list(
      list(
      label = paste(mode, "glissante"),
      data = data_lis$main,
      fill = "false"
      ),
      list(
      label = "borne sup",
      data = data_lis$borne_sup,
      fill = "false"
      ),
      list(
      label = "borne inf",
      data = data_lis$borne_inf,
      fill = "false"
      )
    )
    labels_line = as.character(data_lis$date)
    
    json_obj <- list(
      labels_line = labels_line,
      datasets_line = datasets_list,
      count = nrow(data)
    )
}
