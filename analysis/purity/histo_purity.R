analysis_description <- list(
  name = "histo_purity",
  help = "Renvoie un histograme de la pureté",
  args = list(
    unit = list(required = TRUE, help = "pourcent / poids")
  )
)

analysis_function <- function(data, args) {
  unit = args$unit
  if (unit == "poids"){
    mini = 0
    maxi = max(data$pourcentage)
    step = 20
  }
  if (unit == "pourcent"){
    mini = 0
    maxi = 100
    step = 5
  }

  data = data %>% mutate(pourcentage = as.double(pourcentage))

  tranches <- tibble(classe = seq(mini, maxi, by = step))

  data_histo <- data %>%
  select(pourcentage) %>% 
  mutate(classe = cut(pourcentage,
                      breaks =seq(mini, maxi+step, by = step),
                      include.lowest = TRUE,
                      right = FALSE,  # [x, y[
                      labels = seq(mini, maxi, by = step)))%>%
  count(classe, name = "occurence") %>%
  mutate(classe = as.integer(as.character(classe))) %>% 
  right_join(tranches, by = "classe") %>%
  mutate(occurence = ifelse(is.na(occurence), 0, occurence)) %>% 
  arrange(classe)
  
  json_obj <- list(
    labels = as.character(data_histo$classe),
    data = data_histo$occurence,
    count = nrow(data)
  )
}
