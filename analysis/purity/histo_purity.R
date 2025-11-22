analysis_description <- list(
  name = "histo",
  help = "Affiche le résumé statistique du dataset",
  args = list()
)

analysis_function <- function(data, args) {

  data = data %>% mutate(pourcentage = as.double(pourcentage))

  tranches <- tibble(classe = seq(0, 100, by = 5))

  data_histo <- data %>%
  select(pourcentage) %>% 
  mutate(classe = cut(pourcentage,
                      breaks = seq(0, 105, by = 5),
                      include.lowest = TRUE,
                      right = FALSE,  # [x, y[
                      labels = seq(0, 100, by = 5))) %>%
  count(classe, name = "occurence") %>%
  mutate(classe = as.integer(as.character(classe))) %>% 
  right_join(tranches, by = "classe") %>%
  mutate(occurence = ifelse(is.na(occurence), 0, occurence)) %>% 
  arrange(classe)
  
  data_histo
}
