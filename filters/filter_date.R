filter_description <- list(
  name = "Filtre temporel",
  args = list(
    start = list(required = TRUE, help = "Date de début (JJ/MM/AAAA)"),
    end   = list(required = TRUE, help = "Date de fin (JJ/MM/AAAA)")
  ),
  help = "Filtre les lignes d’un data.frame entre deux dates"
)

filter_function <- function(data, args) {
  start <- as.Date(args$start, format="%d/%m/%Y")
  end   <- as.Date(args$end, format="%d/%m/%Y")
  data %>% filter(date >= start, date <= end)
}