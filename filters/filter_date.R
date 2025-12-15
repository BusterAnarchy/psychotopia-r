filter_description <- list(
  name = "Filtre temporel",
  args = list(
    start = list(required = FALSE, help = "Date de début (JJ/MM/AAAA)"),
    end   = list(required = FALSE, help = "Date de fin (JJ/MM/AAAA)")
  ),
  help = "Filtre les lignes d’un data.frame entre deux dates"
)

filter_function <- function(data, args) {
  
  start <- if (!is.null(args$start) && args$start != "") as.Date(args$start, format="%d/%m/%Y") else NA
  end   <- if (!is.null(args$end) && args$end != "") as.Date(args$end, format="%d/%m/%Y") else NA

  if (!is.na(start) && !is.na(end)) {
    data %>% filter(date >= start, date <= end)
  } else if (!is.na(start)) {
    data %>% filter(date >= start)
  } else if (!is.na(end)) {
    data %>% filter(date <= end)
  } else {
    data
  }
}
