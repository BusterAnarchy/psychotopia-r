analysis_description <- list(
  name = "geo_reg_purity",
  help = "Calcul la régression de la pureté par rapport aux régions",
  args = list()
)

analysis_function <- function(data, args) {

  percent_col <- if ("percent" %in% names(data)) {
    "percent"
  } else {
    stop("geo_reg_purity : colonne 'percent' absente du jeu de données.")
  }

  data = data %>% mutate(percent_val = as.double(.data[[percent_col]]))
  
  suppressPackageStartupMessages({
    library(lubridate)
    library(lfe)
  })

  data_dep_region = read.csv(file.path(Sys.getenv("ROOT"), "csv/departements_region.csv"))

  order <- c(
    "Île-de-France", "Occitanie", "Provence-Alpes-Côte d'Azur",
    "Auvergne-Rhône-Alpes", "Grand Est", "Hauts-de-France",
    "Pays de la Loire", "Bourgogne-Franche-Comté", "Bretagne",
    "Nouvelle-Aquitaine", "Centre-Val de Loire", "Normandie", "Corse"
  )

  data <- data %>%
      filter(supply != "Deep web / dark web") %>%
      mutate(
        departement = ifelse(
          nchar(.data$departement) == 1,
          paste0("0", .data$departement),
          .data$departement
        )
      ) %>%
      left_join(
        data_dep_region,
        by = c("departement" = "code_departement")
      ) %>%
      mutate(
        month = month(.data$date),
        bimestre = 1 + ((.data$month - 1) %/% 2) # Diviser le mois pour obtenir un bimestre
      ) %>%
      mutate(
        nom_region = factor(.data$nom_region, levels = unlist(order))
      )
  model <- felm(percent_val ~ nom_region | bimestre + supply, data = data)

  summar <- summary(model)
  r_squared <- summar$r.squared
  nb_obs <- length(summar$residuals)

  res <- summar$coefficients
  var_names <- rownames(res)           # noms des variables (Intercept, poids)
  coefs <- res[, "Estimate"]            # coefficients
  std_errors <- res[, "Std. Error"]     # erreurs standards

  stars <- cut(res[, "Pr(>|t|)"],
              breaks = c(-Inf, 0.01, 0.05, 0.1, Inf),
              labels = c("***", "**", "*", " "),
              right = FALSE)
  names(stars) <- rownames(res)

  datasets_list <- lapply(var_names, function(var_names_i) {
    list(
      "label" = sub("nom_region", "", var_names_i),
      "coefficient" = paste(
        unname(round(coefs[var_names_i], 3)),
        unname(stars[var_names_i]),
        sep = " "
      ),
      "standard_error" = unname(round(std_errors[var_names_i], 3))
    )
  })

  list(
    data = datasets_list,
    nb_obs = nb_obs,
    r_squared = r_squared
  )
}
