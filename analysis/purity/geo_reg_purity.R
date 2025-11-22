analysis_description <- list(
  name = "geo_reg_purity",
  help = "Affiche le résumé statistique du dataset",
  args = list()
)

analysis_function <- function(data, args) {

  data = data %>% mutate(pourcentage = as.double(pourcentage))
  
  suppressPackageStartupMessages({
    library(lubridate)
    library(lfe)
  })

  data_dep_region = read.csv("csv/departements_region.csv")

  order <- c(
    "Île-de-France", "Occitanie", "Provence-Alpes-Côte d'Azur",
    "Auvergne-Rhône-Alpes", "Grand Est", "Hauts-de-France",
    "Pays de la Loire", "Bourgogne-Franche-Comté", "Bretagne",
    "Nouvelle-Aquitaine", "Centre-Val de Loire", "Normandie", "Corse"
  )

  data <- data %>%
      filter(provenance != "Deep web / dark web") %>%
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
  model <- felm(pourcentage ~ nom_region | bimestre + provenance, data = data)

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
