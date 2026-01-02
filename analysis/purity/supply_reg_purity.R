analysis_description <- list(
  name = "supply_reg_purity",
  help = "Calcul la régression de la pureté par rapport aux modes d'approvisionnements",
  args = list()
)

analysis_function <- function(data, args) {

  percent_col <- if ("percent" %in% names(data)) {
    "percent"
  } else {
    stop("supply_reg_purity : colonne 'percent' absente du jeu de données.")
  }

  data = data %>% mutate(percent_val = as.double(.data[[percent_col]]))

  black_list=c("Produits de coupe et commentaires :","Revendeur habituel","Revendeur occasionnel","Nous ne détectons rien par HPLC / CCM","")

  data <- data %>%
    filter(!supply %in% black_list)

  order = c("Deep web / dark web", "Dealer de rue (four)", "Livreur", "Réseaux sociaux en ligne", "Dealer en soirée", "Don entre partenaire de conso", "Boutique en ligne")
  data_reg=data %>%
    mutate(supply = factor(supply, levels = unlist(order)))

  model = lm(percent_val ~ supply, data=data_reg)
 
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

  mean <- coefs[[1]]+coefs
  
  #Formating the Intercept for the table
  mean[[1]] <- coefs[[1]]
  coefs[[1]] <- NA
  stars[[1]] <- " "
  
  # Génération de la liste des datasets
  datasets_list <- lapply(var_names, function(var_names_i) {
    list(
      "label" = ifelse(var_names_i == "(Intercept)", "Deep web / dark web", sub("supply", "", var_names_i)),
      "mean" = unname(round(mean[var_names_i],3)),
      "coefficient" = paste(unname(round(coefs[var_names_i],3)), unname(stars[var_names_i]),sep=""),
      "standard_error" = unname(round(std_errors[var_names_i],3))
    )
  })
  
  list(
    data = datasets_list,
    nb_obs = nb_obs,
    r_squared = r_squared
  )
}
