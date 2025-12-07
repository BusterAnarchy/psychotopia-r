analysis_description <- list(
  name = "mass_reg_purity",
  help = "Calcul la régression de la teneur par rapport au poids",
  args = list()
)

analysis_function <- function(data, args) {
  data <- data %>% filter(!is.na(tablet_mass))

  model <- lm(pourcentage ~ tablet_mass, data)
  summar <- summary(model)
  coef <- summar$coefficients  
  poids_lis=list()
  pred <- predict(model, 
  newdata = data.frame(tablet_mass = data$tablet_mass), 
  interval = "confidence", 
  level = 0.95) 
  

  
  scatter_data <- list() 
  for (i in 1:length(data$tablet_mass)){ 
    scatter_data <- append(scatter_data,list(c(x = data$tablet_mass[[i]], y=data$pourcentage[[i]]))) 
  } 


  ord <- order(data$tablet_mass)
  
  fit_points <- lapply(ord, function(i) list(x = data$tablet_mass[i], y = pred[i,"fit"]))
  lwr_points <- lapply(ord, function(i) list(x = data$tablet_mass[i], y = pred[i,"lwr"]))
  upr_points <- lapply(ord, function(i) list(x = data$tablet_mass[i], y = pred[i,"upr"]))

  # Génération de la liste des datasets 
  datasets_list <- list( 
    list( 
      label = "Données", 
      type = "scatter", 
      data = scatter_data,
      borderColor = "blue"
    ), 
    list( 
      type = "line", 
      label = "Régression linéaire", 
      data = fit_points,
      borderColor = "red",
      pointRadius = 0,
      fill = FALSE
    ),
    # Confidence ribbon
    list(
      type = "line",
      data = lwr_points,
      label = NULL,
      showLegend = FALSE,
      borderColor = "grey",
      pointRadius = 0,
      fill = FALSE
    ),
    list(
      type = "line",
      label = "Intervalle de confiance à 95%",
      data = upr_points,
      borderColor = "grey",
      pointRadius = 0,
      fill = list(target = "-1"),  # fill to previous dataset (the lwr line below)
      backgroundColor = "rgba(128,128,128,0.3)"
    )
  )
  
  json_obj <- list(
    labels = as.character(sort(data$tablet_mass)), 
    datasets = datasets_list, 
    coef = c(coef[1], coef[2]),
    min_x = min(data$tablet_mass),
    max_x = max(data$tablet_mass)
    )
}