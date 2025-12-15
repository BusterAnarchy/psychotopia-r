analysis_description <- list(
  name = "describe",
  help = "Décris les colones du jeu de donnée",
  args = list()
)

analysis_function <- function(data, args) {
  # Helper to compute top counts for any categorical column
  top_counts <- function(values, top_n = 5, label = "value") {
    values <- values[!is.na(values)]
    if (is.character(values) || is.factor(values)) {
      values <- trimws(as.character(values))
    }
    values <- values[values != ""]
    if (!length(values)) {
      return(NULL)
    }
    counts <- sort(table(values), decreasing = TRUE)
    top_tab <- head(counts, top_n)
    df <- data.frame(
      value = names(top_tab),
      sample_count = as.integer(top_tab),
      share_pct = round(as.numeric(top_tab) / sum(counts) * 100, 1),
      stringsAsFactors = FALSE
    )
    names(df)[1] <- label
    df
  }

  result <- list()
  result$sample_count <- nrow(data)

  if ("date" %in% names(data)) {
    valid_dates <- data$date[!is.na(data$date)]
    if (length(valid_dates)) {
      valid_dates <- as.Date(valid_dates)
      first_date <- min(valid_dates)
      last_date <- max(valid_dates)
      result$temporal_coverage <- list(
        first_sample_date = first_date,
        last_sample_date = last_date,
        duration_days = as.integer(last_date - first_date)
      )
    }
  }

  molecule_col <- NULL
  if ("molecule_simp" %in% names(data)) {
    molecule_col <- "molecule_simp"
  } else if ("molecule" %in% names(data)) {
    molecule_col <- "molecule"
  }

  if (!is.null(molecule_col)) {
    mol_values <- data[[molecule_col]]
    mol_values <- mol_values[!is.na(mol_values)]
    if (is.character(mol_values) || is.factor(mol_values)) {
      mol_values <- trimws(as.character(mol_values))
    }
    mol_values <- mol_values[mol_values != ""]
    if (length(mol_values)) {
      result$molecules <- list(
        column_name = molecule_col,
        distinct_count = length(unique(mol_values)),
        top = top_counts(mol_values, label = "molecule")
      )
    }
  }

  if ("famille" %in% names(data)) {
    fam_values <- data$famille
    fam_values <- fam_values[!is.na(fam_values)]
    if (length(fam_values)) {
      fam_values <- trimws(as.character(fam_values))
      result$families <- list(
        distinct_count = length(unique(fam_values)),
        top = top_counts(fam_values, label = "family")
      )
    }
  }

  if ("forme" %in% names(data)) {
    forms <- data$forme
    forms <- forms[!is.na(forms)]
    if (length(forms)) {
      forms <- trimws(as.character(forms))
      result$forms <- list(
        distinct_count = length(unique(forms)),
        top = top_counts(forms, label = "form")
      )
    }
  }

  if ("departement" %in% names(data)) {
    deps <- data$departement
    deps <- deps[!is.na(deps)]
    if (length(deps)) {
      deps <- trimws(as.character(deps))
      result$geography <- list(
        department_count = length(unique(deps)),
        top_departments = top_counts(deps, label = "department")
      )
    }
  }

  if ("pourcentage" %in% names(data)) {
    pur <- suppressWarnings(as.numeric(data$pourcentage))
    pur <- pur[!is.na(pur)]
    if (length(pur)) {
      result$purity <- list(
        mean = round(mean(pur), 2),
        median = round(stats::median(pur), 2),
        min = round(min(pur), 2),
        max = round(max(pur), 2)
      )
    }
  }

  if ("presencecoupe" %in% names(data)) {
    coupe <- data$presencecoupe
    coupe <- coupe[!is.na(coupe)]
    if (length(coupe)) {
      coupe_numeric <- as.numeric(coupe)
      coupe_numeric <- ifelse(coupe_numeric > 0, 1, 0)
      result$cut_presence <- list(
        cut_samples = sum(coupe_numeric),
        percentage = round(sum(coupe_numeric) / length(coupe_numeric) * 100, 1)
      )
    }
  }

  if (length(result) == 1L && !is.null(result$sample_count)) {
    # Fall back to simply describing the columns if nothing else was computed
    result$columns <- names(data)
  }

  result
}
