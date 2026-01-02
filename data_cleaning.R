#!/usr/bin/env Rscript

invisible(Sys.setlocale(category = "LC_ALL", locale = "C.UTF-8"))
Sys.setenv(ROOT = "/home/ubuntu/psychotopia-r")

suppressPackageStartupMessages({
    library(DBI)
    library(RMariaDB)
    library(dplyr)
    library(stringi)
    library(stringr)
})

con <- dbConnect(
    RMariaDB::MariaDB(),
    dbname   = Sys.getenv("DB_NAME"),
    host     = Sys.getenv("HOST"),
    port     = as.integer(Sys.getenv("PORT")),
    user     = Sys.getenv("USER"),
    password = Sys.getenv("PASSWORD"),
    encoding = "utf8mb4"
)

# ----------------------------
# Chargement des tables
# ----------------------------
ra <- dbReadTable(con, "resultats_analyse")
alias <- dbReadTable(con, "psychotopia_molecule_alias")

# ----------------------------
# Nettoyage encodage (sécurité)
# ----------------------------
ra[] <- lapply(ra, function(col) {
    if (is.character(col)) {
        col <- gsub("Ã©", "é", col)
        col <- gsub("Ã¯", "ï", col)
    }
    col
})

# ----------------------------
# Suppression d'une entrée invalide
# ----------------------------
ra <- ra %>%
  filter(identification != "PSYCHOACTIF2023007")

# ----------------------------
# Normalisation molecule
# ----------------------------
ra <- ra %>%
mutate(
    molecule_norm = molecule %>%
    trimws() %>%
    tolower() %>%
    stringi::stri_trans_general("Latin-ASCII")
)

alias <- alias %>%
mutate(
    alias_norm = alias %>%
    trimws() %>%
    tolower() %>%
    stringi::stri_trans_general("Latin-ASCII")
)

ra <- ra %>%
left_join(
    alias %>% select(alias_norm, name),
    by = c("molecule_norm" = "alias_norm")
) %>%
mutate(
    molecule_final = ifelse(is.na(name), molecule_norm, name)
)

# ----------------------------
# Normalisation forme
# ----------------------------
ra <- ra %>%
mutate(
    forme_final = forme %>%
    { ifelse(is.na(.), "", .) } %>%
    trimws() %>%
    tolower() %>%
    stringi::stri_trans_general("Latin-ASCII"),

    forme_final = case_when(
        forme_final == "" & molecule_final == "cannabis" ~ "herbe",
        forme_final == "" & molecule_final == "lsd" ~ "buvard",
        forme_final == ""       ~ "cristal",
        forme_final == "poudre" ~ "cristal",
        TRUE                    ~ forme_final
    )
)

# ----------------------------
# Normalisation pourcentage
# ----------------------------
ra <- ra %>%
  mutate(pourcentage = tolower(pourcentage)) %>%
  mutate(pourcentage = gsub(",",".",pourcentage)) %>%
  mutate(pourcentage = gsub("%","",pourcentage)) %>%
  mutate(pourcentage = gsub("eq","",pourcentage)) %>%
  mutate(pourcentage = gsub("équivalent","",pourcentage)) %>%
  mutate(pourcentage = gsub("base","",pourcentage)) %>%
  mutate(pourcentage = gsub("sel","",pourcentage)) %>%
  mutate(pourcentage = gsub("chlorhydrate","",pourcentage)) %>%
  mutate(pourcentage = sub(".*thc ([0-9]{2}).*", "\\1", pourcentage)) %>%
  mutate(pourcentage = sub(".*thc ([0-9]{1}).*", "\\1", pourcentage)) %>%
  mutate(pourcentage = gsub(" ","",pourcentage)) %>%
  mutate(pourcentage = suppressWarnings(as.numeric(pourcentage)))


# ----------------------------
# Préparation table finale
# ----------------------------
final_data <- ra %>%
  transmute(
    id           = identification,
    molecule     = molecule_final,
    form         = forme_final,
    supply       = provenance,
    departement  = departement,
    consumed     = consomme,
    percent      = pourcentage,
    is_cutted    = presencecoupe,
    date         = as.Date(date),
  )

# ----------------------------
# Préparation tables secondaires
# ----------------------------
cut_agents_data <- ra %>%
  transmute(
    id = identification,
    coupe = coupe,
    paracetamol = paracetamol,
    cafeine = cafeine,
    levamisole = levamisole,
    phenacetine = phenacetine,
    hydroxyzine = hydroxyzine,
    lidocaine = lidocaine,
    procaine = procaine,
    dextrometorphane = dextrometorphane
  )

sub_products_data <- ra %>%
  transmute(
    id = identification,
    X6MAM = X6MAM,
    noscapine = noscapine,
    papaverine = papaverine,
    morphine = morphine
  )

extract_number_combined <- function(comment, molecule) {
  comment_clean <- gsub("\\s+", "", comment)
  comment_clean <- gsub("-", "", comment_clean)
  comment_clean <- tolower(comment_clean)

  molecule_clean <- tolower(gsub("-", "", molecule))
  
  patterns <- c(
    paste0(".*?(\\d+[\\.,]?\\d*)mgde", molecule_clean, ".*"),
    paste0(".*?(\\d+[\\.,]?\\d*)mg", molecule_clean, "/.*"),
    ".*?(\\d+[\\.,]?\\d*)mgeqbase.*"
  )
  
  for (pattern in patterns) {
    match <- sub(pattern, "\\1", comment_clean)
    match <- gsub(",", ".", match)
    if (!is.na(suppressWarnings(as.numeric(match)))) {
      return(as.numeric(match))
    }
  }
  
  NA_real_
}

tablet_mass_data <- ra %>%
  filter(forme_final == "comprime") %>%
  mutate(
    tablet_content = vapply(
      coupe,
      extract_number_combined,
      numeric(1),
      molecule = molecule
    ),

    coupe_clean = ifelse(is.na(coupe), "", coupe),
    coupe_clean = gsub("mg", "", coupe_clean),
    coupe_clean = gsub(",", ".", coupe_clean),

    tablet_mass = vapply(
      str_extract_all(coupe_clean, "\\b[0-9]+(?:\\.[0-9]+)?\\b"),
      function(x) if (length(x) == 0) NA_real_ else max(as.numeric(x)),
      numeric(1)
    ),

    tablet_mass = ifelse(tablet_mass > tablet_content / 0.8, tablet_mass, NA_real_)
  ) %>%
  transmute(
    id = identification,
    tablet_content,
    tablet_mass
  )


# ----------------------------
# Écriture en base
# ----------------------------
dbWriteTable(
    con,
    name = "psychotopia_analysis",
    value = final_data,
    overwrite = TRUE
)

dbWriteTable(
    con,
    name = "psychotopia_analysis_cut_agents",
    value = cut_agents_data,
    overwrite = TRUE
)

dbWriteTable(
    con,
    name = "psychotopia_analysis_sub_products",
    value = sub_products_data,
    overwrite = TRUE
)

dbWriteTable(
    con,
    name = "psychotopia_analysis_tablet_mass",
    value = tablet_mass_data,
    overwrite = TRUE
)

dbDisconnect(con)
