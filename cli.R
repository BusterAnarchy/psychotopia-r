#!/usr/bin/env Rscript

invisible(Sys.setlocale(category = "LC_ALL", locale = "C.UTF-8"))

Sys.setenv(ROOT = "/home/ubuntu/psychotopia-r")

load_data <- function(){
  suppressPackageStartupMessages({
    library(DBI)
    library(RMariaDB)
    library(dplyr)
  })

  user <- Sys.getenv("USER")
  pwd <- Sys.getenv("PASSWORD")
  host <- Sys.getenv("HOST")
  port <- as.integer(Sys.getenv("PORT"))
  
  con <- dbConnect(RMariaDB::MariaDB(),
                 dbname = Sys.getenv("DB_NAME"),
                 host = host,
                 port = port,
                 user = user,
                 password = pwd,
                 client.flag = CLIENT_LOCAL_FILES,
                 encoding = "utf8mb4")
  
  dbListTables(con)
  data <- dbReadTable(con, "resultats_analyse_cleaned")
  dbDisconnect(con)
  data[] <- lapply(data, function(col) {
    if (is.character(col)) {
      col <- gsub("Ã©", "é", col)
      col <- gsub("Ã¯", "ï", col)
      return(col)
    } else {
      return(col)
    }
  })
  data = data %>% mutate(date=as.Date(date))
  
  return(data)
}

load_filters <- function(path = "filters") {
  
  filter_files <- list.files(file.path(Sys.getenv("ROOT"), path), full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
  filters <- list()

  for (file in filter_files) {
    env <- new.env()
    source(file, local = env)
    filters[[env$filter_description$name]] <- list(
      description = env$filter_description,
      fn = env$filter_function
    )
  }

  filters
}

load_analysis <- function(path = "analysis") {

  files <- list.files(file.path(Sys.getenv("ROOT"), path), full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
  analysis <- list()
  
  for (file in files) {
    env <- new.env()
    source(file, local = env)
    analysis[[env$analysis_description$name]] <- list(
      description = env$analysis_description,
      fn = env$analysis_function
    )
  }
  
  analysis
}

parse_analysis <- function(a_str) {
  parts <- strsplit(a_str, ":")[[1]]
  name <- parts[1]
  params <- list()
  if (length(parts) > 1) {
    kv_pairs <- strsplit(parts[2], ",")[[1]]
    for (kv in kv_pairs) {
      kv_split <- strsplit(kv, "=")[[1]]
      params[[kv_split[1]]] <- kv_split[2]
    }
  }
  list(name = name, params = params)
}

save_result <- function(result, name, output_dir = NULL, formats = c("txt"), verbose = TRUE) {
  
  for (fmt in formats) {
    
    # Si output_dir n'est pas défini, on affiche dans le terminal
    if (is.null(output_dir) || output_dir == "") {
      if (verbose) {
        cat(green$bold(paste0("💻 Affichage de ", name, " au format ", fmt, "\n\n")))
      }
      
      # Affichage selon le format choisi
      if (fmt == "csv" && is.data.frame(result)) {
        print(result)  # affichage "tableau"
      } else if (fmt == "json") {
        cat(jsonlite::toJSON(result, pretty = TRUE, auto_unbox = TRUE), "\n")
      } else if (fmt == "rds") {
        cat("⚠️  Format RDS choisi : impossible d'afficher directement en console. Voici un aperçu :\n")
        print(result)
      } else if (fmt == "txt") {
        print(result)
      } else {
        warning("Format non supporté pour l'affichage : ", fmt)
        print(result)
      }
      
      next
    }
    
    # Sinon, on construit le chemin de fichier
    file_path <- file.path(output_dir, paste0(name, ".", fmt))
    
    if (verbose) {
      cat(green$bold(paste0("💾 Sauvegarde de ", name, " au format ", fmt, "\n\n")))
    }
    
    tryCatch({
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      if (fmt == "csv" && is.data.frame(result)) {
        write.csv(result, file_path, row.names = FALSE)
      } else if (fmt == "json") {
        jsonlite::write_json(result, file_path, pretty = TRUE, auto_unbox = TRUE)
      } else if (fmt == "rds") {
        saveRDS(result, file_path)
      } else if (fmt == "txt") {
        capture.output(print(result), file = file_path)
      } else {
        warning("Format non supporté : ", fmt)
      }
    }, error = function(e) {
      warning("Erreur lors de la sauvegarde de ", name, " au format ", fmt, " : ", e$message)
    })
  }
}

suppressPackageStartupMessages({
  library(argparse)
  library(crayon)
})

# --- Loading filters and analysis modules ---

filters <- load_filters()
analysis <- load_analysis()

# --- Create argument parser ---

parser <- ArgumentParser(description = 'Pipeline d’analyse de données')

parser$add_argument("--verbose", "-v", help = "Activer le mode verbeux", action = "store_true", dest = "verbose")
parser$add_argument("--output", "-o", help = "Dossier de sortie pour sauvegarder les résultats", default = NULL)
parser$add_argument("--format", "-f", help = "Formats de sortie séparés par des virgules (ex: csv,json,txt)", default = "txt")

# --- add filters argument parser ---

for (f in filters) {
  for (arg_name in names(f$description$args)) {
    arg_def <- f$description$args[[arg_name]]

    arg_list <- list(paste0("--", arg_name))

    if (!is.null(arg_def$alias)) {
      arg_list <- c(paste0("-", arg_def$alias), arg_list)
    }

    arg_list$help <- arg_def$help
    arg_list$dest <- arg_name

    if (!is.null(arg_def$action) && arg_def$action == "store_true") {
      arg_list$action <- "store_true"
    }

    do.call(parser$add_argument, arg_list)
  }
}

# --- add analysis ---

parser$add_argument("analysis", nargs="+", help="Analyses à exécuter, format: name:param1=val1,param2=val2")

for (a in analysis) {

    for (arg_name in names(a$description$args)) {
        arg_def <- a$description$args[[arg_name]]
        parser$add_argument(paste0("--", arg_name), help = paste0(a$description$name, " : ", arg_def$help))
    }
}

args <- parser$parse_args()
args <- as.list(args)

# --- load the data from Database ---

data = load_data()

if (args$verbose){
  cat(blue$bold("##### Applications des filtres #####"))
  cat("\n\n")
}

# --- apply filters on data ---

for (f in filters) {
  arg_defs <- f$description$args
  arg_names <- names(arg_defs)
  
  should_run <- FALSE
  
  if (any(sapply(arg_defs, function(a) identical(a$action, "store_true")))) {
    flag_names <- names(Filter(function(a) identical(a$action, "store_true"), arg_defs))
    should_run <- any(sapply(flag_names, function(n) isTRUE(args[[n]])))
  } else {
    required_args <- names(Filter(function(a) isTRUE(a$required), arg_defs))
    should_run <- all(!sapply(required_args, function(n) is.null(args[[n]])))
  }

  if (!should_run) {

    if (args$verbose) {
      cat(yellow$bold("⚠️  Filtre ignoré : "), yellow(f$description$name), "\n")
    }

    next
  }
    
  if (args$verbose){
    cat(green$bold("✅  Filtre appliqué : "), bold(f$description$name), "\n")
  }

  data <- f$fn(data, args)
}

if (args$verbose){
  cat("\n")
}

# --- execute analysis ---

parsed_analyses <- lapply(args$analysis, parse_analysis)
results <- list()
counters <- list() 

for (a in parsed_analyses) {

  selected_analyse <- a$name
  params <- a$params

  if (!(selected_analyse %in% names(analysis))) {
    stop("Analyse inconnue : ", selected_analyse)
  }

  if (!is.null(args$verbose) && args$verbose) {
    cat("🚀 Exécution de l’analyse : ", selected_analyse, "\n")
  }

  res <- analysis[[selected_analyse]]$fn(data, c(list(data = data), params))

  if (!is.null(params$label)) {
    result_name <- params$label
  } else {

    result_name <- selected_analyse
  }

  results[[result_name]] <- res
}


# --- save the result ---

save_result(results, selected_analyse, args$output, args$format, args$verbose)


