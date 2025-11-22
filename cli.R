#!/usr/bin/env Rscript

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
  filter_files <- list.files(path, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
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
  files <- list.files(path, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
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

save_result <- function(result, name, output_dir, formats, verbose) {
   
  for (fmt in formats) {
    file_path <- file.path(output_dir, paste0(name, ".", fmt))

    if (verbose) {
      cat(green$bold(paste0("💾 Sauvegarde de ", name, " au format ", fmt, "\n\n")))
    }

    tryCatch({

      if (fmt == "cli") {
        print(result);
        next
      }
      
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
parser$add_argument("--output", "-o", help = "Dossier de sortie pour sauvegarder les résultats", default = "results")
parser$add_argument("--format", "-f", help = "Formats de sortie séparés par des virgules (ex: csv,json,txt)", default = "cli")

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

# --- add analysis sub-parser ---

subparsers <- parser$add_subparsers(dest="analysis", help="Analyse à exécuter", required=TRUE, metavar="analysis")

for (a in analysis) {

    sub_parser <- subparsers$add_parser(a$description$name, help = a$description$help)

    for (arg_name in names(a$description$args)) {
        arg_def <- a$description$args[[arg_name]]
        sub_parser$add_argument(paste0("--", arg_name), help = arg_def$help)
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

  if (should_run) {
    
    if (args$verbose){
      cat(green$bold("✅  Filtre appliqué : "), bold(f$description$name), "\n")
    }

    data <- f$fn(data, args)

  } else if (args$verbose) {
    cat(yellow$bold("⚠️  Filtre ignoré : "), yellow(f$description$name), "\n")
  }
}

if (args$verbose){
  cat("\n")
}

# --- execute analysis ---

selected_analyse <- args$analysis

formats <- if (!is.null(args$format)) unlist(strsplit(args$format, ",")) else NULL
output_dir <- args$output

if (!(selected_analyse %in% names(analysis))) {
  stop("Analyse inconnue : ", selected_analyse)
}

if (args$verbose) {
  cat(green$bold("🚀 Exécution de l’analyse : "), bold(selected_analyse), "\n\n")
}

a <- analysis[[selected_analyse]]
res <- a$fn(data, args)

# --- save the result ---

save_result(res, selected_analyse, output_dir, formats, args$verbose)


