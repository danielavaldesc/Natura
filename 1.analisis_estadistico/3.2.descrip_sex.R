# ============================================================
# Descriptivo por SEXO (género)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(janitor); library(stringr)
})

base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Natura"
setwd(base_dir)
source(file.path("1.analisis_estadistico","3.descriptives.R"), encoding = "UTF-8")

cali_csv <- file.path("Output","cali_limpio.csv")
med_csv  <- file.path("Output","medellin_limpio.csv")
dfs <- list()
if (file.exists(cali_csv)) dfs$cali <- readr::read_csv(cali_csv, show_col_types = FALSE) %>% mutate(ciudad="Cali")
if (file.exists(med_csv))  dfs$med  <- readr::read_csv(med_csv,  show_col_types = FALSE) %>% mutate(ciudad="Medellin")
data <- bind_rows(dfs) %>% janitor::clean_names()

find_var <- function(df, codes, patterns){
  for (code in tolower(codes)) {
    hit <- names(df)[tolower(names(df)) == code]
    if (length(hit)) return(hit[1])
  }
  pat <- paste0("(", paste(patterns, collapse="|"), ")")
  hit <- names(df)[grepl(pat, names(df), ignore.case = TRUE)]
  if (length(hit)) return(hit[1])
  NULL
}

var_genero  <- find_var(data, c("genero","sexo"), c("^genero$","^sexo$","gener"))
if (is.null(var_genero)) stop("No pude detectar la variable de SEXO/GÉNERO.")

is_cont <- function(x){
  z <- suppressWarnings(as.numeric(x))
  un <- length(unique(z[is.finite(z)]))
  isTRUE(un > 10)
}
cont_vars <- names(data)[vapply(data, is_cont, logical(1))]
cont_vars <- setdiff(cont_vars, c("id","id_encuesta","identificador"))
cat_vars  <- setdiff(names(data), c(cont_vars))

out_dir  <- file.path("1.analisis_estadistico","Output_by_sexo")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

res <- run_descriptivo(
  data              = data,
  continuous_vars   = cont_vars,
  categorical_vars  = cat_vars,
  group_vars        = c(var_genero),   # <-- por sexo
  out_xlsx          = file.path(out_dir, "descriptivo_por_sexo.xlsx"),
  out_tex_dir       = file.path(out_dir, "tex")
)

message("OK: descriptivo por SEXO exportado a: ", out_dir)
