# ============================================================
# Descriptivo por MODO de transporte (P17)
# Requiere: funciones de 3.descriptives (run_descriptivo)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(janitor); library(stringr)
  library(readxl); library(purrr)
})

# ---- Config
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Natura"
setwd(base_dir)

# Fuente de funciones (tu archivo con run_descriptivo y helpers)
source(file.path("1.analisis_estadistico","3.descriptives.R"), encoding = "UTF-8")

# ---- Cargar bases limpias (Cali + Medellín) y unir
cali_csv <- file.path("Output","cali_limpio.csv")
med_csv  <- file.path("Output","medellin_limpio.csv")
stopifnot(file.exists(cali_csv) || file.exists(med_csv))

dfs <- list()
if (file.exists(cali_csv)) dfs$cali <- readr::read_csv(cali_csv, show_col_types = FALSE) %>% mutate(ciudad="Cali")
if (file.exists(med_csv))  dfs$med  <- readr::read_csv(med_csv,  show_col_types = FALSE) %>% mutate(ciudad="Medellin")
data <- bind_rows(dfs) %>% janitor::clean_names()

# ---- Detectores (usa preguntas.xlsx para P17)
preg_xlsx <- file.path("Input","preguntas.xlsx")

find_var <- function(df, codes, patterns){
  # (1) por código exacto de la encuesta (p. ej. "p17")
  for (code in tolower(codes)) {
    hit <- names(df)[tolower(names(df)) == code]
    if (length(hit)) return(hit[1])
  }
  # (2) por patrón en nombre
  pat <- paste0("(", paste(patterns, collapse="|"), ")")
  hit <- names(df)[grepl(pat, names(df), ignore.case = TRUE)]
  if (length(hit)) return(hit[1])
  NULL
}

# leer preguntas.xlsx y ubicar código P17 si es posible
p17_names <- c("p17")
if (file.exists(preg_xlsx)) {
  shs <- try(readxl::excel_sheets(preg_xlsx), silent = TRUE)
  if (!inherits(shs, "try-error")) {
    for (sh in shs) {
      d <- try(readxl::read_excel(preg_xlsx, sheet = sh), silent = TRUE)
      if (inherits(d,"try-error") || !nrow(d)) next
      nms <- tolower(janitor::make_clean_names(names(d)))
      # busca una columna que se vea como "codigo"/"variable"
      code_col <- intersect(nms, c("codigo","variable","id","preg","pregunta","var","cod"))
      if (!length(code_col)) next
      codes <- janitor::make_clean_names(tolower(as.character(d[[ code_col[1] ]])))
      if (any(codes == "p17")) { p17_names <- c("p17"); break }
    }
  }
}

# variables clave
var_modo    <- find_var(data, p17_names, c("\\bmodo\\b","medio.*trans","transporte","\\bp17\\b"))
var_genero  <- find_var(data, c("genero","sexo"), c("^genero$","^sexo$","gener"))
var_estrato <- find_var(data, c("estrato","p9"),  c("\\bestrato\\b","^p9(\\b|_)"))

if (is.null(var_modo)) stop("No pude detectar la variable de MODO (P17). Revisa preguntas.xlsx o nombres de columnas.")

# ---- Selección automática de continuas y categóricas
is_cont <- function(x){
  z <- suppressWarnings(as.numeric(x))
  un <- length(unique(z[is.finite(z)]))
  isTRUE(un > 10)
}
cont_vars <- names(data)[vapply(data, is_cont, logical(1))]
# quitar id obvios
cont_vars <- setdiff(cont_vars, c("id","id_encuesta","identificador"))

cat_vars <- setdiff(names(data), c(cont_vars))

# ---- Correr descriptivo (estratificando por MODO)
out_dir  <- file.path("1.analisis_estadistico","Output_by_mode")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

res <- run_descriptivo(
  data              = data,
  continuous_vars   = cont_vars,
  categorical_vars  = cat_vars,
  group_vars        = c(var_modo),                 # <-- estrato por MODO
  out_xlsx          = file.path(out_dir, "descriptivo_por_modo.xlsx"),
  out_tex_dir       = file.path(out_dir, "tex")    # opcional
)

message("OK: descriptivo por MODO exportado a: ", out_dir)
