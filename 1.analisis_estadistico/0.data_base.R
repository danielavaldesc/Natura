# -----------------------------------------------
# 0.data_base.R
# Limpieza + identificación cuali/cuanti (SIN renombrar columnas)
# Proyecto NATURA - Encuesta Movilidad (Cali/Medellín)
# -----------------------------------------------

suppressPackageStartupMessages({
  library(readxl);  library(dplyr);  library(tidyr)
  library(stringr); library(janitor); library(readr); library(purrr)
})

# ========= Utilidades =========

# Limpia strings: recorta y compacta espacios (no quita tildes ni cambia mayúsculas)
clean_strings <- function(x){
  if (!is.character(x)) return(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_squish(x)
  x
}

# Convierte números con coma decimal / puntos de miles (p.ej., GPS)
parse_num_locale <- function(x){
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  # elimina punto de miles y usa punto como decimal
  x <- stringr::str_replace_all(x, "(?<=\\d)\\.(?=\\d{3}(\\D|$))", "")
  x <- stringr::str_replace(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

# Heurísticas para columnas GPS
is_gps_col <- function(nm) grepl("lat|lon|long|alt|gps", nm, ignore.case = TRUE)

# Enteros “puros” (para detectar likert/binaras codificadas)
is_whole_number <- function(x) is.finite(x) & (abs(x - round(x)) < .Machine$double.eps^0.5)

# Cuali / cuanti (heurística)
infer_var_type <- function(x){
  if (is.character(x) || is.factor(x)) return("cualitativa")
  if (inherits(x, "labelled"))         return("cualitativa")
  if (is.numeric(x)) {
    ux <- unique(x[!is.na(x)])
    if (length(ux) <= 10 && all(is_whole_number(ux))) return("cualitativa") # Likert/binaria
    return("cuantitativa")
  }
  "cualitativa"
}

# ========= Lectura base (robusta) =========
# readxl -> openxlsx -> readxl con unzip interno (para casos de OneDrive)
.read_base_xlsx <- function(path_xlsx){
  stopifnot(file.exists(path_xlsx))
  
  # 1) readxl normal
  sh_try <- try(readxl::excel_sheets(path_xlsx), silent = TRUE)
  if (!inherits(sh_try, "try-error")) {
    sh <- sh_try
    sh_num   <- sh[grepl("numer|numeric", sh, ignore.case = TRUE)]
    sh_txt   <- sh[grepl("texto|text",   sh, ignore.case = TRUE)]
    sh_codes <- sh[grepl("codig|codigo", sh, ignore.case = TRUE)]
    if (length(sh_num)==0)   sh_num   <- sh[1]
    if (length(sh_txt)==0)   sh_txt   <- sh[min(2, length(sh))]
    if (length(sh_codes)==0) sh_codes <- sh[length(sh)]
    data_num <- readxl::read_excel(path_xlsx, sheet = sh_num[1]) |> janitor::clean_names()
    data_txt <- readxl::read_excel(path_xlsx, sheet = sh_txt[1]) |> janitor::clean_names()
    codigos  <- readxl::read_excel(path_xlsx, sheet = sh_codes[1])
    return(list(num = data_num, txt = data_txt, cod = codigos))
  }
  
  # 2) openxlsx
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    sh2_try <- try(openxlsx::getSheetNames(path_xlsx), silent = TRUE)
    if (!inherits(sh2_try, "try-error")) {
      sh <- sh2_try
      sh_num   <- sh[grep("numer|numeric", sh, ignore.case = TRUE)]
      sh_txt   <- sh[grep("texto|text",   sh, ignore.case = TRUE)]
      sh_codes <- sh[grep("codig|codigo", sh, ignore.case = TRUE)]
      if (length(sh_num)==0)   sh_num   <- sh[1]
      if (length(sh_txt)==0)   sh_txt   <- sh[min(2, length(sh))]
      if (length(sh_codes)==0) sh_codes <- sh[length(sh)]
      data_num <- openxlsx::read.xlsx(path_xlsx, sheet = sh_num[1])  |> janitor::clean_names()
      data_txt <- openxlsx::read.xlsx(path_xlsx, sheet = sh_txt[1])  |> janitor::clean_names()
      codigos  <- openxlsx::read.xlsx(path_xlsx, sheet = sh_codes[1])
      return(list(num = data_num, txt = data_txt, cod = codigos))
    }
  }
  
  # 3) readxl con unzip interno (índices 1/2/3 como fallback)
  old_unzip <- getOption("unzip"); on.exit(options(unzip = old_unzip), add = TRUE)
  options(unzip = "internal")
  data_num <- readxl::read_excel(path_xlsx, sheet = 1) |> janitor::clean_names()
  data_txt <- readxl::read_excel(path_xlsx, sheet = 2) |> janitor::clean_names()
  codigos  <- tryCatch(readxl::read_excel(path_xlsx, sheet = 3),
                       error = function(e) readxl::read_excel(path_xlsx, sheet = 2))
  list(num = data_num, txt = data_txt, cod = codigos)
}

# ========= Limpieza principal =========
# Devuelve: list(df_limpio, tipos)
# - No renombra columnas
# - Limpia strings (trim/squish)
# - Convierte GPS/altitud a numérico
# - Señala tipo cuali/cuanti (heurística)
clean_base <- function(path_xlsx_base,
                       use_text_sheet = TRUE){
  
  b <- .read_base_xlsx(path_xlsx_base)
  df_txt <- b$txt; df_num <- b$num
  
  df <- if (use_text_sheet) df_txt else df_num
  
  # 1) limpieza básica de valores string
  df <- df |> mutate(across(where(is.character), clean_strings))
  
  # 2) GPS/altitud a numérico si aplica
  for (v in names(df)) {
    if (is_gps_col(v)) df[[v]] <- parse_num_locale(df[[v]])
  }
  
  # 3) Tipos cuali/cuanti
  tipos <- tibble::tibble(
    variable      = names(df),
    tipo_inferido = vapply(df, infer_var_type, FUN.VALUE = character(1)),
    na            = vapply(df, function(z) sum(is.na(z)), integer(1)),
    unicos        = vapply(df, function(z) length(unique(z)), integer(1))
  )
  
  invisible(list(df_limpio = df, tipos = tipos))
}
