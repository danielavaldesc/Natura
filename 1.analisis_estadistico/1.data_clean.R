# -----------------------------------------------
# 1.data_clean.R
# Ejecuta limpieza (0.data_base.R) para Cali y Medellín - RUTAS FIJAS
# SOLO exporta la base limpia (sin renombrados ni diccionarios)
# -----------------------------------------------

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(janitor); library(readr); library(tibble)
})

# === RUTA BASE DEL PROYECTO ===
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Natura"
setwd(base_dir)

source(file.path("1.analisis_estadistico", "0.data_base.R"))

# === RUTAS FIJAS ===
archivo_cali <- file.path(base_dir, "Input", "BD Base Movilidad Cali 2025_V01_Cliente.xlsx")
archivo_med  <- file.path(base_dir, "Input", "BD Base Movilidad Medellin 2025_Cliente.xlsx")  # sin tilde

# Verificación rápida
verifica <- function(p){
  cat("→", normalizePath(p, winslash = "\\"), "\n")
  tibble(existe = file.exists(p),
         size   = if (file.exists(p)) file.info(p)$size else NA_real_)
}
cat("Archivo CALI:\n");     print(verifica(archivo_cali))
cat("Archivo MEDELLIN:\n"); print(verifica(archivo_med))

dir.create("salidas", showWarnings = FALSE)

# === CALI ===
res_cali <- clean_base(
  path_xlsx_base = archivo_cali,
  use_text_sheet = TRUE
)
readr::write_csv(res_cali$df_limpio, file.path("salidas", "cali_limpio.csv"), na = "")
cat("✔ CALI listo (", nrow(res_cali$df_limpio), " filas)\n", sep = "")

# === MEDELLIN ===
res_med <- clean_base(
  path_xlsx_base = archivo_med,
  use_text_sheet = TRUE
)
readr::write_csv(res_med$df_limpio, file.path("salidas", "medellin_limpio.csv"), na = "")
cat("✔ MEDELLIN listo (", nrow(res_med$df_limpio), " filas)\n", sep = "")

message("Listo: bases limpias exportadas a carpeta 'salidas/'")

