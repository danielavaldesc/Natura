# Agregar tablas de resumen descriptivo (Cali)
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\")

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(writexl)

# === Helpers ================================================================

read_xlsx_safe <- function(path) {
  tryCatch(read_excel(path),
           error = function(e) { warning(sprintf("No se pudo leer %s: %s", path, e$message)); NULL })
}

# Une todas las tablas disponibles para un módulo (m1..m4)
load_module <- function(m) {
  dir_mod <- sprintf("output/m%d", m)
  if (!dir.exists(dir_mod)) {
    warning(sprintf("No existe la carpeta %s", dir_mod))
    return(NULL)
  }
  
  # Sufijos que buscamos (incluye los de la imagen)
  wanted_suffix <- c("by_gender","by_ses","by_travel_mode",
                     "by_education","by_main_activity","by_age_r")
  
  # Encuentra todos los .xlsx del módulo y se queda con los sufijos deseados
  files_all <- list.files(dir_mod, pattern = sprintf("_mod%d_.*\\.xlsx$", m), full.names = TRUE)
  if (length(files_all) == 0) {
    warning(sprintf("No se encontraron archivos para el módulo %d en %s", m, dir_mod))
    return(NULL)
  }
  
  # Extrae el sufijo real (después de _mod{m}_)
  suffix <- str_extract(basename(files_all), sprintf("(?<=_mod%d_).*(?=\\.xlsx$)", m))
  keep <- suffix %in% wanted_suffix
  files <- files_all[keep]
  suffix <- suffix[keep]
  
  if (length(files) == 0) {
    warning(sprintf("No hay archivos de los sufijos esperados en %s", dir_mod))
    return(NULL)
  }
  
  # Ordena por el orden sugerido (gender, ses, travel, education, main_activity, age_r)
  ord <- order(match(suffix, wanted_suffix))
  files <- files[ord]
  suffix <- suffix[ord]
  
  tabs <- lapply(files, read_xlsx_safe)
  # descarta NULLs
  keep2 <- !vapply(tabs, is.null, logical(1))
  tabs <- tabs[keep2]
  suffix <- suffix[keep2]
  
  # Une por claves estándar
  merged <- Reduce(function(x, y) full_join(x, y, by = c("Variable", "Label", "Categoria")), tabs)
  
  # Devuelve lista con el merged y los nombres que se usaron
  list(
    merged = merged,
    used_files = setNames(files[keep2], suffix)
  )
}

# === Carga por módulos y exporta ===========================================

dir.create("output/summary", recursive = TRUE, showWarnings = FALSE)

mod1_res <- load_module(1)
mod2_res <- load_module(2)
mod3_res <- load_module(3)
mod4_res <- load_module(4)

mod1 <- if (!is.null(mod1_res)) mod1_res$merged else NULL
mod2 <- if (!is.null(mod2_res)) mod2_res$merged else NULL
mod3 <- if (!is.null(mod3_res)) mod3_res$merged else NULL
mod4 <- if (!is.null(mod4_res)) mod4_res$merged else NULL

# Etiquetas y consolidado
if (!is.null(mod1)) mod1$Modulo <- "Módulo 1"
if (!is.null(mod2)) mod2$Modulo <- "Módulo 2"
if (!is.null(mod3)) mod3$Modulo <- "Módulo 3"
if (!is.null(mod4)) mod4$Modulo <- "Módulo 4"

consolidado <- bind_rows(mod1, mod2, mod3, mod4)

# Exportar por módulo (solo si existen) y consolidado
if (!is.null(mod1)) write_xlsx(mod1, "output/summary/merged_mod1.xlsx")
if (!is.null(mod2)) write_xlsx(mod2, "output/summary/merged_mod2.xlsx")
if (!is.null(mod3)) write_xlsx(mod3, "output/summary/merged_mod3.xlsx")
if (!is.null(mod4)) write_xlsx(mod4, "output/summary/merged_mod4.xlsx")
write_xlsx(consolidado, "output/summary/merged_all_modules.xlsx")


