setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\")
library(readxl)
library(dplyr)
library(stringr)
library(purrr)

# Lee un .xlsx de forma segura
read_xlsx_safe <- function(path) {
  tryCatch(readxl::read_excel(path),
           error = function(e) { warning(sprintf("No se pudo leer %s: %s", path, e$message)); NULL })
}

# Une todas las tablas encontradas para un módulo (m1..m4)
load_module_any <- function(m) {
  dir_mod <- sprintf("output/m%d", m)
  # lista todos los .xlsx del módulo
  files <- list.files(dir_mod, pattern = sprintf("_mod%d_.*\\.xlsx$", m), full.names = TRUE)
  if (length(files) == 0) {
    warning(sprintf("No se encontraron archivos para el módulo %d en %s", m, dir_mod))
    return(NULL)
  }
  
  # Lee todo lo que exista
  tabs <- lapply(files, read_xlsx_safe)
  # descarta NULLs (si algún archivo falló)
  keep <- !vapply(tabs, is.null, logical(1))
  tabs <- tabs[keep]
  files <- files[keep]
  
  # Ordena para que queden primero gender/ses/travel si existen (opcional)
  order_key <- c("by_gender", "by_ses", "by_travel_mode",
                 "by_education", "by_main_activity", "by_age_r")
  suffix <- str_remove(basename(files), "\\.xlsx$")
  suffix <- str_extract(suffix, "(?<=_mod\\d_).*$")
  ord <- order(match(suffix, order_key), na.last = TRUE)
  tabs <- tabs[ord]
  
  # Une por claves estándar
  merged <- Reduce(function(x, y) dplyr::full_join(x, y, by = c("Variable", "Label", "Categoria")), tabs)
  merged
}

# Cargar y unir cada módulo (tomará todo lo que encuentre: gender, ses, travel_mode, education, main_activity, age_r)
mod1 <- load_module_any(1)
mod2 <- load_module_any(2)
mod3 <- load_module_any(3)
mod4 <- load_module_any(4)

# (Opcional) añadir etiqueta de módulo y consolidar
if (!is.null(mod1)) mod1$Modulo <- "Módulo 1"
if (!is.null(mod2)) mod2$Modulo <- "Módulo 2"
if (!is.null(mod3)) mod3$Modulo <- "Módulo 3"
if (!is.null(mod4)) mod4$Modulo <- "Módulo 4"

consolidado <- bind_rows(mod1, mod2, mod3, mod4)

# (Opcional) exportar resultados
dir.create("output/summary", recursive = TRUE, showWarnings = FALSE)
if (!is.null(mod1)) writexl::write_xlsx(mod1, "output/summary/merged_mod1.xlsx")
if (!is.null(mod2)) writexl::write_xlsx(mod2, "output/summary/merged_mod2.xlsx")
if (!is.null(mod3)) writexl::write_xlsx(mod3, "output/summary/merged_mod3.xlsx")
if (!is.null(mod4)) writexl::write_xlsx(mod4, "output/summary/merged_mod4.xlsx")
writexl::write_xlsx(consolidado, "output/summary/merged_all_modules.xlsx")


