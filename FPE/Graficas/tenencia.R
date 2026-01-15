# ============================================================
# TENENCIA: Licencia, Auto y Moto
# Por género y ciudad (Cali / Medellín)
# % sobre total de hombres y mujeres
# ============================================================

library(tidyverse)
library(readxl)
library(openxlsx)

# -----------------------------
# Rutas
# -----------------------------
paths <- tibble(
  ciudad = c("Cali", "Medellín"),
  path = c(
    "C:/Users/danie/OneDrive/Escritorio/Natura/271025_Results_Med/input/BD Base Movilidad Cali 2025_V01_Cliente.xlsx",
    "C:/Users/danie/OneDrive/Escritorio/Natura/271025_Results_Med/input/BD Base Movilidad Medellin 2025_Cliente.xlsx"
  )
)

# 👉 Carpeta de salida CORRECTA
out_dir  <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Graficas/tenencias"
out_xlsx <- file.path(out_dir, "trabajo_2_tenencia.xlsx")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Función de procesamiento
# -----------------------------
procesar_ciudad <- function(path_xlsx, ciudad_nombre) {
  
  df <- read_excel(path_xlsx) %>%
    transmute(
      ciudad = ciudad_nombre,
      genero = as.character(p40),
      
      # -------------------------
      # Licencia (p14)
      # -------------------------
      tiene_licencia = case_when(
        p14 %in% c("Si, auto", "Si, motocicleta", "Si de auto y motocicleta") ~ "Sí",
        p14 == "No" ~ "No",
        TRUE ~ NA_character_
      ),
      
      # -------------------------
      # Auto (p15 / p15_1)
      # -------------------------
      tiene_auto = if_else(
        suppressWarnings(as.numeric(p15)) > 0 |
          suppressWarnings(as.numeric(p15_1)) > 0,
        "Sí", "No", missing = NA_character_
      ),
      
      # -------------------------
      # Motocicleta (p16 / p16_1)
      # -------------------------
      tiene_moto = if_else(
        suppressWarnings(as.numeric(p16)) > 0 |
          suppressWarnings(as.numeric(p16_1)) > 0,
        "Sí", "No", missing = NA_character_
      )
    ) %>%
    filter(genero %in% c("Hombre", "Mujer"))
  
  # -----------------------------
  # Función tabla por indicador
  # -----------------------------
  tabla_indicador <- function(var, nombre) {
    df %>%
      filter(!is.na(.data[[var]])) %>%
      count(genero, .data[[var]], name = "n") %>%
      group_by(genero) %>%
      mutate(pct = 100 * n / sum(n)) %>%
      ungroup() %>%
      filter(.data[[var]] == "Sí") %>%
      transmute(
        Categoria = nombre,
        Genero = genero,
        Valor = paste0(n, " (", round(pct, 1), "%)")
      ) %>%
      pivot_wider(
        names_from = Genero,
        values_from = Valor
      )
  }
  
  bind_rows(
    tabla_indicador("tiene_licencia", "Posee licencia"),
    tabla_indicador("tiene_auto",     "Posee auto"),
    tabla_indicador("tiene_moto",     "Posee motocicleta")
  )
}

# -----------------------------
# Procesar ciudades
# -----------------------------
tabla_cali     <- procesar_ciudad(paths$path[1], "Cali")
tabla_medellin <- procesar_ciudad(paths$path[2], "Medellín")

# -----------------------------
# Exportar a Excel
# -----------------------------
wb <- createWorkbook()

addWorksheet(wb, "Cali")
writeData(wb, "Cali", tabla_cali)

addWorksheet(wb, "Medellín")
writeData(wb, "Medellín", tabla_medellin)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message(
  "Listo ✅\n",
  "Archivo Excel generado en:\n",
  out_xlsx
)

