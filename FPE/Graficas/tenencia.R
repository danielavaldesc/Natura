# ============================================================
# TENENCIA: Licencia, Auto y Moto
# Por género y ciudad (Cali / Medellín)
# % con denominador GLOBAL (total Cali + Medellín) por género
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

out_dir  <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Graficas/tenencias"
out_xlsx <- file.path(out_dir, "trabajo_2_tenencia.xlsx")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1) Cargar y estandarizar (por ciudad) -> luego unimos
# -----------------------------
leer_ciudad <- function(path_xlsx, ciudad_nombre) {
  
  read_excel(path_xlsx) %>%
    transmute(
      ciudad = ciudad_nombre,
      genero = as.character(p40),
      
      # Licencia (p14)
      tiene_licencia = case_when(
        p14 %in% c("Si, auto", "Si, motocicleta", "Si de auto y motocicleta") ~ "Sí",
        p14 == "No" ~ "No",
        TRUE ~ NA_character_
      ),
      
      # Auto (p15 / p15_1)
      tiene_auto = if_else(
        suppressWarnings(as.numeric(p15)) > 0 |
          suppressWarnings(as.numeric(p15_1)) > 0,
        "Sí", "No", missing = NA_character_
      ),
      
      # Moto (p16 / p16_1)
      tiene_moto = if_else(
        suppressWarnings(as.numeric(p16)) > 0 |
          suppressWarnings(as.numeric(p16_1)) > 0,
        "Sí", "No", missing = NA_character_
      )
    ) %>%
    filter(genero %in% c("Hombre", "Mujer"))
}

df_all <- pmap_dfr(paths, ~ leer_ciudad(..2, ..1))

# -----------------------------
# 2) Denominadores globales por género (total Cali + Medellín)
#    OJO: el denominador se calcula por variable (porque hay NA distintos)
# -----------------------------
denom_global <- function(var) {
  df_all %>%
    filter(!is.na(.data[[var]])) %>%
    count(genero, name = "N_total_genero")
}

# -----------------------------
# 3) Tabla por indicador: n (Sí) por ciudad×género, % sobre N_total_genero
# -----------------------------
tabla_indicador_global <- function(var, nombre) {
  
  den <- denom_global(var)
  
  df_all %>%
    filter(!is.na(.data[[var]])) %>%
    filter(.data[[var]] == "Sí") %>%
    count(ciudad, genero, name = "n_si") %>%
    left_join(den, by = "genero") %>%
    mutate(pct = 100 * n_si / N_total_genero) %>%
    transmute(
      ciudad,
      Categoria = nombre,
      Genero = genero,
      Valor = paste0(n_si, " (", round(pct, 1), "%)")
    ) %>%
    pivot_wider(names_from = Genero, values_from = Valor) %>%
    arrange(ciudad, Categoria)
}

tabla_long <- bind_rows(
  tabla_indicador_global("tiene_licencia", "Posee licencia"),
  tabla_indicador_global("tiene_auto",     "Posee auto"),
  tabla_indicador_global("tiene_moto",     "Posee motocicleta")
)

# -----------------------------
# 4) Separar por ciudad para exportar como antes
# -----------------------------
tabla_cali     <- tabla_long %>% filter(ciudad == "Cali") %>% select(-ciudad)
tabla_medellin <- tabla_long %>% filter(ciudad == "Medellín") %>% select(-ciudad)

# -----------------------------
# 5) Exportar a Excel
# -----------------------------
wb <- createWorkbook()

addWorksheet(wb, "Cali")
writeData(wb, "Cali", tabla_cali)

addWorksheet(wb, "Medellín")
writeData(wb, "Medellín", tabla_medellin)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("Listo ✅\nArchivo Excel generado en:\n", out_xlsx)

# -----------------------------
# 6) Chequeos rápidos (recomendados)
# -----------------------------
# Denominadores globales por variable y género
# denom_global("tiene_licencia")
# denom_global("tiene_auto")
# denom_global("tiene_moto")
