library(dplyr)
library(tidyr)

# -----------------------------
# 1) Datos tal como aparecen en las imágenes
# -----------------------------
datos_imagenes <- tibble::tribble(
  ~ciudad,     ~genero,  ~tipo_viaje, ~n,
  "Cali",      "Hombre", "Pendular",   3,
  "Cali",      "Hombre", "Poligonal",  1,
  "Cali",      "Mujer",  "Pendular",   8,
  "Cali",      "Mujer",  "Poligonal",  5,
  "Medellín",  "Hombre", "Pendular",   6,
  "Medellín",  "Hombre", "Poligonal",  2,
  "Medellín",  "Mujer",  "Pendular",   7,
  "Medellín",  "Mujer",  "Poligonal",  5
)

# -----------------------------
# 2) Proporción dentro del total de cada género por ciudad
# -----------------------------
tabla_proporciones <- datos_imagenes %>%
  group_by(ciudad, genero) %>%
  mutate(
    total_genero_ciudad = sum(n),
    prop = n / total_genero_ciudad,
    pct  = round(100 * prop, 1)
  ) %>%
  ungroup()

View(tabla_proporciones)
