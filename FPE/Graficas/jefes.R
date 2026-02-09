# ============================================================
# FIGURA — Torta % (Hombre vs Mujer) "Jefes(as) de hogar"
# ============================================================

library(tidyverse)
library(readxl)
library(forcats)
library(stringr)

# -----------------------------
# 1) Rutas
# -----------------------------
input_cali <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/BD Base Movilidad Cali 2025_V01_Cliente.xlsx"
input_med  <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/BD Base Movilidad Medellin 2025_Cliente.xlsx"

out_dir <- file.path("C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Graficas/jefes")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(input_cali))
stopifnot(file.exists(input_med))

# -----------------------------
# 2) Paleta azul
# -----------------------------
colores_genero <- c(
  "Hombre" = "#1F3A5F",  # oscuro
  "Mujer"  = "#4A90C2"   # claro
)

# -----------------------------
# 2.1) Tema global con letra MÁS GRANDE
# -----------------------------
tema_grande <- theme_minimal(base_size = 17) +
  theme(
    plot.title    = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 17),
    strip.text    = element_text(size = 18, face = "bold"),
    legend.text   = element_text(size = 16),
    legend.title  = element_text(size = 16),
    axis.text     = element_text(size = 16),
    axis.title    = element_text(size = 17)
  )

# -----------------------------
# 3) Filtros p8 (según imágenes)
# -----------------------------
p8_hombre <- c(
  "Con su pareja",
  "Con su pareja y otros familiares",
  "Con su pareja y sus hijos o hijas",
  "Con su pareja, sus hijos y otros familiares",
  "Con sus hijos y otros familiares",
  "Sola(o)",
  "Sola(o) con sus hijas o hijos"
)

p8_mujer <- c(
  "Con otros familiares diferentes a sus hijos o pareja",
  "Con sus hijos y otros familiares",
  "Sola(o)",
  "Sola(o) con sus hijas o hijos"
)

# -----------------------------
# 4) Cargar SOLO columnas necesarias
# -----------------------------
prep_min <- function(path_xlsx, ciudad_label) {
  df <- read_excel(path_xlsx)
  
  required_cols <- c("p40", "p8")
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(paste0("Faltan columnas en ", ciudad_label, ": ", paste(missing, collapse = ", ")))
  }
  
  df %>%
    transmute(
      ciudad = ciudad_label,
      p40    = as.character(p40),
      p8     = str_squish(as.character(p8))
    ) %>%
    mutate(
      genero_2 = case_when(
        p40 == "Hombre" ~ "Hombre",
        p40 == "Mujer"  ~ "Mujer",
        TRUE            ~ NA_character_
      )
    ) %>%
    filter(!is.na(genero_2), !is.na(p8))
}

datos <- bind_rows(
  prep_min(input_cali, "Cali"),
  prep_min(input_med,  "Medellín")
) %>%
  mutate(
    genero_2 = fct_relevel(genero_2, "Hombre", "Mujer"),
    ciudad   = factor(ciudad, levels = c("Cali", "Medellín"))
  )

# -----------------------------
# 5) Construir "jefes(as) de hogar"
# -----------------------------
datos_jefes <- datos %>%
  filter(
    (genero_2 == "Hombre" & p8 %in% p8_hombre) |
      (genero_2 == "Mujer" & p8 %in% p8_mujer)
  )

# -----------------------------
# Helper: color de texto según luminancia del HEX
# (oscuro => blanco, claro => negro)
# -----------------------------
texto_color_por_fill <- function(fill_hex) {
  hex <- gsub("#", "", fill_hex)
  r <- strtoi(substr(hex, 1, 2), 16L) / 255
  g <- strtoi(substr(hex, 3, 4), 16L) / 255
  b <- strtoi(substr(hex, 5, 6), 16L) / 255
  lum <- 0.2126 * r + 0.7152 * g + 0.0722 * b
  ifelse(lum < 0.55, "white", "black")
}

# ============================================================
# FUNCIÓN: Torta por ciudad (SIN facet) + label grande
# ============================================================
hacer_torta_jefes <- function(df_ciudad, ciudad_nombre, out_dir) {
  
  df_pie <- df_ciudad %>%
    count(genero_2) %>%
    mutate(
      porcentaje  = 100 * n / sum(n),
      label       = paste0(round(porcentaje, 1), "%"),
      fill_hex    = colores_genero[as.character(genero_2)],
      label_color = texto_color_por_fill(fill_hex)
    )
  
  p_pie <- ggplot(df_pie, aes(x = "", y = porcentaje, fill = genero_2)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = label, color = label_color),
      position = position_stack(vjust = 0.5),
      size = 6.6,              # 👈 MÁS GRANDE
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_fill_manual(values = colores_genero) +
    scale_color_identity() +
    labs(
      title = paste0("Jefes(as) de hogar (definición por p8) — ", ciudad_nombre),
      fill = NULL
    ) +
    tema_grande +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      legend.position = "top"
    )
  
  out_png <- file.path(out_dir, paste0("fig_pie_jefes_hogar_por_genero_", ciudad_nombre, ".png"))
  
  ggsave(
    filename = out_png,
    plot = p_pie,
    width = 9.5, height = 7.2, dpi = 300
  )
  
  message("✔ Listo jefes(as): ", ciudad_nombre, "\n- ", out_png)
  
  invisible(list(df = df_pie, plot = p_pie, file = out_png))
}

# ============================================================
# EJECUCIÓN: Cali y Medellín por separado (SIN facet)
# ============================================================
jefes_cali <- datos_jefes %>% filter(ciudad == "Cali")
jefes_med  <- datos_jefes %>% filter(ciudad == "Medellín")

hacer_torta_jefes(jefes_cali, "Cali", out_dir)
hacer_torta_jefes(jefes_med,  "Medellin", out_dir)

# ============================================================
# TABLA CHECK (por ciudad y género)
# ============================================================
tabla_check <- datos_jefes %>%
  count(ciudad, genero_2) %>%
  arrange(ciudad, genero_2)

write_csv(tabla_check, file.path(out_dir, "tabla_check_jefes_hogar_conteos.csv"))

# -----------------------------
# Mensaje final
# -----------------------------
message(
  "✅ Listo. Guardé:\n",
  "- fig_pie_jefes_hogar_por_genero_Cali.png\n",
  "- fig_pie_jefes_hogar_por_genero_Medellin.png\n",
  "- tabla_check_jefes_hogar_conteos.csv\n",
  "En: ", out_dir
)

# ============================================================
# TABLA — Por ciudad: Categoria x (Hombre/Mujer) con "n (pct%)"
# ============================================================

library(tidyr)
library(dplyr)

tabla_ciudad_cat <- datos_jefes %>%
  mutate(Categoria = "Jefes(as) de hogar (p8)") %>%
  count(ciudad, Categoria, genero_2, name = "n") %>%
  group_by(ciudad) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(valor = paste0(n, " (", round(pct, 1), "%)")) %>%
  select(ciudad, Categoria, genero_2, valor) %>%
  pivot_wider(
    names_from = genero_2,
    values_from = valor,
    values_fill = "0 (0%)"
  ) %>%
  arrange(ciudad, Categoria)

tabla_ciudad_cat
