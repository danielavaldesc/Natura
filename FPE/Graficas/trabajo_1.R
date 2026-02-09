# ============================================================
# SCRIPT R — Trabajo (1)
# ============================================================

library(tidyverse)
library(readxl)
library(forcats)
library(stringr)

# -----------------------------
# Rutas
# -----------------------------
input_cali <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/BD Base Movilidad Cali 2025_V01_Cliente.xlsx"
input_med  <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/BD Base Movilidad Medellin 2025_Cliente.xlsx"

stopifnot(file.exists(input_cali))
stopifnot(file.exists(input_med))

out_dir <- file.path("C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Graficas/trabaja")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Paleta azul
# -----------------------------
colores_genero <- c(
  "Hombre" = "#4A90C2",
  "Mujer"  = "#5B4B8A"
)

# -----------------------------
# Tema global con letra MÁS GRANDE
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
# Cargar SOLO p40 y p7
# -----------------------------
prep_p7 <- function(path_xlsx, ciudad_label) {
  df <- read_excel(path_xlsx)
  
  required_cols <- c("p40", "p7")
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(paste0("Faltan columnas en ", ciudad_label, ": ", paste(missing, collapse = ", ")))
  }
  
  df %>%
    transmute(
      ciudad = ciudad_label,
      p40 = as.character(p40),
      p7  = str_squish(as.character(p7))
    ) %>%
    mutate(
      genero_2 = case_when(
        p40 == "Hombre" ~ "Hombre",
        p40 == "Mujer"  ~ "Mujer",
        TRUE            ~ NA_character_
      )
    ) %>%
    filter(!is.na(genero_2), !is.na(p7))
}

datos <- bind_rows(
  prep_p7(input_cali, "Cali"),
  prep_p7(input_med,  "Medellín")
) %>%
  mutate(
    genero_2 = fct_relevel(genero_2, "Hombre", "Mujer"),
    ciudad   = factor(ciudad, levels = c("Cali", "Medellín"))
  )

# -----------------------------
# Filtrar SOLO quienes "Trabajar"
# (acepta código 2 o etiqueta "Trabajar")
# -----------------------------
trabajan <- datos %>%
  filter(p7 == "2" | str_to_lower(p7) == "trabajar")

# ============================================================
# FUNCIÓN: Figura torta por ciudad (SIN facet) + labels grandes
# ============================================================
hacer_torta_trabajan <- function(df_ciudad, ciudad_nombre, out_dir) {
  
  df_pie <- df_ciudad %>%
    count(genero_2) %>%
    mutate(
      porcentaje = 100 * n / sum(n),
      label = paste0(genero_2, "\n", round(porcentaje, 1), "%"),
      # Texto blanco si el segmento es oscuro (Mujer)
      label_color = if_else(genero_2 == "Mujer", "white", "black")
    )
  
  p_pie <- ggplot(df_pie, aes(x = "", y = porcentaje, fill = genero_2)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = label, color = label_color),
      position = position_stack(vjust = 0.5),
      size = 6.2,              # 👈 MÁS GRANDE
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_fill_manual(values = colores_genero) +
    scale_color_identity() +
    labs(
      title = paste0("Personas que trabajan (p7) — ", ciudad_nombre),
      fill = NULL
    ) +
    tema_grande +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      legend.position = "top"
    )
  
  out_png <- file.path(out_dir, paste0("fig_pie_trabajan_por_genero_", ciudad_nombre, ".png"))
  
  ggsave(
    filename = out_png,
    plot = p_pie,
    width = 9.5, height = 7.2, dpi = 300
  )
  
  message("✔ Listo torta: ", ciudad_nombre, "\n- ", out_png)
  
  invisible(list(df = df_pie, plot = p_pie, file = out_png))
}

# ============================================================
# EJECUCIÓN: Cali y Medellín por separado (SIN facet)
# ============================================================
trabajan_cali <- trabajan %>% filter(ciudad == "Cali")
trabajan_med  <- trabajan %>% filter(ciudad == "Medellín")

hacer_torta_trabajan(trabajan_cali, "Cali", out_dir)
hacer_torta_trabajan(trabajan_med,  "Medellin", out_dir)

# -----------------------------
# Mensaje final
# -----------------------------
message("✅ Todo guardado en: ", out_dir)

# ============================================================
# % que trabajan por género SOBRE el TOTAL de la muestra
# ============================================================

# Total muestra (global y por ciudad)
totales_global <- datos %>%
  summarise(N_total = n())

totales_ciudad <- datos %>%
  count(ciudad, name = "N_total")

# Conteo de quienes trabajan (global y por ciudad) por género
trabajan_global <- trabajan %>%
  count(genero_2, name = "N_trabajan") %>%
  mutate(
    pct_sobre_total_muestra = 100 * N_trabajan / totales_global$N_total
  )

trabajan_ciudad <- trabajan %>%
  count(ciudad, genero_2, name = "N_trabajan") %>%
  left_join(totales_ciudad, by = "ciudad") %>%
  mutate(
    pct_sobre_total_muestra_ciudad = 100 * N_trabajan / N_total
  )

# (Opcional) ver tablas en consola
print(trabajan_global)
print(trabajan_ciudad)

# ============================================================
# Si además quieres AMBAS cosas:
# A) composición entre quienes trabajan (lo que ya tenías)
# B) prevalencia sobre el total de la muestra (nuevo)
# ============================================================

trabajan_ciudad_ambos <- trabajan %>%
  count(ciudad, genero_2, name = "N_trabajan") %>%
  group_by(ciudad) %>%
  mutate(pct_dentro_de_trabajan_ciudad = 100 * N_trabajan / sum(N_trabajan)) %>%
  ungroup() %>%
  left_join(totales_ciudad, by = "ciudad") %>%
  mutate(pct_sobre_total_muestra_ciudad = 100 * N_trabajan / N_total)

print(trabajan_ciudad_ambos)

