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
    axis.text     = element_text(size = 16)
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
      p7  = as.character(p7)
    ) %>%
    mutate(
      genero_2 = case_when(
        p40 == "Hombre" ~ "Hombre",
        p40 == "Mujer"  ~ "Mujer",
        TRUE            ~ NA_character_
      ),
      p7 = str_squish(p7)
    ) %>%
    filter(!is.na(genero_2), !is.na(p7))
}

cali <- prep_p7(input_cali, "Cali")
med  <- prep_p7(input_med,  "Medellín")

datos <- bind_rows(cali, med) %>%
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
# Torta TOTAL (Cali + Medellín)
# ============================================================
df_pie_trab_total <- trabajan %>%
  count(genero_2) %>%
  mutate(
    porcentaje = 100 * n / sum(n),
    label = paste0(genero_2, "\n", round(porcentaje, 1), "%"),
    # ✅ Texto blanco si el color del segmento es oscuro (aquí: Mujer)
    label_color = if_else(genero_2 == "Mujer", "white", "black")
  )

p_pie_trab_total <- ggplot(df_pie_trab_total, aes(x = "", y = porcentaje, fill = genero_2)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label, color = label_color),
    position = position_stack(vjust = 0.5),
    size = 6.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = colores_genero) +
  scale_color_identity() +
  labs(
    title = "Personas que trabajan (p7) — Distribución por género",
    fill = NULL
  ) +
  tema_grande +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top"
  )

ggsave(
  filename = file.path(out_dir, "fig_pie_trabajan_por_genero_total.png"),
  plot = p_pie_trab_total,
  width = 9.5, height = 7.2, dpi = 300
)

# ============================================================
# Por ciudad
# ============================================================
df_pie_trab_ciudad <- trabajan %>%
  count(ciudad, genero_2) %>%
  group_by(ciudad) %>%
  mutate(
    porcentaje = 100 * n / sum(n),
    label = paste0(genero_2, "\n", round(porcentaje, 1), "%"),
    # ✅ Texto blanco si el color del segmento es oscuro (aquí: Mujer)
    label_color = if_else(genero_2 == "Mujer", "white", "black")
  ) %>%
  ungroup()

p_pie_trab_ciudad <- ggplot(df_pie_trab_ciudad, aes(x = "", y = porcentaje, fill = genero_2)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label, color = label_color),
    position = position_stack(vjust = 0.5),
    size = 5.8,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~ ciudad) +
  scale_fill_manual(values = colores_genero) +
  scale_color_identity() +
  labs(
    title = "Personas que trabajan (p7) — Por ciudad y género",
    fill = NULL
  ) +
  tema_grande +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top"
  )

ggsave(
  filename = file.path(out_dir, "fig_pie_trabajan_por_genero_por_ciudad.png"),
  plot = p_pie_trab_ciudad,
  width = 14, height = 7.4, dpi = 300
)

message(
  "Listo. Guardé:\n",
  "- fig_pie_trabajan_por_genero_total.png\n",
  "- fig_pie_trabajan_por_genero_por_ciudad.png\n",
  "En: ", out_dir
)
