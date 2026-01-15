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

# -----------------------------
# Torta TOTAL (Cali + Medellín)
# -----------------------------
df_pie_trab_total <- trabajan %>%
  count(genero_2) %>%
  mutate(
    porcentaje = 100 * n / sum(n),
    label = paste0(genero_2, "\n", round(porcentaje, 1), "%")
  )

p_pie_trab_total <- ggplot(df_pie_trab_total, aes(x = "", y = porcentaje, fill = genero_2)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = colores_genero) +
  labs(
    title = "Personas que trabajan (p7) — Distribución por género",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_dir, "fig_pie_trabajan_por_genero_total.png"),
  plot = p_pie_trab_total,
  width = 8, height = 6, dpi = 300
)

# -----------------------------
# (Opcional) Por ciudad
# -----------------------------
df_pie_trab_ciudad <- trabajan %>%
  count(ciudad, genero_2) %>%
  group_by(ciudad) %>%
  mutate(
    porcentaje = 100 * n / sum(n),
    label = paste0(genero_2, "\n", round(porcentaje, 1), "%")
  ) %>%
  ungroup()

p_pie_trab_ciudad <- ggplot(df_pie_trab_ciudad, aes(x = "", y = porcentaje, fill = genero_2)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3.7) +
  facet_wrap(~ ciudad) +
  scale_fill_manual(values = colores_genero) +
  labs(
    title = "Personas que trabajan (p7) — Por ciudad y género",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_dir, "fig_pie_trabajan_por_genero_por_ciudad.png"),
  plot = p_pie_trab_ciudad,
  width = 12, height = 6, dpi = 300
)

message(
  "Listo. Guardé:\n",
  "- fig_pie_trabajan_por_genero_total.png\n",
  "- fig_pie_trabajan_por_genero_por_ciudad.png\n",
  "En: ", out_dir
)
