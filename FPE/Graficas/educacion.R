# ============================================================
# FIGURA — Torta nivel educativo (p5_agregado)
#           por ciudad y sexo (p40)
# ============================================================

library(tidyverse)
library(readxl)
library(stringr)
library(scales)

# -----------------------------
# 1) Rutas
# -----------------------------
input_cali <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/input_famd_cali_29102025.xlsx"
input_med  <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/input_famd_med_29102025.xlsx"

out_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Graficas/educacion"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(input_cali))
stopifnot(file.exists(input_med))

# -----------------------------
# 2) Paleta morado (sobria)
# -----------------------------
paleta_morado <- c(
  "Primaria o menos"        = "#4B1D70",
  "Secundaria"              = "#6A2C91",
  "Técnico / Tecnológico"   = "#9A6FB0",
  "Superior"                = "#C6A5D9"
)

# -----------------------------
# 3) Cargar y limpiar
# -----------------------------
prep_edu <- function(path_xlsx, ciudad_label) {
  df <- read_excel(path_xlsx)
  
  if (!all(c("p5_agregado", "p40") %in% names(df))) {
    stop(paste0("En ", ciudad_label, " faltan variables p5_agregado o p40"))
  }
  
  df %>%
    transmute(
      ciudad = ciudad_label,
      p5_agregado = str_squish(as.character(p5_agregado)),
      p40 = str_squish(as.character(p40))
    ) %>%
    mutate(
      p5_agregado = case_when(
        str_to_lower(p5_agregado) == "primaria o menos" ~ "Primaria o menos",
        str_to_lower(p5_agregado) == "secundaria" ~ "Secundaria",
        str_detect(str_to_lower(p5_agregado), "tecn") ~ "Técnico / Tecnológico",
        str_to_lower(p5_agregado) == "superior" ~ "Superior",
        TRUE ~ NA_character_
      ),
      sexo = case_when(
        str_detect(str_to_lower(p40), "homb") ~ "Hombre",
        str_detect(str_to_lower(p40), "muj")  ~ "Mujer",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(p5_agregado), !is.na(sexo))
}

datos <- bind_rows(
  prep_edu(input_cali, "Cali"),
  prep_edu(input_med,  "Medellín")
) %>%
  mutate(
    ciudad = factor(ciudad, levels = c("Cali", "Medellín")),
    sexo   = factor(sexo, levels = c("Mujer", "Hombre")),
    p5_agregado = factor(
      p5_agregado,
      levels = c("Primaria o menos", "Secundaria",
                 "Técnico / Tecnológico", "Superior")
    )
  )

# -----------------------------
# 4) Tabla para torta (%)
# -----------------------------
tabla_edu <- datos %>%
  count(ciudad, sexo, p5_agregado, name = "n") %>%
  group_by(ciudad, sexo) %>%
  mutate(
    pct = n / sum(n)
  ) %>%
  ungroup()

# -----------------------------
# 5) GRÁFICA — Torta (PORCENTAJES GRANDES Y BLANCOS)
# -----------------------------
p <- ggplot(tabla_edu, aes(x = "", y = pct, fill = p5_agregado)) +
  geom_col(width = 1, color = "white", linewidth = 0.4) +
  coord_polar(theta = "y") +
  facet_grid(sexo ~ ciudad) +
  geom_text(
    aes(label = percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",        # ✅ TEXTO BLANCO
    size = 6,               # ✅ TEXTO GRANDE
    fontface = "bold"
  ) +
  scale_fill_manual(values = paleta_morado) +
  labs(
    title = "Nivel educativo",
    subtitle = "Distribución porcentual por ciudad y sexo",
    fill = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = "right"
  )

ggsave(
  filename = file.path(out_dir, "fig_torta_nivel_educativo_p5_por_ciudad_sexo.png"),
  plot = p,
  width = 14,
  height = 10,
  dpi = 300
)

message(
  "Listo ✅ Torta guardada en: ",
  file.path(out_dir, "fig_torta_nivel_educativo_p5_por_ciudad_sexo.png")
)
