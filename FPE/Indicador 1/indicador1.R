# ============================================================
# Indicador 1: Carga total de tiempo de movilidad
# VERSION: por CIUDAD (sin facet) + SIN texto "Mediana"
# ============================================================

# -----------------------------
# 0) Paquetes
# -----------------------------
pkgs <- c("tidyverse", "readxl", "forcats", "tidyr", "ragg")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(readxl)
library(forcats)
library(tidyr)
library(ragg)

# -----------------------------
# 1) Paleta azul
# -----------------------------
colores_genero <- c(
  "Hombre" = "#1F3A5F",
  "Mujer"  = "#4A90C2"
)

# -----------------------------
# 2) Rutas
# -----------------------------
base_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura"

input_cali <- file.path(base_dir, "201025_Results_Cali", "output", "input_famd_cali_29102025.xlsx")
input_med  <- file.path(base_dir, "271025_Results_Med",  "output", "input_famd_med_29102025.xlsx")

out_dir <- file.path(base_dir, "FPE", "Indicador 1", "Por_Ciudad")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(input_cali), file.exists(input_med))

# -----------------------------
# 3) Preparar datos
# -----------------------------
prep_dataset <- function(path_xlsx, ciudad_label) {
  df <- read_excel(path_xlsx)
  
  df %>%
    mutate(
      ciudad = ciudad_label,
      genero_2 = case_when(
        p40 == "Hombre" ~ "Hombre",
        p40 == "Mujer"  ~ "Mujer",
        TRUE            ~ NA_character_
      )
    ) %>%
    filter(!is.na(tiempo_total), !is.na(genero_2))
}

datos <- bind_rows(
  prep_dataset(input_cali, "Cali"),
  prep_dataset(input_med,  "Medellín")
) %>%
  mutate(
    genero_2 = fct_relevel(genero_2, "Hombre", "Mujer")
  )

# -----------------------------
# 4) Resumen por género y ciudad
# -----------------------------
resumen_genero <- datos %>%
  group_by(ciudad, genero_2) %>%
  summarise(
    promedio = mean(tiempo_total, na.rm = TRUE),
    mediana  = median(tiempo_total, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 5) FIGURA — Promedio (barra) + línea punteada (SIN texto)
# ============================================================

bar_width  <- 0.70
line_width <- bar_width * 0.70

tema_fig <- theme_minimal(base_size = 24) +
  theme(
    plot.title = element_text(size = 36, face = "bold"),
    axis.text  = element_text(size = 22),
    axis.title = element_text(size = 26),
    legend.position = "none",
    plot.margin = margin(14, 20, 14, 20)
  )

hacer_fig_ciudad <- function(ciudad_nombre) {
  
  df <- resumen_genero %>%
    filter(ciudad == ciudad_nombre)
  
  p <- ggplot(df, aes(x = genero_2, y = promedio, fill = genero_2)) +
    
    # Barra = PROMEDIO
    geom_col(width = bar_width) +
    
    # Etiqueta promedio
    geom_text(
      aes(label = round(promedio, 1)),
      vjust = -0.35,
      size = 8,
      fontface = "bold"
    ) +
    
    # Línea punteada blanca (mediana, sin texto)
    geom_segment(
      aes(
        x    = as.numeric(genero_2) - (line_width / 2),
        xend = as.numeric(genero_2) + (line_width / 2),
        y    = mediana,
        yend = mediana
      ),
      inherit.aes = FALSE,
      linetype = "dashed",
      linewidth = 1.6,
      color = "white"
    ) +
    
    scale_fill_manual(values = colores_genero) +
    labs(
      x = NULL,
      y = "Minutos",
      title = paste("Tiempo total de movilidad por género —", ciudad_nombre)
    ) +
    tema_fig +
    coord_cartesian(clip = "off")
  
  out_png <- file.path(out_dir, paste0("fig_1_tiempo_movilidad_", ciudad_nombre, ".png"))
  
  ggsave(
    filename = out_png,
    plot = p,
    width = 12,
    height = 8,
    dpi = 320,
    device = ragg::agg_png
  )
  
  message("✔ Figura lista: ", ciudad_nombre)
}

# -----------------------------
# 6) Ejecutar
# -----------------------------
hacer_fig_ciudad("Cali")
hacer_fig_ciudad("Medellín")

message("✅ Listo. Figuras guardadas en: ", out_dir)

# ============================================================
# 7) FIGURA 2 — Distribución densidad + P75 y P90
# ============================================================
df_p_lines <- resumen_genero %>%
  dplyr::select(ciudad, genero_2, p75, p90) %>%
  pivot_longer(cols = c(p75, p90),
               names_to = "percentil",
               values_to = "minutos") %>%
  mutate(percentil = recode(percentil, p75 = "P75", p90 = "P90"))

p_dens <- ggplot(datos, aes(x = tiempo_total, color = genero_2)) +
  geom_density(linewidth = 1.15) +
  facet_wrap(~ ciudad, scales = "free_y") +
  geom_vline(
    data = df_p_lines,
    aes(xintercept = minutos, color = genero_2),
    linetype = "dashed",
    linewidth = 0.9
  ) +
  geom_text(
    data = df_p_lines,
    aes(x = minutos, y = 0, label = percentil, color = genero_2),
    vjust = 1.4,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = colores_genero) +
  coord_cartesian(xlim = c(0, 140)) +
  labs(
    x = "Tiempo total de movilidad (minutos)",
    y = "Densidad",
    title = "Distribución del tiempo total de movilidad por género",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_dir, "fig_2_distribucion_densidad_p75_p90.png"),
  plot = p_dens,
  width = 12,
  height = 6,
  dpi = 300
)

# ============================================================
# 8) FIGURA 3 — Boxplot por género y ciudad
# ============================================================
p_box <- ggplot(datos, aes(x = genero_2, y = tiempo_total, fill = genero_2)) +
  geom_boxplot(outlier.alpha = 0.25) +
  facet_wrap(~ ciudad) +
  scale_fill_manual(values = colores_genero) +
  labs(
    x = "Género",
    y = "Tiempo total de movilidad (minutos)",
    subtitle = "Boxplot comparativo por ciudad",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_dir, "fig_3_boxplot_tiempo_total_por_genero_ciudad.png"),
  plot = p_box,
  width = 12,
  height = 6,
  dpi = 300
)

# -----------------------------
# 9) Mensaje final
# -----------------------------
message(
  "Listo. Salidas guardadas en:\n", out_dir,
  "\n\nTablas:",
  "\n- tabla_resumen_general_ciudad.csv",
  "\n- tabla_resumen_por_genero_ciudad.csv",
  "\n\nFiguras:",
  "\n- fig_1_media_mediana_por_genero_ciudad.png",
  "\n- fig_2_distribucion_densidad_p75_p90.png",
  "\n- fig_3_boxplot_tiempo_total_por_genero_ciudad.png"
)
