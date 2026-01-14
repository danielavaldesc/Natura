# ============================================================
# Indicador 1: Carga total de tiempo de movilidad
# ============================================================

# -----------------------------
# 0) Paquetes
# -----------------------------
pkgs <- c("tidyverse", "readxl", "forcats", "tidyr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(readxl)
library(forcats)
library(tidyr)

# -----------------------------
# 1) Paleta azul única (todas las gráficas)
# -----------------------------
colores_genero <- c(
  "Hombre" = "#1F3A5F",  # azul oscuro
  "Mujer"  = "#4A90C2"   # azul claro
)

# -----------------------------
# 2) Rutas (AJUSTA SOLO base_dir si lo necesitas)
# -----------------------------
base_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura"

input_cali <- file.path(base_dir, "201025_Results_Cali", "output", "input_famd_cali_29102025.xlsx")
input_med  <- file.path(base_dir, "271025_Results_Med",  "output", "input_famd_med_29102025.xlsx")

out_dir <- file.path(base_dir, "FPE", "Indicador 1", "Comparativo_Cali_Medellin")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(input_cali))
stopifnot(file.exists(input_med))

# -----------------------------
# 3) Función: cargar y preparar datos
# -----------------------------
prep_dataset <- function(path_xlsx, ciudad_label) {
  df <- read_excel(path_xlsx)
  
  required_cols <- c("tiempo_total", "p40")
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(paste0("Faltan columnas en ", ciudad_label, ": ", paste(missing, collapse = ", ")))
  }
  
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

# -----------------------------
# 4) Cargar datos
# -----------------------------
cali <- prep_dataset(input_cali, "Cali")
med  <- prep_dataset(input_med,  "Medellín")

datos <- bind_rows(cali, med) %>%
  mutate(
    genero_2 = fct_relevel(genero_2, "Hombre", "Mujer"),
    ciudad   = factor(ciudad, levels = c("Cali", "Medellín"))
  )

# -----------------------------
# 5) Tablas resumen
# -----------------------------
resumen_general <- datos %>%
  group_by(ciudad) %>%
  summarise(
    n = n(),
    promedio = mean(tiempo_total, na.rm = TRUE),
    mediana  = median(tiempo_total, na.rm = TRUE),
    p25      = quantile(tiempo_total, 0.25, na.rm = TRUE),
    p75      = quantile(tiempo_total, 0.75, na.rm = TRUE),
    p90      = quantile(tiempo_total, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

resumen_genero <- datos %>%
  group_by(ciudad, genero_2) %>%
  summarise(
    n = n(),
    promedio = mean(tiempo_total, na.rm = TRUE),
    mediana  = median(tiempo_total, na.rm = TRUE),
    p25      = quantile(tiempo_total, 0.25, na.rm = TRUE),
    p75      = quantile(tiempo_total, 0.75, na.rm = TRUE),
    p90      = quantile(tiempo_total, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(resumen_general, file.path(out_dir, "tabla_resumen_general_ciudad.csv"))
write_csv(resumen_genero,  file.path(out_dir, "tabla_resumen_por_genero_ciudad.csv"))

# ============================================================
# 6) FIGURA 1 — Media y Mediana por género y ciudad
# (Mantiene paleta azul y estética limpia)
# ============================================================
df_mm <- resumen_genero %>%
  dplyr::select(ciudad, genero_2, promedio, mediana) %>%
  pivot_longer(cols = c(promedio, mediana),
               names_to = "estadistico",
               values_to = "minutos") %>%
  mutate(
    estadistico = case_when(
      estadistico == "promedio" ~ "Promedio",
      estadistico == "mediana"  ~ "Mediana",
      TRUE ~ estadistico
    )
  )

# Dos tonos azules para los estadísticos (promedio/mediana) coherentes con el esquema
colores_estad <- c(
  "Promedio" = "#1F3A5F",  # azul oscuro
  "Mediana"  = "#4A90C2"   # azul claro
)

p_mm <- ggplot(df_mm, aes(x = genero_2, y = minutos, fill = estadistico)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(
    aes(label = round(minutos, 1)),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    size = 3
  ) +
  facet_wrap(~ ciudad) +
  scale_fill_manual(values = colores_estad) +
  labs(
    x = NULL,
    y = "Minutos",
    title = "Tiempo total de movilidad por género",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_dir, "fig_1_media_mediana_por_genero_ciudad.png"),
  plot = p_mm,
  width = 12, height = 6, dpi = 300
)

# ============================================================
# 7) FIGURA 2 — Distribución (DENSIDAD) + P75 y P90 (COLAS)
# (Todo en azul por género)
# ============================================================
df_p_lines <- resumen_genero %>%
  dplyr::select(ciudad, genero_2, p75, p90) %>%
  pivot_longer(cols = c(p75, p90),
               names_to = "percentil",
               values_to = "minutos") %>%
  mutate(
    percentil = case_when(
      percentil == "p75" ~ "P75",
      percentil == "p90" ~ "P90",
      TRUE ~ percentil
    )
  )

p_dens <- ggplot(datos, aes(x = tiempo_total, color = genero_2)) +
  geom_density(linewidth = 1.15, adjust = 1.05) +
  facet_wrap(~ ciudad, scales = "free_y") +
  geom_vline(
    data = df_p_lines,
    aes(xintercept = minutos, color = genero_2),
    linetype = "dashed",
    linewidth = 0.9,
    alpha = 0.95
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
  width = 12, height = 6, dpi = 300
)

# ============================================================
# 8) FIGURA 3 — Boxplot por género y ciudad (paleta azul)
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
  width = 12, height = 6, dpi = 300
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

