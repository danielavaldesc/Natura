# ============================================================
# SCRIPT R — Tiempo total de movilidad por modo de transporte
# ============================================================

# -----------------------------
# 0) Paquetes
# -----------------------------
pkgs <- c("tidyverse", "readxl", "scales", "ragg")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(readxl)
library(scales)
library(ragg)

# -----------------------------
# 1) Paleta
# -----------------------------
colores_genero <- c(
  "Hombre" = "#1F3A5F",  # azul oscuro
  "Mujer"  = "#4A90C2"   # azul medio
)

# -----------------------------
# 1.1) Tema global con letra MÁS GRANDE (XXL)
# -----------------------------
tema_xxl <- theme_minimal(base_size = 20) +
  theme(
    plot.title    = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 20),
    strip.text    = element_text(size = 22, face = "bold"),
    legend.text   = element_text(size = 18),
    legend.title  = element_text(size = 18),
    axis.title    = element_text(size = 20),
    axis.text     = element_text(size = 18),
    plot.margin   = margin(10, 15, 10, 15)
  )

# -----------------------------
# 2) Rutas
# -----------------------------
base_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input"

file_cali <- file.path(base_dir, "input_famd_cali_29102025.xlsx")
file_med  <- file.path(base_dir, "input_famd_med_29102025.xlsx")

out_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Indicador 1/Comparativo_Cali_Medellin/Modos_transporte"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(file_cali), file.exists(file_med))

# -----------------------------
# 3) Función para cargar y limpiar
# -----------------------------
load_city <- function(path, ciudad_nombre) {
  read_excel(path) %>%
    mutate(
      ciudad = ciudad_nombre,
      genero_2 = case_when(
        p40 == "Hombre" ~ "Hombre",
        p40 == "Mujer"  ~ "Mujer",
        TRUE ~ NA_character_
      ),
      medio = as.character(medio),
      tiempo_total = as.numeric(tiempo_total)
    ) %>%
    dplyr::select(ciudad, genero_2, medio, tiempo_total) %>%
    filter(
      !is.na(genero_2),
      !is.na(medio),
      !is.na(tiempo_total)
    )
}

df_cali <- load_city(file_cali, "Cali")
df_med  <- load_city(file_med,  "Medellín")

df <- bind_rows(df_cali, df_med) %>%
  mutate(
    ciudad = factor(ciudad, levels = c("Cali", "Medellín")),
    genero_2 = factor(genero_2, levels = c("Hombre", "Mujer"))
  )

# Guardar tabla base
write_csv(df, file.path(out_dir, "tabla_tiempo_x_modo_genero_ciudad.csv"))

# -----------------------------
# 4) Ordenar modos por mediana de tiempo
# -----------------------------
modo_order <- df %>%
  group_by(medio) %>%
  summarise(mediana = median(tiempo_total, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mediana)) %>%
  pull(medio)

df <- df %>%
  mutate(medio = factor(medio, levels = modo_order))

# ============================================================
# FIGURA — Tiempo total por modo, género y ciudad
# ============================================================
p_modos <- ggplot(
  df,
  aes(x = medio, y = tiempo_total, fill = genero_2)
) +
  geom_boxplot(
    outlier.alpha = 0.3,
    width = 0.65
  ) +
  facet_wrap(~ ciudad, scales = "free_x") +
  scale_fill_manual(values = colores_genero) +
  labs(
    x = "Modo de transporte",
    y = "Tiempo total de movilidad (minutos)",
    title = "Tiempo total de movilidad por modo de transporte, género y ciudad",
    subtitle = "Distribución, mediana y dispersión del tiempo total",
    fill = NULL
  ) +
  tema_xxl +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 1, size = 18),
    axis.ticks.length = unit(3, "mm")
  )

# -----------------------------
# 5) Guardar figura (texto más nítido con ragg)
# -----------------------------
ggsave(
  filename = file.path(out_dir, "fig_7_tiempo_por_modo_genero_ciudad.png"),
  plot = p_modos,
  width = 18,
  height = 10,
  dpi = 320,
  device = ragg::agg_png
)

message(
  "Listo.\n",
  "Figura guardada en:\n", out_dir,
  "\n\nArchivo:\n- fig_7_tiempo_por_modo_genero_ciudad.png",
  "\n\nTabla base:\n- tabla_tiempo_x_modo_genero_ciudad.csv"
)
