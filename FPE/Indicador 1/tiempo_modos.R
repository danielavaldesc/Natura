# ============================================================
# SCRIPT R — Tiempo total de movilidad por modo de transporte
# ============================================================

# -----------------------------
# 0) Paquetes
# -----------------------------
pkgs <- c("tidyverse", "readxl", "scales")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(readxl)
library(scales)

# -----------------------------
# 1) Paleta (MISMA lógica que antes)
# -----------------------------
colores_genero <- c(
  "Hombre" = "#1F3A5F",  # azul oscuro
  "Mujer"  = "#4A90C2"   # azul medio
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
# 4) (Opcional pero recomendado)
# Ordenar modos por mediana de tiempo
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
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_dir, "fig_7_tiempo_por_modo_genero_ciudad.png"),
  plot = p_modos,
  width = 14,
  height = 7,
  dpi = 300
)

message(
  "Listo.\n",
  "Figura guardada en:\n", out_dir,
  "\n\nArchivo:\n- fig_7_tiempo_por_modo_genero_ciudad.png",
  "\n\nTabla base:\n- tabla_tiempo_x_modo_genero_ciudad.csv"
)
