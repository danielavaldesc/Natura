# ============================================================
# SCRIPT R — Conexión Motivo -> Modo (Alluvial)
# ============================================================

# -----------------------------
# 0) Paquetes
# -----------------------------
pkgs <- c("tidyverse", "readxl", "stringr", "scales", "ggalluvial")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(readxl)
library(stringr)
library(scales)
library(ggalluvial)

# -----------------------------
# 1) Rutas
# -----------------------------
base_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input"
file_cali <- file.path(base_dir, "input_famd_cali_29102025.xlsx")
file_med  <- file.path(base_dir, "input_famd_med_29102025.xlsx")

out_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Graficas/motivos"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(file_cali), file.exists(file_med))

# -----------------------------
# 2) Función para cargar y limpiar
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
      motivo = as.character(p23_agregado),
      medio  = as.character(medio)
    ) %>%
    dplyr::select(ciudad, genero_2, motivo, medio) %>%
    filter(!is.na(genero_2), !is.na(motivo), !is.na(medio))
}

df <- bind_rows(
  load_city(file_cali, "Cali"),
  load_city(file_med,  "Medellín")
) %>%
  mutate(
    ciudad   = factor(ciudad, levels = c("Cali", "Medellín")),
    genero_2 = factor(genero_2, levels = c("Hombre", "Mujer")),
    motivo   = str_squish(motivo),
    medio    = str_squish(medio)
  )

# -----------------------------
# 3) Agregación (conteos)
# -----------------------------
df_flow <- df %>%
  count(ciudad, genero_2, motivo, medio, name = "n")

write_csv(df_flow, file.path(out_dir, "tabla_flujos_motivo_medio_genero_ciudad.csv"))

# -----------------------------
# 4) Reducir complejidad visual (TOP + "Otros")
# -----------------------------
top_motivos <- df_flow %>%
  group_by(motivo) %>%
  summarise(N = sum(n), .groups = "drop") %>%
  arrange(desc(N)) %>%
  slice_head(n = 6) %>%
  pull(motivo)

top_medios <- df_flow %>%
  group_by(medio) %>%
  summarise(N = sum(n), .groups = "drop") %>%
  arrange(desc(N)) %>%
  slice_head(n = 6) %>%
  pull(medio)

df_flow2 <- df_flow %>%
  mutate(
    motivo2 = if_else(motivo %in% top_motivos, motivo, "Otros motivos"),
    medio2  = if_else(medio  %in% top_medios,  medio,  "Otros modos")
  ) %>%
  group_by(ciudad, genero_2, motivo2, medio2) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  group_by(ciudad, genero_2) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# -----------------------------
# 5) Ordenar niveles
# -----------------------------
motivo_order <- df_flow2 %>%
  group_by(motivo2) %>%
  summarise(p = sum(pct), .groups = "drop") %>%
  arrange(desc(p)) %>%
  pull(motivo2)

medio_order <- df_flow2 %>%
  group_by(medio2) %>%
  summarise(p = sum(pct), .groups = "drop") %>%
  arrange(desc(p)) %>%
  pull(medio2)

df_flow2 <- df_flow2 %>%
  mutate(
    motivo2 = factor(motivo2, levels = motivo_order),
    medio2  = factor(medio2,  levels = medio_order)
  )

# -----------------------------
# 6) Paleta sobria (azules + morados) — por MOTIVO (fill)
# -----------------------------
pal_motivos <- c(
  "#807DBA", "#6A51A3", "#54278F", "#4A90C2", "#5A6ACF", "#6E67D8", "#8A7FF0"
)
pal_motivos <- rep(pal_motivos, length.out = length(levels(df_flow2$motivo2)))
names(pal_motivos) <- levels(df_flow2$motivo2)

# -----------------------------
# 7) Etiquetas abreviadas (SOLO VISUALES)
# -----------------------------
label_motivo <- c(
  "Trabajo" = "Trabajo",
  "Recreación, salud y actividades personales" = "Recreación / salud",
  "Compras y trámites" = "Compras / trámites",
  "Estudio" = "Estudio",
  "Visitas sociales" = "Visitas",
  "Otros motivos" = "Otros motivos",
  "Otro" = "Otro"
)

label_medio <- c(
  "Transporte público"  = "Transp. público",
  "Transporte informal" = "Transp. informal",
  "Moto privada"        = "Moto privada",
  "Taxi / Plataforma"   = "Taxi / app",
  "Auto privado"        = "Auto privado",
  "Modo activo"         = "Modo activo",
  "Otros modos"         = "Otros modos"
)

label_motivo_full <- setNames(as.character(levels(df_flow2$motivo2)), levels(df_flow2$motivo2))
label_motivo_full[names(label_motivo)] <- label_motivo

label_medio_full <- setNames(as.character(levels(df_flow2$medio2)), levels(df_flow2$medio2))
label_medio_full[names(label_medio)] <- label_medio

# -----------------------------
# 8) FIGURA — Alluvial Motivo -> Modo
# (FIX: indexar por as.character(after_stat(stratum)))
# -----------------------------
p_alluvial <- ggplot(
  df_flow2,
  aes(axis1 = motivo2, axis2 = medio2, y = pct)
) +
  geom_alluvium(aes(fill = motivo2), alpha = 0.85, width = 0.30) +
  geom_stratum(width = 0.30, color = "grey70", fill = "grey95") +
  geom_text(
    stat = "stratum",
    aes(
      label = ifelse(
        after_stat(x) == 1,
        label_motivo_full[as.character(after_stat(stratum))],
        label_medio_full[as.character(after_stat(stratum))]
      )
    ),
    size = 2.4,
    lineheight = 0.9,
    hjust = 0.5
  ) +
  scale_fill_manual(values = pal_motivos) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  facet_grid(ciudad ~ genero_2) +
  labs(
    title = "Conexión entre motivos de viaje y modos de transporte",
    subtitle = "Diagrama aluvial (proporción de viajes) por ciudad y género",
    x = NULL,
    y = "Porcentaje de viajes",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(out_dir, "fig_motivo_a_modo_alluvial_ciudad_genero.png"),
  plot = p_alluvial,
  width = 16,
  height = 9,
  dpi = 300
)

message(
  "Listo. Salidas guardadas en:\n", out_dir,
  "\n\nFigura:",
  "\n- fig_motivo_a_modo_alluvial_ciudad_genero.png",
  "\n\nTabla:",
  "\n- tabla_flujos_motivo_medio_genero_ciudad.csv"
)
