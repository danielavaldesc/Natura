# ============================================================
# SCRIPT R — Conexión Motivo -> Modo (Alluvial)
# Global + individuales (ciudad × género)
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
# 2) Función cargar + limpiar
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
    select(ciudad, genero_2, motivo, medio) %>%
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
# 3) Agregación
# -----------------------------
df_flow <- df %>%
  count(ciudad, genero_2, motivo, medio, name = "n")

write_csv(
  df_flow,
  file.path(out_dir, "tabla_flujos_motivo_medio_genero_ciudad.csv")
)

# -----------------------------
# 4) Reducir complejidad (TOP + otros)
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
# 5) Orden de factores
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
# 6) Paleta (motivos)
# -----------------------------
pal_motivos <- c(
  "#807DBA", "#6A51A3", "#54278F",
  "#4A90C2", "#5A6ACF", "#6E67D8", "#8A7FF0"
)
pal_motivos <- rep(pal_motivos, length.out = length(levels(df_flow2$motivo2)))
names(pal_motivos) <- levels(df_flow2$motivo2)

# -----------------------------
# 7) Etiquetas visuales
# -----------------------------
label_motivo <- c(
  "Recreación y actividades personales" = "Recreación y\nact. personales"
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

# ============================================================
# 8) FIGURA GENERAL (facetada)
# ============================================================
p_alluvial <- ggplot(
  df_flow2,
  aes(axis1 = motivo2, axis2 = medio2, y = pct)
) +
  geom_alluvium(aes(fill = motivo2), alpha = 0.85, width = 0.35) +
  geom_stratum(width = 0.35, color = "grey70", fill = "grey95") +
  geom_text(
    stat = "stratum",
    aes(
      label = ifelse(
        after_stat(x) == 1,
        label_motivo_full[as.character(after_stat(stratum))],
        label_medio_full[as.character(after_stat(stratum))]
      )
    ),
    size = 4,
    lineheight = 0.95
  ) +
  scale_fill_manual(values = pal_motivos) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  facet_grid(ciudad ~ genero_2) +
  labs(
    title = "Conexión entre motivos de viaje y modos de transporte",
    subtitle = "Proporción de viajes por ciudad y género",
    x = NULL,
    y = "Porcentaje de viajes",
    fill = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(out_dir, "fig_alluvial_global_ciudad_genero.png"),
  p_alluvial, width = 18, height = 10.5, dpi = 300
)

# ============================================================
# 9) FUNCIÓN: gráfico individual
# ============================================================
plot_alluvial_single <- function(data, ciudad_sel, genero_sel) {
  
  p <- ggplot(
    data %>% filter(ciudad == ciudad_sel, genero_2 == genero_sel),
    aes(axis1 = motivo2, axis2 = medio2, y = pct)
  ) +
    geom_alluvium(aes(fill = motivo2), alpha = 0.85, width = 0.35) +
    geom_stratum(width = 0.35, color = "grey70", fill = "grey95") +
    geom_text(
      stat = "stratum",
      aes(
        label = ifelse(
          after_stat(x) == 1,
          label_motivo_full[as.character(after_stat(stratum))],
          label_medio_full[as.character(after_stat(stratum))]
        )
      ),
      size = 4,
      lineheight = 0.95
    ) +
    scale_fill_manual(values = pal_motivos) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = paste("Motivos y modos de viaje –", ciudad_sel),
      subtitle = paste("Género:", genero_sel),
      x = NULL,
      y = "Porcentaje de viajes",
      fill = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 15),
      axis.text.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  fname <- paste0(
    "fig_alluvial_",
    tolower(ciudad_sel), "_",
    tolower(genero_sel), ".png"
  )
  
  ggsave(
    file.path(out_dir, fname),
    p, width = 9, height = 8, dpi = 300
  )
}

# ============================================================
# 10) GENERAR LAS 4 GRÁFICAS
# ============================================================
for (c in levels(df_flow2$ciudad)) {
  for (g in levels(df_flow2$genero_2)) {
    plot_alluvial_single(df_flow2, c, g)
  }
}

message("Figuras globales e individuales guardadas en:\n", out_dir)


