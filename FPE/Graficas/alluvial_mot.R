# ============================================================
# SCRIPT R — Conexión Motivo -> Modo (Alluvial)
# Estética cuidada + paletas elegantes
# CON LABELS DE MOTIVOS Y MODOS
# ============================================================

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

# -----------------------------
# 2) Cargar + limpiar
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
      motivo = str_squish(as.character(p23_agregado)),
      medio  = str_squish(as.character(medio))
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
    genero_2 = factor(genero_2, levels = c("Hombre", "Mujer"))
  )

# -----------------------------
# 3) Agregación
# -----------------------------
df_flow2 <- df %>%
  count(ciudad, genero_2, motivo, medio, name = "n") %>%
  group_by(ciudad, genero_2) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    motivo = factor(motivo),
    medio  = factor(medio)
  )

# -----------------------------
# 4) LABELS BONITOS
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

label_motivo_full <- setNames(as.character(levels(df_flow2$motivo)),
                              levels(df_flow2$motivo))
label_motivo_full[names(label_motivo)] <- label_motivo

label_medio_full <- setNames(as.character(levels(df_flow2$medio)),
                             levels(df_flow2$medio))
label_medio_full[names(label_medio)] <- label_medio

# -----------------------------
# 5) PALETAS (MISMAS QUE TE GUSTARON)
# -----------------------------
pal_global <- c(
  "#5B5F97", "#6C7AA1", "#8FA4C8",
  "#A7C0D9", "#8CBEB2", "#5C9EAD", "#326273"
)

pal_cali_hombre <- c(
  "#5B5F97", "#6C7AA1", "#8FA4C8",
  "#A7C0D9", "#8CBEB2", "#5C9EAD", "#326273"
)

pal_cali_mujer <- c(
  "#5B5F97", "#6C7AA1", "#8FA4C8",
  "#A7C0D9", "#8CBEB2", "#5C9EAD", "#326273"
)

pal_med_hombre <- c(
  "#5B5F97", "#6C7AA1", "#8FA4C8",
  "#A7C0D9", "#8CBEB2", "#5C9EAD", "#326273"
)

pal_med_mujer <- c(
  "#5B5F97", "#6C7AA1", "#8FA4C8",
  "#A7C0D9", "#8CBEB2", "#5C9EAD", "#326273"
)

fix_pal <- function(pal) {
  pal <- rep(pal, length.out = length(levels(df_flow2$motivo)))
  names(pal) <- levels(df_flow2$motivo)
  pal
}

pal_by_group <- list(
  "global"            = fix_pal(pal_global),
  "Cali|Hombre"       = fix_pal(pal_cali_hombre),
  "Cali|Mujer"        = fix_pal(pal_cali_mujer),
  "Medellín|Hombre"   = fix_pal(pal_med_hombre),
  "Medellín|Mujer"    = fix_pal(pal_med_mujer)
)

# -----------------------------
# 6) THEME LIMPIO
# -----------------------------
theme_alluvial_clean <- theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "grey30"),
    legend.position = "none"
  )

# ============================================================
# 7) FIGURA GENERAL
# ============================================================
p_alluvial <- ggplot(
  df_flow2,
  aes(axis1 = motivo, axis2 = medio, y = pct)
) +
  geom_alluvium(aes(fill = motivo), alpha = 0.9, width = 0.32) +
  geom_stratum(width = 0.32, fill = "grey96", color = "grey70") +
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
  scale_fill_manual(values = pal_by_group[["global"]]) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_grid(ciudad ~ genero_2) +
  labs(
    title = "Conexión entre motivos de viaje y modos de transporte",
    subtitle = "Proporción de viajes por ciudad y género",
    y = "Porcentaje de viajes"
  ) +
  theme_alluvial_clean

ggsave(
  file.path(out_dir, "fig_alluvial_global.png"),
  p_alluvial, width = 18, height = 10, dpi = 300
)

# ============================================================
# 8) FUNCIÓN GRÁFICOS INDIVIDUALES
# ============================================================
plot_single <- function(data, ciudad_sel, genero_sel) {
  
  pal_use <- pal_by_group[[paste(ciudad_sel, genero_sel, sep = "|")]]
  
  p <- ggplot(
    data %>% filter(ciudad == ciudad_sel, genero_2 == genero_sel),
    aes(axis1 = motivo, axis2 = medio, y = pct)
  ) +
    geom_alluvium(aes(fill = motivo), alpha = 0.9, width = 0.32) +
    geom_stratum(width = 0.32, fill = "grey96", color = "grey70") +
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
    scale_fill_manual(values = pal_use) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = paste("Motivos y modos de viaje –", ciudad_sel),
      subtitle = paste("Género:", genero_sel),
      y = "Porcentaje de viajes"
    ) +
    theme_alluvial_clean
  
  ggsave(
    file.path(
      out_dir,
      paste0("fig_alluvial_", tolower(ciudad_sel), "_", tolower(genero_sel), ".png")
    ),
    p, width = 9, height = 8, dpi = 300
  )
}

# ============================================================
# 9) GENERAR GRÁFICAS
# ============================================================
for (c in levels(df_flow2$ciudad)) {
  for (g in levels(df_flow2$genero_2)) {
    plot_single(df_flow2, c, g)
  }
}

message("Listo 😌 Misma estética, mismos colores, labels de motivos recuperados.")
