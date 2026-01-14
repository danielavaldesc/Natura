# ============================================================
# SCRIPT R — Motivos de viaje
# ============================================================

# -----------------------------
# 0) Paquetes
# -----------------------------
pkgs <- c("tidyverse", "readxl", "forcats", "stringr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(readxl)
library(forcats)
library(stringr)

# -----------------------------
# 1) Paleta azul única (igual a la anterior)
# -----------------------------
colores_genero <- c(
  "Hombre" = "#1F3A5F",  # azul oscuro
  "Mujer"  = "#4A90C2"   # azul claro
)

# -----------------------------
# 2) Rutas
# -----------------------------
excel_path <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Outputs/cruces_ideam_genero_estrato_motivo_tiempo.xlsx"
sheet_name <- "motivo_x_genero"

out_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Indicador 1/Comparativo_Cali_Medellin/Motivos_viaje"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(excel_path))

# -----------------------------
# 3) Cargar y limpiar
# -----------------------------
df_raw <- read_excel(excel_path, sheet = sheet_name)

# Esperamos columnas: ciudad, p40, p23_agregado, n, pct
required_cols <- c("ciudad", "p40", "p23_agregado", "n", "pct")
missing <- setdiff(required_cols, names(df_raw))
if (length(missing) > 0) stop(paste("Faltan columnas:", paste(missing, collapse = ", ")))

df <- df_raw %>%
  mutate(
    ciudad = as.character(ciudad),
    genero_2 = as.character(p40),
    motivo = as.character(p23_agregado),
    n = as.numeric(n),
    # pct puede venir como número o como texto con coma decimal
    pct = case_when(
      is.numeric(pct) ~ pct,
      TRUE ~ as.numeric(str_replace(as.character(pct), ",", "."))
    )
  ) %>%
  dplyr::select(ciudad, genero_2, motivo, n, pct) %>%
  filter(!is.na(ciudad), !is.na(genero_2), !is.na(motivo), !is.na(pct)) %>%
  mutate(
    ciudad = factor(ciudad, levels = c("Cali", "Medellín")),
    genero_2 = factor(genero_2, levels = c("Hombre", "Mujer"))
  )

# Guardar tabla limpia
write_csv(df, file.path(out_dir, "tabla_motivos_x_genero_ciudad_limpia.csv"))

# -----------------------------
# 4) (Opcional pero útil) Ordenar motivos por peso promedio
#    para que las barras se lean bien
# -----------------------------
motivo_order <- df %>%
  group_by(motivo) %>%
  summarise(pct_prom = mean(pct, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(pct_prom)) %>%
  pull(motivo)

df <- df %>%
  mutate(motivo = factor(motivo, levels = motivo_order))

# ============================================================
# FIGURA A — Composición de motivos por género, facet por ciudad
# (Barras 100% apiladas por género)
# ============================================================
pA <- ggplot(df, aes(x = genero_2, y = pct, fill = motivo)) +
  geom_col(width = 0.7) +
  facet_wrap(~ ciudad) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Género",
    y = "Porcentaje de viajes",
    title = "Motivos de viaje por género y ciudad",
    subtitle = "Composición porcentual por motivo (p23_agregado)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_dir, "fig_A_motivos_composicion_100_apilado.png"),
  plot = pA,
  width = 13, height = 6.5, dpi = 300
)

# ============================================================
# FIGURA B — Trabajo vs No trabajo (para responder hipótesis)
# ============================================================
df_trabajo <- df %>%
  mutate(
    bloque = if_else(motivo == "Trabajo", "Trabajo remunerado", "Otros motivos (no trabajo)")
  ) %>%
  group_by(ciudad, genero_2, bloque) %>%
  summarise(pct = sum(pct, na.rm = TRUE), n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    bloque = factor(bloque, levels = c("Trabajo remunerado", "Otros motivos (no trabajo)"))
  )

pB <- ggplot(df_trabajo, aes(x = bloque, y = pct, fill = genero_2)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 0.1)),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    size = 3
  ) +
  facet_wrap(~ ciudad) +
  scale_fill_manual(values = colores_genero) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.05)) +
  labs(
    x = NULL,
    y = "Porcentaje de viajes",
    title = "Participación del trabajo remunerado en los motivos de viaje",
    subtitle = "Comparativo por género y ciudad",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_dir, "fig_B_trabajo_vs_no_trabajo_por_genero_ciudad.png"),
  plot = pB,
  width = 12, height = 6, dpi = 300
)

# ============================================================
# FIGURA C — Top motivos citados en el texto (lado a lado)
# Trabajo / Recreación-salud / Compras-trámites
# ============================================================
motivos_clave <- c(
  "Trabajo",
  "Recreación, salud y actividades personales",
  "Compras y trámites"
)

df_clave <- df %>%
  filter(as.character(motivo) %in% motivos_clave) %>%
  mutate(motivo = fct_relevel(motivo, motivos_clave))

pC <- ggplot(df_clave, aes(x = motivo, y = pct, fill = genero_2)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 0.1)),
    position = position_dodge(width = 0.7),
    vjust = -0.35,
    size = 3
  ) +
  facet_wrap(~ ciudad) +
  scale_fill_manual(values = colores_genero) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  labs(
    x = NULL,
    y = "Porcentaje de viajes",
    title = "Motivos de viaje seleccionados por género y ciudad",
    subtitle = "Trabajo, recreación/salud/actividades personales, y compras/trámites",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 1)
  )

ggsave(
  filename = file.path(out_dir, "fig_C_motivos_clave_por_genero_ciudad.png"),
  plot = pC,
  width = 13, height = 6, dpi = 300
)

# -----------------------------
# 5) Mensaje final
# -----------------------------
message(
  "Listo. Salidas guardadas en:\n", out_dir,
  "\n\nFiguras:",
  "\n- fig_A_motivos_composicion_100_apilado.png",
  "\n- fig_B_trabajo_vs_no_trabajo_por_genero_ciudad.png",
  "\n- fig_C_motivos_clave_por_genero_ciudad.png",
  "\n\nTabla:",
  "\n- tabla_motivos_x_genero_ciudad_limpia.csv"
)
