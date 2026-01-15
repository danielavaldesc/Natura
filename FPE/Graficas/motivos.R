# ============================================================
# SCRIPT R — Motivos de viaje 
# ============================================================

# -----------------------------
# 0) Paquetes
# -----------------------------
pkgs <- c("tidyverse", "readxl", "forcats", "stringr", "scales")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(readxl)
library(forcats)
library(stringr)
library(scales)

# -----------------------------
# 1) Rutas
# -----------------------------
excel_path <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\FPE\\Outputs\\cruces_ideam_genero_estrato_motivo_tiempo.xlsx"

out_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Graficas/motivos"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(excel_path))

# Hojas
sheet_A <- "motivo_x_genero"
sheet_B <- "motivo_x_genero_x_estrato"
sheet_C <- "tiempo_x_motivo_x_genero"

# Validar hojas
sheets_disponibles <- readxl::excel_sheets(excel_path)
for (sh in c(sheet_A, sheet_B, sheet_C)) {
  if (!(sh %in% sheets_disponibles)) {
    message("⚠️ No encontré la hoja: '", sh, "'.")
    message("Hojas disponibles:\n- ", paste(sheets_disponibles, collapse = "\n- "))
    stop("Cambia el nombre en sheet_A / sheet_B / sheet_C.")
  }
}

# -----------------------------
# 2) Paletas
# -----------------------------
colores_genero <- c(
  "Hombre" = "#1F3A5F",
  "Mujer"  = "#4A90C2"
)

paleta_base_motivos <- c(
  "#1F3A5F", "#2E5984", "#4A90C2", "#7FB3D5", "#C6DBEF",
  "#5B4B8A", "#7E6BB3", "#9E9AC8", "#BCBDDC", "#DADAEB",
  "#807DBA", "#6A51A3", "#54278F"
)

# -----------------------------
# 3) Función para hojas de MOTIVOS (%)
# -----------------------------
cargar_motivos <- function(sheet, requiere_estrato = FALSE) {
  df_raw <- read_excel(excel_path, sheet = sheet)
  
  required_cols <- c("ciudad", "p40", "p23_agregado", "n", "pct")
  if (requiere_estrato) required_cols <- c(required_cols, "p9_estrato3")
  
  missing <- setdiff(required_cols, names(df_raw))
  if (length(missing) > 0) {
    stop(paste0("En la hoja '", sheet, "' faltan columnas: ", paste(missing, collapse = ", ")))
  }
  
  df <- df_raw %>%
    mutate(
      ciudad   = str_squish(as.character(ciudad)),
      genero_2 = str_squish(as.character(p40)),
      motivo   = str_squish(as.character(p23_agregado)),
      n        = suppressWarnings(as.numeric(n)),
      pct      = case_when(
        is.numeric(pct) ~ pct,
        TRUE ~ suppressWarnings(as.numeric(str_replace(as.character(pct), ",", ".")))
      )
    )
  
  if (requiere_estrato) {
    df <- df %>%
      mutate(estrato = str_squish(as.character(p9_estrato3)))
  }
  
  df %>%
    select(any_of(c("ciudad", "genero_2", "estrato", "motivo", "n", "pct"))) %>%
    filter(!is.na(ciudad), !is.na(genero_2), !is.na(motivo), !is.na(pct)) %>%
    mutate(
      ciudad   = factor(ciudad, levels = c("Cali", "Medellín")),
      genero_2 = factor(genero_2, levels = c("Hombre", "Mujer"))
    )
}

# ============================================================
# FIGURA 1 — motivo_x_genero (100% por género, por ciudad)
# ============================================================
dfA <- cargar_motivos(sheet_A, requiere_estrato = FALSE)

motivo_order_A <- dfA %>%
  group_by(motivo) %>%
  summarise(pct_prom = mean(pct, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(pct_prom)) %>%
  pull(motivo)

dfA <- dfA %>% mutate(motivo = factor(motivo, levels = motivo_order_A))

n_motivos_A <- nlevels(dfA$motivo)
paleta_motivos_A <- paleta_base_motivos[seq_len(min(n_motivos_A, length(paleta_base_motivos)))]
names(paleta_motivos_A) <- levels(dfA$motivo)[seq_along(paleta_motivos_A)]

p1 <- ggplot(dfA, aes(x = genero_2, y = pct, fill = motivo)) +
  geom_col(width = 0.7, color = "grey90", linewidth = 0.15) +
  facet_wrap(~ ciudad) +  # ✅ por ciudad
  scale_fill_manual(values = paleta_motivos_A) +
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
  filename = file.path(out_dir, "fig_1_motivos_100_genero_ciudad.png"),
  plot = p1,
  width = 13, height = 6.5, dpi = 300
)

# ============================================================
# FIGURA 2 — motivo_x_genero_x_estrato (100% por estrato)
#   ✅ Diferenciado por ciudad (columnas) y género (filas)
# ============================================================
dfB <- cargar_motivos(sheet_B, requiere_estrato = TRUE)

orden_estrato <- c("Rural", "1", "2", "3", "4", "5", "6")
dfB <- dfB %>%
  mutate(
    estrato = factor(estrato, levels = unique(c(orden_estrato, sort(unique(estrato)))))
  ) %>%
  filter(!is.na(estrato))

motivo_order_B <- dfB %>%
  group_by(motivo) %>%
  summarise(pct_prom = mean(pct, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(pct_prom)) %>%
  pull(motivo)

dfB <- dfB %>% mutate(motivo = factor(motivo, levels = motivo_order_B))

n_motivos_B <- nlevels(dfB$motivo)
paleta_motivos_B <- paleta_base_motivos[seq_len(min(n_motivos_B, length(paleta_base_motivos)))]
names(paleta_motivos_B) <- levels(dfB$motivo)[seq_along(paleta_motivos_B)]

p2 <- ggplot(dfB, aes(x = estrato, y = pct, fill = motivo)) +
  geom_col(width = 0.72, color = "grey92", linewidth = 0.15) +
  facet_grid(genero_2 ~ ciudad) +  # ✅ ciudad separada
  scale_fill_manual(values = paleta_motivos_B) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Estrato",
    y = "Porcentaje de viajes",
    title = "Distribución porcentual de motivos de viaje por estrato",
    subtitle = "Barras 100% apiladas (por género y ciudad)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_dir, "fig_2_motivos_100_por_estrato_genero_ciudad.png"),
  plot = p2,
  width = 14, height = 7.2, dpi = 300
)

# ============================================================
# FIGURA 3 — tiempo_x_motivo_x_genero
#   ✅ Tiempo por motivo y género, diferenciado por ciudad
#   Usamos MEDIANA como punto y P25–P75 como barra (tipo "intervalo")
# ============================================================

dfC_raw <- read_excel(excel_path, sheet = sheet_C)

required_C <- c("ciudad", "p23_agregado", "p40", "n", "mediana", "p25", "p75")
missing_C <- setdiff(required_C, names(dfC_raw))
if (length(missing_C) > 0) {
  stop(paste0("En la hoja '", sheet_C, "' faltan columnas: ", paste(missing_C, collapse = ", ")))
}

dfC <- dfC_raw %>%
  transmute(
    ciudad   = factor(str_squish(as.character(ciudad)), levels = c("Cali", "Medellín")),
    motivo   = str_squish(as.character(p23_agregado)),
    genero_2 = factor(str_squish(as.character(p40)), levels = c("Hombre", "Mujer")),
    n        = suppressWarnings(as.numeric(n)),
    mediana  = suppressWarnings(as.numeric(mediana)),
    p25      = suppressWarnings(as.numeric(p25)),
    p75      = suppressWarnings(as.numeric(p75))
  ) %>%
  filter(!is.na(ciudad), !is.na(motivo), !is.na(genero_2), !is.na(mediana), !is.na(p25), !is.na(p75))

# Ordenar motivos por mediana promedio (para lectura)
motivo_order_C <- dfC %>%
  group_by(motivo) %>%
  summarise(mediana_prom = mean(mediana, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mediana_prom)) %>%
  pull(motivo)

dfC <- dfC %>% mutate(motivo = factor(motivo, levels = motivo_order_C))

p3 <- ggplot(dfC, aes(x = motivo, y = mediana, color = genero_2)) +
  geom_linerange(aes(ymin = p25, ymax = p75),
                 position = position_dodge(width = 0.6),
                 linewidth = 1) +
  geom_point(position = position_dodge(width = 0.6), size = 2.2) +
  facet_wrap(~ ciudad) +  # ✅ por ciudad
  scale_color_manual(values = colores_genero) +
  labs(
    x = NULL,
    y = "Tiempo de viaje (minutos)",
    title = "Tiempo de viaje por motivo y género (mediana y rango intercuartílico)",
    subtitle = "Punto = mediana; línea = P25–P75 (por ciudad)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

ggsave(
  filename = file.path(out_dir, "fig_3_tiempo_motivo_genero_ciudad_mediana_IQR.png"),
  plot = p3,
  width = 14, height = 6.5, dpi = 300
)

# -----------------------------
# 7) Mensaje final
# -----------------------------
message(
  "Listo ✅ Guardé 3 gráficas en:\n", out_dir,
  "\n\n1) fig_1_motivos_100_genero_ciudad.png",
  "\n2) fig_2_motivos_100_por_estrato_genero_ciudad.png",
  "\n3) fig_3_tiempo_motivo_genero_ciudad_mediana_IQR.png"
)
