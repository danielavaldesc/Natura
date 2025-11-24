#############################################################################
## Figura 5: Georreferenciación de elección modal CALI x estrato x motivo  ##
#############################################################################

library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggpubr)
library(plyr)
library(rlang)
library(knitr)
library(haven)
library(foreign)
library(stringi)
library(labelled)
library(tidyr)
library(treemapify)
library(viridis)
library(kableExtra)
library(sf)
library(RColorBrewer)
library(memisc)
library(assertthat)
library(sqldf)
library(magrittr)
library(scatterpie)
library(maps)
library(ggnewscale)
library(grid)
library(forcats)   # <- para fct_collapse

# -------------------------------------------------------------------
# 1) Datos
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
dataset <- read_excel("input_famd_cali_29102025.xlsx")

dataset$id      <- as.character(dataset$id)
dataset$medio   <- as.character(dataset$medio)
dataset$Comuna  <- as.integer(gsub("\\D", "", as.character(dataset$p19comuna)))
data <- dataset

# -------------------------------------------------------------------
# Estrato predominante por comuna (CATEGÓRICO: Alto/Medio/Bajo)
# -------------------------------------------------------------------
nombre_estrato <- if ("p9_estrato3" %in% names(data)) "p9_estrato3" else
  if ("p9_estratro3" %in% names(data)) "p9_estratro3" else NA
if (is.na(nombre_estrato)) stop("No se encontró la columna de estrato (p9_estrato3 / p9_estratro3) en la base.")

tmp <- data.frame(
  Comuna = data$Comuna,
  estrato_cat = trimws(tolower(as.character(data[[nombre_estrato]]))),
  stringsAsFactors = FALSE
)
tmp$estrato_cat[tmp$estrato_cat %in% c("alto","alta")]   <- "Alto"
tmp$estrato_cat[tmp$estrato_cat %in% c("medio","media")] <- "Medio"
tmp$estrato_cat[tmp$estrato_cat %in% c("bajo","baja")]   <- "Bajo"
tmp <- tmp[!is.na(tmp$estrato_cat) & tmp$estrato_cat != "", ]

niveles_em <- c("Bajo","Medio","Alto")
tmp$estrato_cat <- factor(tmp$estrato_cat, levels = niveles_em, ordered = TRUE)

estratos_comuna <- tmp %>%
  dplyr::group_by(Comuna, estrato_cat) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop_last") %>%
  dplyr::arrange(dplyr::desc(n), estrato_cat) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(Comuna, categoria = as.character(estrato_cat))

# -------------------------------------------------------------------
# 2) Zonas a partir de Comuna (para ubicar PIES; no rotulamos)
# -------------------------------------------------------------------
data$zona <- NA_character_
for (k in 1:nrow(data)) {
  if (data$Comuna[k] %in% c(1, 2, 3, 9))                data$zona[k] <- "Noroccidente"
  if (data$Comuna[k] %in% c(4, 5, 6, 7, 8))             data$zona[k] <- "Nororiente"
  if (data$Comuna[k] %in% c(11,12,13,14,15,16,21))      data$zona[k] <- "Oriente-aguablanca"
  if (data$Comuna[k] %in% c(10,17,18,19,20,22))         data$zona[k] <- "Sur"
}
data <- data[!is.na(data$zona), ]
data$zona <- factor(data$zona,
                    levels = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur"))

# Coordenadas (se mantienen EXACTAS)
coords_zona <- data.frame(
  zona = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur"),
  long = c(1060000-300, 1065000-200, 1065000-600, 1059.28*1000),
  lat  = c(875000-1050, 875000+600, 870.5*1000, 866.4*1000),
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------------
# 3) Unificación propósito (p23_agr5) y definición de motivo
# -------------------------------------------------------------------
if (!"p23_agregado" %in% names(data)) {
  stop("No se encontró la columna 'p23_agregado' en la base.")
}

data <- data %>%
  mutate(
    p23_agregado = trimws(as.character(p23_agregado)),
    p23_agr5 = fct_collapse(
      p23_agregado,
      "Trabajo"          = c("Trabajo"),
      "Compras/Trámites" = c("Compras y trámites","Compras y tr\u00e1mites"),
      "Tiempo personal"  = c("Recreación, salud y actividades personales",
                             "Recreaci\u00f3n, salud y actividades personales",
                             "Visitas sociales"),
      "Estudio"          = "Estudio",
      "Cuidado"          = c(
        "Cuidado y familia (centro educativo, niños/as o jóvenes)",
        "Cuidado y familia (otro lugar, niños/as o jóvenes)",
        "Cuidado y familia (persona con discapacidad)",
        "Cuidado y familia (persona enferma)",
        "Cuidado y familia (recreación, niños)",
        "Cuidado y familia (salud, niños)",
        "Cuidado y familia (recreaci\u00f3n, ni\u00f1as/os)",
        "Cuidado y familia (salud, ni\u00f1as/os)"
      ),
      "Otros"            = "Otro"
    ) %>% fct_drop()
  ) %>%
  filter(!is.na(p23_agr5), p23_agr5 != "Otros") %>%
  mutate(p23_agr5 = factor(as.character(p23_agr5)))

# usamos p23_agr5 como motivo (orden fijo para facets)
data$motivo <- factor(
  as.character(data$p23_agr5),
  levels = c("Trabajo","Estudio","Compras/Trámites","Tiempo personal","Cuidado")
)

# -------------------------------------------------------------------
# 4) Shape + unión con estrato
# -------------------------------------------------------------------
shape <- st_read("mc_comunas.shp", quiet = TRUE)
columna_comuna_shape <- names(shape)[grepl("comuna", names(shape), ignore.case = TRUE)][1]
if (is.na(columna_comuna_shape)) stop("No se detectó columna con 'comuna' en el shapefile.")
shape$Comuna <- as.integer(gsub("\\D","", as.character(shape[[columna_comuna_shape]])))
shape <- dplyr::left_join(shape, estratos_comuna, by = "Comuna")

# -------------------------------------------------------------------
# 5) Conteos por motivo-zona-medio (wide para scatterpie)
# -------------------------------------------------------------------
df_counts <- data %>%
  dplyr::group_by(motivo, zona, medio) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = medio, values_from = n, values_fill = 0) %>%
  dplyr::left_join(coords_zona, by = "zona")

cols_pie <- setdiff(names(df_counts), c("motivo","zona","long","lat"))

# -------------------------------------------------------------------
# 6) Colores
# -------------------------------------------------------------------
colores_estrato <- c("Bajo"="#F2F2F2","Medio"="#DDDDDD","Alto"="#C8C8C8")
colores_medio <- c(
  "Auto privado"        = "#C86A62",
  "Modo activo"         = "#D4B86A",
  "Moto privada"        = "#5BA97A",
  "Taxi / Plataforma"   = "#5A9BB0",
  "Transporte informal" = "#9E77A3",
  "Transporte público"  = "#6C78A8"
)
breaks_medios <- intersect(names(colores_medio), cols_pie)

# -------------------------------------------------------------------
# 7) Brújula con flechas (arriba-derecha)
# -------------------------------------------------------------------
compass_brown <- "#6F3E2B"
arrow_compass <- function(color = "#6F3E2B", txt = 0.85, lwd = 1.8, alen = 0.08){
  grobTree(
    segmentsGrob(x0 = 0.5, y0 = 0.20, x1 = 0.5, y1 = 0.80,
                 gp = gpar(col = color, lwd = lwd),
                 arrow = arrow(type = "closed", ends = "both", length = unit(alen, "npc"))),
    segmentsGrob(x0 = 0.20, y0 = 0.5, x1 = 0.80, y1 = 0.5,
                 gp = gpar(col = color, lwd = lwd),
                 arrow = arrow(type = "closed", ends = "both", length = unit(alen, "npc"))),
    textGrob("N", x = 0.50, y = 0.96, gp = gpar(col = color, cex = txt, fontface = "bold")),
    textGrob("S", x = 0.50, y = 0.04, gp = gpar(col = color, cex = txt, fontface = "bold")),
    textGrob("E", x = 0.96, y = 0.50, gp = gpar(col = color, cex = txt, fontface = "bold")),
    textGrob("W", x = 0.04, y = 0.50, gp = gpar(col = color, cex = txt, fontface = "bold"))
  )
}

# BBox para posicionar la brújula (no toca pies)
bb    <- sf::st_bbox(shape)
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])
bxmin <- as.numeric(bb["xmin"]) + 0.82 * xspan
bxmax <- as.numeric(bb["xmin"]) + 0.92 * xspan
bymin <- as.numeric(bb["ymin"]) + 0.78 * yspan
bymax <- as.numeric(bb["ymin"]) + 0.94 * yspan

# -------------------------------------------------------------------
# 8) Mapa facetado por MOTIVO (p23_agr5) (sin nombres de zona ni coordenadas)
# -------------------------------------------------------------------
map.cali.motivo <- ggplot() +
  geom_sf(
    data = shape,
    aes(fill = categoria),
    color = "#6E6E6E",
    linewidth = 0.25,
    alpha = 0.95
  ) +
  scale_fill_manual(
    name   = "Estrato predominante",
    values = colores_estrato,
    na.value = "#F7F7F7"
  ) +
  coord_sf(clip = "off") +
  ggnewscale::new_scale_fill() +
  geom_scatterpie(
    data = df_counts,
    aes(x = long, y = lat, group = zona, r = 190*6),  # ← se mantiene igual
    cols = cols_pie,
    color = "white", linewidth = 0.25, alpha = 0.92
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",
    values = colores_medio,
    breaks = breaks_medios,
    guide  = guide_legend(override.aes = list(alpha = 1))
  ) +
  facet_wrap(~ motivo, ncol = 3) +
  labs(
    x = NULL, y = NULL,
    title = "Elección modal por comuna - Cali (por motivo de viaje)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position  = "right",
    axis.text        = element_blank(),
    axis.ticks       = element_blank()
  ) +
  annotation_custom(
    grob = arrow_compass(color = compass_brown, txt = 0.80, lwd = 1.6, alen = 0.08),
    xmin = bxmin, xmax = bxmax, ymin = bymin, ymax = bymax
  )

ggsave(
  plot = map.cali.motivo,
  filename = "map.cali_por_motivo_p23_agr5.png",
  width = 14, height = 8, dpi = 300, bg = "white"
)
