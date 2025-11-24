#############################################################################
## Figura 4: Georreferenciación de elección modal MED x estrato x edad     ##
#############################################################################


# Paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(tidyr)
library(scatterpie)
library(ggnewscale)
library(grid)
library(units)

# -------------------------------------------------------------------
# 1) Datos
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")
ruta_xlsx <- "input_famd_med_29102025.xlsx"
ruta_shp  <- "LimiteComunaCorregimiento_2014.shp"

dataset <- readxl::read_excel(ruta_xlsx)

dataset$id      <- as.character(dataset$id)
dataset$medio   <- as.character(dataset$medio)
dataset$Comuna  <- suppressWarnings(as.integer(str_extract(as.character(dataset$p19comuna), "\\d+")))

data <- dataset %>%
  filter(!is.na(medio), !is.na(Comuna), Comuna %in% 1:16)

# -------------------------------------------------------------------
# Estrato predominante por comuna (Bajo/Medio/Alto)
# -------------------------------------------------------------------
nombre_estrato <- if ("p9_estrato3" %in% names(data)) "p9_estrato3" else
  if ("p9_estratro3" %in% names(data)) "p9_estratro3" else NA
if (is.na(nombre_estrato)) stop("No se encontró la columna de estrato (p9_estrato3 / p9_estratro3).")

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
  group_by(Comuna, estrato_cat) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop_last") %>%
  arrange(desc(n), estrato_cat) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(Comuna, categoria = as.character(estrato_cat))

# -------------------------------------------------------------------
# 2) RANGO DE EDAD
# -------------------------------------------------------------------
if (!"edad_r2" %in% names(data)) stop("No se encontró la columna 'edad_r2'.")
data$rango_edad <- factor(
  trimws(as.character(data$edad_r2)),
  levels = c("18 - 34 años","35 - 54 años","55 - 80 años")
)
data <- data[!is.na(data$rango_edad), ]
data$rango_edad <- droplevels(data$rango_edad)

# -------------------------------------------------------------------
# 3) Shape Medellín → SOLO 16 comunas urbanas 
# -------------------------------------------------------------------
shape_med <- sf::st_read(ruta_shp, quiet = TRUE)

# Asegurar CRS correcto (3116 viene en el shapefile)
if (is.na(st_crs(shape_med))) {
  shape_med <- st_set_crs(shape_med, 3116)
}

# Filtrar SOLO COMUNAS 1–16 usando IDENTIFICA ("Comuna 1", ..., "Comuna 16")
# y extraer número real con CODIGO ("01","02",..., "16")
shape_comunas <- shape_med %>%
  filter(IDENTIFICA %in% paste("Comuna", 1:16)) %>%
  mutate(Comuna = as.integer(CODIGO)) %>%   # ← CÓDIGO REAL
  arrange(Comuna)

# Transformar a WGS84 y unir estrato predominante
shape <- shape_comunas %>%
  st_transform(4326) %>%
  left_join(estratos_comuna, by = "Comuna")

# -------------------------------------------------------------------
# 4) Pies: cuadrantes fijos del mapa (no se rotulan)
# -------------------------------------------------------------------
cent <- st_coordinates(st_centroid(shape))
shape$cx <- cent[,1]; shape$cy <- cent[,2]
bb <- st_bbox(shape)
xmid <- (bb["xmin"] + bb["xmax"])/2
ymid <- (bb["ymin"] + bb["ymax"])/2

shape$cuadrante <- with(shape,
                        ifelse(cy >= ymid & cx < xmid, "NW",
                               ifelse(cy >= ymid & cx >= xmid, "NE",
                                      ifelse(cy <  ymid & cx < xmid, "SW", "SE")))
)

df_counts <- data %>%
  inner_join(shape %>% st_drop_geometry() %>% dplyr::select(Comuna, cuadrante), by = "Comuna") %>%
  dplyr::count(rango_edad, cuadrante, medio, name = "n") %>%
  pivot_wider(names_from = medio, values_from = n, values_fill = 0)

cols_pie <- setdiff(names(df_counts), c("rango_edad","cuadrante"))

# posiciones de los pies (centros de cuadrantes) y radio relativo
pie_pos <- data.frame(
  cuadrante = c("NW","NE","SW","SE"),
  long = c((bb["xmin"]+xmid)/2, (xmid+bb["xmax"])/2, (bb["xmin"]+xmid)/2, (xmid+bb["xmax"])/2),
  lat  = c((ymid+bb["ymax"])/2, (ymid+bb["ymax"])/2, (bb["ymin"]+ymid)/2, (bb["ymin"]+ymid)/2)
)
df_counts <- df_counts %>% left_join(pie_pos, by = "cuadrante")

xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])
r_pie <- 0.060 * min(xspan, yspan)

# -------------------------------------------------------------------
# 5) Paletas
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
# 6) Brújula con flechas
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
bxmin <- as.numeric(bb["xmin"]) + 0.82 * xspan
bxmax <- as.numeric(bb["xmin"]) + 0.92 * xspan
bymin <- as.numeric(bb["ymin"]) + 0.78 * yspan
bymax <- as.numeric(bb["ymin"]) + 0.94 * yspan

# -------------------------------------------------------------------
# 7) Mapa facetado por rango de edad (sin nombres de zona ni coordenadas)
# -------------------------------------------------------------------
map.med.edad <- ggplot() +
  geom_sf(
    data  = shape,
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
    aes(x = long, y = lat, group = cuadrante, r = r_pie),
    cols = cols_pie,
    color = "white", linewidth = 0.25, alpha = 0.92
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",
    values = colores_medio,
    breaks = breaks_medios,
    guide  = guide_legend(override.aes = list(alpha = 1))
  ) +
  facet_wrap(~ rango_edad, ncol = 3) +
  labs(x = NULL, y = NULL,
       title = "Elección modal por comuna - Medellín (por rango de edad)") +
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
  plot = map.med.edad,
  filename = "map.med_por_edad.png",
  width = 14, height = 8, dpi = 300, bg = "white"
)
