#########################################################
## Figura 1: Georreferenciación tiempo total MEDELLÍN  ##
#########################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(grid)
library(units)

# === Rutas (AMVA) ===
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")
ruta_xlsx <- "input_famd_med_29102025.xlsx"
ruta_shp  <- "LimiteComunaCorregimiento_2014.shp"

# ============ 1) Datos ============
df <- readxl::read_excel(ruta_xlsx) %>%
  mutate(
    p40 = str_to_title(trimws(as.character(p40))),
    p40 = ifelse(p40 %in% c("Hombre","Mujer"), p40, NA_character_),
    p19comuna   = str_extract(as.character(p19comuna), "\\d+"),
    p19comuna   = suppressWarnings(as.integer(p19comuna)),
    tiempo_total = suppressWarnings(as.numeric(tiempo_total))
  ) %>%
  filter(!is.na(p40), !is.na(p19comuna), !is.na(tiempo_total))

# --- Diagnóstico de cobertura por comuna ---
df %>% count(p19comuna, sort = TRUE) %>% print(n=Inf)

# --- Nos quedamos SOLO con comunas 1..16 (evita NA / corregimientos) ---
df <- df %>% filter(p19comuna %in% 1:16)

agg <- df %>%
  group_by(p19comuna, p40) %>%
  summarise(
    n         = n(),
    mean_time = mean(tiempo_total, na.rm = TRUE),
    .groups   = "drop"
  )

# ============ 2) Shape (limpieza → 16 comunas urbanas) ============
shape_med <- sf::st_read(ruta_shp, quiet = TRUE)

# Si no trae CRS, asigno WGS84 y paso a metros (3116) para áreas
if (is.na(st_crs(shape_med))) shape_med <- st_set_crs(shape_med, 4326)
shape_m <- st_transform(shape_med, 3116)

# Exploto multipolígonos, valido, quito islitas pequeñas
polys <- shape_m %>%
  st_make_valid() %>%
  st_cast("POLYGON", warn = FALSE) %>%
  mutate(
    area_m2 = as.numeric(st_area(geometry)),
    # centroides en grados para filtrar por valle urbano
    cx = st_coordinates(st_centroid(st_transform(geometry, 4326)))[,1],
    cy = st_coordinates(st_centroid(st_transform(geometry, 4326)))[,2]
  ) %>%
  filter(area_m2 >= 5e5)   # fuera islitas (<0.5 km2)

# BBox urbano amplio (sin corregimientos) y 16 más grandes
comunas_sf_m <- polys %>%
  filter(
    cx > -75.635, cx < -75.520,   # longitudes valle
    cy >   6.200,  cy <   6.340   # latitudes valle
  ) %>%
  slice_max(order_by = area_m2, n = 16, with_ties = FALSE) %>%
  mutate(comuna_join_num = row_number()) %>%
  dplyr::select(-area_m2, -cx, -cy)

# Regreso a 4326 para dibujar
comunas_sf <- st_transform(comunas_sf_m, 4326)

# ============ 3) Join ============
shape_join <- comunas_sf %>%
  left_join(agg, by = c("comuna_join_num" = "p19comuna"))

# ============ 4) Parámetros ============
lims <- range(shape_join$mean_time, na.rm = TRUE)
mid  <- mean(shape_join$mean_time, na.rm = TRUE)
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

# ============ 5) Brújula ============
bb <- st_bbox(shape_join)
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])
bxmin <- as.numeric(bb["xmin"]) + 0.82 * xspan
bxmax <- as.numeric(bb["xmin"]) + 0.92 * xspan
bymin <- as.numeric(bb["ymin"]) + 0.78 * yspan
bymax <- as.numeric(bb["ymin"]) + 0.94 * yspan

# ============ 6) Plot ============
p <- ggplot(shape_join) +
  geom_sf(aes(fill = mean_time), color = "grey70", linewidth = 0.25) +
  scale_fill_gradient2(
    name = "Tiempo promedio (min)",
    limits = lims, midpoint = mid,
    low = "#2E7D32", mid = "#F4D03F", high = "#C62828",
    na.value = "grey90",
    guide = guide_colorbar(barheight = grid::unit(60, "pt"))
  ) +
  facet_wrap(~ p40, nrow = 1) +
  labs(title = "Medellín • Tiempo promedio de viaje (min) por comuna") +
  coord_sf(clip = "off", expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid  = element_blank(),
    axis.title  = element_blank(),
    axis.text   = element_blank(),
    axis.ticks  = element_blank(),
    legend.position  = "right",
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text       = element_text(colour = "grey20", face = "bold")
  ) +
  annotation_custom(
    grob = arrow_compass(color = compass_brown, txt = 0.80, lwd = 1.6, alen = 0.08),
    xmin = bxmin, xmax = bxmax, ymin = bymin, ymax = bymax
  )

ggsave("medellin_tiempo_continuo_facet.png", p,
       width = 10, height = 6, dpi = 300, bg = "white")

