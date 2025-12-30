#########################################################
## Figura 1: Georreferenciación tiempo total MEDELLÍN  ##
#########################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(units)
library(ggspatial)

# === Rutas (AMVA) ===
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")
ruta_xlsx  <- "input_famd_med_29102025.xlsx"
ruta_shp   <- "LimiteComunaCorregimiento_2014.shp"
ruta_metro <- "Lineas_Sistema_Metro_-OD\\Lineas_Sistema_Metro_-OD.shp"

# ==========================================================
# 1) Datos
# ==========================================================
df <- read_excel(ruta_xlsx) %>%
  mutate(
    p40 = str_to_title(trimws(as.character(p40))),
    p40 = ifelse(p40 %in% c("Hombre","Mujer"), p40, NA_character_),
    p19comuna    = suppressWarnings(as.integer(str_extract(as.character(p19comuna), "\\d+"))),
    tiempo_total = suppressWarnings(as.numeric(tiempo_total))
  ) %>%
  filter(!is.na(p40), !is.na(p19comuna), !is.na(tiempo_total)) %>%
  filter(p19comuna %in% 1:16)

agg <- df %>%
  group_by(p19comuna, p40) %>%
  summarise(
    n = n(),
    mean_time = mean(tiempo_total, na.rm = TRUE),
    .groups = "drop"
  )

# ==========================================================
# 2) Shape (limpieza → 16 comunas urbanas)
# ==========================================================
shape_med <- st_read(ruta_shp, quiet = TRUE)

# Si no trae CRS, asigno 4326 y paso a 3116 para áreas
if (is.na(st_crs(shape_med))) shape_med <- st_set_crs(shape_med, 4326)
shape_m <- st_transform(shape_med, 3116)

polys <- shape_m %>%
  st_make_valid() %>%
  st_cast("POLYGON", warn = FALSE) %>%
  mutate(
    area_m2 = as.numeric(st_area(geometry)),
    cx = st_coordinates(st_centroid(st_transform(geometry, 4326)))[,1],
    cy = st_coordinates(st_centroid(st_transform(geometry, 4326)))[,2]
  ) %>%
  filter(area_m2 >= 5e5)

comunas_sf_m <- polys %>%
  filter(
    cx > -75.635, cx < -75.520,
    cy >   6.200,  cy <   6.340
  ) %>%
  slice_max(order_by = area_m2, n = 16, with_ties = FALSE) %>%
  mutate(comuna_join_num = row_number()) %>%
  dplyr::select(-area_m2, -cx, -cy)

comunas_sf <- st_transform(comunas_sf_m, 4326)

# ==========================================================
# 3) Join
# ==========================================================
shape_join <- comunas_sf %>%
  left_join(agg, by = c("comuna_join_num" = "p19comuna"))

# ==========================================================
# 4) Parámetros escala de color
# ==========================================================
lims <- range(shape_join$mean_time, na.rm = TRUE)
mid  <- mean(shape_join$mean_time, na.rm = TRUE)

# ==========================================================
# 5) Metro: gris oscuro, una sola tinta, SIN leyenda
# ==========================================================
metro_clip <- NULL
if (file.exists(ruta_metro)) {
  metro_ln <- st_read(ruta_metro, quiet = TRUE)
  if (is.na(st_crs(metro_ln))) metro_ln <- st_set_crs(metro_ln, 3116)
  metro_ln <- st_transform(metro_ln, 4326)
  
  med_union <- st_make_valid(st_union(comunas_sf))
  
  metro_clip <- tryCatch({
    st_intersection(st_make_valid(metro_ln), med_union)
  }, error = function(e) {
    st_crop(metro_ln, st_bbox(comunas_sf))
  })
}

# ==========================================================
# 6) BBOX con margen + GRADOS (ejes)
# ==========================================================
bb <- st_bbox(comunas_sf)
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])
xpad <- 0.02 * xspan
ypad <- 0.02 * yspan

xlim <- c(as.numeric(bb["xmin"]) - xpad, as.numeric(bb["xmax"]) + xpad)
ylim <- c(as.numeric(bb["ymin"]) - ypad, as.numeric(bb["ymax"]) + ypad)

# ==========================================================
# 7) Plot (RECUADRO + GRADOS + METRO gris)
# ==========================================================
p <- ggplot() +
  
  # Fondo: comunas + tiempo
  geom_sf(
    data = shape_join,
    aes(fill = mean_time),
    color = "grey70",
    linewidth = 0.25
  ) +
  
  # Metro en gris oscuro (sin leyenda)
  {if (!is.null(metro_clip))
    geom_sf(
      data = metro_clip,
      color = "grey25",
      linewidth = 0.80,
      alpha = 0.85,
      lineend = "round",
      inherit.aes = FALSE
    )
  } +
  
  scale_fill_gradient2(
    name = "Tiempo promedio (min)",
    limits = lims,
    midpoint = mid,
    low = "#2E7D32", mid = "#F4D03F", high = "#C62828",
    na.value = "grey90",
    guide = guide_colorbar(barheight = unit(60, "pt"))
  ) +
  
  facet_wrap(~ p40, nrow = 1) +
  
  labs(title = "Medellín • Tiempo promedio de viaje (min) por comuna") +
  
  # GRADOS: datum 4326 + límites con margen
  coord_sf(
    xlim = xlim, ylim = ylim,
    clip = "on",
    expand = FALSE,
    datum = st_crs(4326)
  ) +
  
  # Brújula + escala
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering,
    height = unit(1.15, "cm"),
    width  = unit(1.15, "cm"),
    pad_x  = unit(0.25, "cm"),
    pad_y  = unit(0.25, "cm")
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.22,
    pad_x = unit(1.55, "cm"),
    pad_y = unit(0.30, "cm"),
    text_cex = 0.85
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid  = element_blank(),
    axis.title  = element_blank(),
    
    # ✅ GRADOS visibles
    axis.text        = element_text(size = 9, color = "grey20"),
    axis.ticks       = element_line(color = "grey25"),
    axis.ticks.length = unit(0.12, "cm"),
    
    legend.position  = "right",
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text       = element_text(colour = "grey20", face = "bold"),
    
    # ✅ RECUADRO por panel
    panel.border = element_rect(color = "grey30", fill = NA, linewidth = 0.6),
    
    # sin borde externo
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  "medellin_tiempo_continuo.png",
  p,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
