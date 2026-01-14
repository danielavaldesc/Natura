######################################################
## Figura 6: Georreferenciación comunas y MIO  CALI ##
######################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(ggspatial)

# Evitar choques
if ("package:plyr" %in% search()) detach("package:plyr", unload = TRUE)
if ("package:tidytable" %in% search()) detach("package:tidytable", unload = TRUE)

# ==========================================================
# 0) RUTAS
# ==========================================================
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")

ruta_xlsx       <- "input_famd_cali_29102025.xlsx"
ruta_comunas    <- "mc_comunas.shp"
ruta_terminales <- "terminales\\terminales.shp"
ruta_paradas    <- "Estaciones_de_Parada_2025\\Estaciones_de_Parada_2025.shp"
ruta_corredores <- "corredores\\corredores.shp"

# ==========================================================
# 1) DATOS – ESTRATO PREDOMINANTE POR COMUNA
# ==========================================================
data <- read_excel(ruta_xlsx)

data$Comuna <- suppressWarnings(as.integer(gsub("\\D","", as.character(data$p19comuna))))

nombre_estrato <- if ("p9_estrato3" %in% names(data)) "p9_estrato3" else
  if ("p9_estratro3" %in% names(data)) "p9_estratro3" else NA
if (is.na(nombre_estrato)) stop("No se encontró la columna de estrato.")

tmp <- data.frame(
  Comuna = data$Comuna,
  estrato_cat = trimws(tolower(as.character(data[[nombre_estrato]]))),
  stringsAsFactors = FALSE
)

tmp$estrato_cat[tmp$estrato_cat %in% c("alto","alta")]   <- "Alto"
tmp$estrato_cat[tmp$estrato_cat %in% c("medio","media")] <- "Medio"
tmp$estrato_cat[tmp$estrato_cat %in% c("bajo","baja")]   <- "Bajo"
tmp <- tmp[!is.na(tmp$Comuna) & !is.na(tmp$estrato_cat) & tmp$estrato_cat != "", ]

tmp$estrato_cat <- factor(tmp$estrato_cat, levels = c("Bajo","Medio","Alto"), ordered = TRUE)

estratos_comuna <- tmp %>%
  group_by(Comuna, estrato_cat) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  arrange(desc(n), estrato_cat) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(Comuna, categoria = as.character(estrato_cat))

# ==========================================================
# 2) SHAPES + JOIN
# ==========================================================
shape_cali <- st_read(ruta_comunas, quiet = TRUE)
terminales <- st_read(ruta_terminales, quiet = TRUE)
paradas    <- st_read(ruta_paradas, quiet = TRUE)
corredores <- st_read(ruta_corredores, quiet = TRUE)

col_comuna <- names(shape_cali)[grepl("comuna", names(shape_cali), ignore.case = TRUE)][1]
shape_cali$Comuna <- suppressWarnings(as.integer(gsub("\\D","", as.character(shape_cali[[col_comuna]]))))

shape_cali <- left_join(shape_cali, estratos_comuna, by = "Comuna")
shape_cali$categoria <- factor(shape_cali$categoria, levels = c("Bajo","Medio","Alto"))

# ==========================================================
# 3) CRS → WGS84
# ==========================================================
crs_src <- st_crs(shape_cali)

if (is.na(st_crs(terminales))) st_crs(terminales) <- crs_src
if (is.na(st_crs(paradas)))    st_crs(paradas)    <- crs_src
if (is.na(st_crs(corredores))) st_crs(corredores) <- crs_src

shape_cali <- st_transform(shape_cali, 4326)
terminales <- st_transform(terminales, 4326)
paradas    <- st_transform(paradas, 4326)
corredores <- st_transform(corredores, 4326)

# ==========================================================
# 4) RECORTE CORREDORES
# ==========================================================
cali_union <- st_make_valid(st_union(shape_cali))

corredores_clip <- tryCatch(
  st_intersection(st_make_valid(corredores), cali_union),
  error = function(e) st_crop(corredores, st_bbox(shape_cali))
)

# ==========================================================
# 5) Etiquetas comunas
# ==========================================================
lab_pts <- st_point_on_surface(shape_cali) %>%
  mutate(label = as.character(Comuna))

# ==========================================================
# 6) Puntos MIO
# ==========================================================
paradas$tipo    <- "Paradas"
terminales$tipo <- "Terminales"

mio_pts <- bind_rows(paradas, terminales) %>%
  mutate(tipo = factor(tipo, levels = c("Paradas","Terminales")))

# ==========================================================
# 7) Paletas
# ==========================================================
colores_estrato <- c(
  "Bajo"  = "#F2F2F2",
  "Medio" = "#D9D9D9",
  "Alto"  = "#BFBFBF"
)

azul_paradas    <- "#6BAED6"
azul_terminales <- "#08519C"
azul_corredor   <- "#2171B5"

# ==========================================================
# 8) Zoom
# ==========================================================
bb <- st_bbox(shape_cali)
xpad <- 0.02 * (bb["xmax"] - bb["xmin"])
ypad <- 0.02 * (bb["ymax"] - bb["ymin"])

xlim <- c(bb["xmin"] - xpad, bb["xmax"] + xpad)
ylim <- c(bb["ymin"] - ypad, bb["ymax"] + ypad)

# ==========================================================
# 9) MAPA FINAL
# ==========================================================
map_final <- ggplot() +
  
  geom_sf(
    data = shape_cali,
    aes(fill = categoria),
    color = "grey35",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    name = "Estrato predominante",
    values = colores_estrato,
    drop = FALSE
  ) +
  
  geom_sf(
    data = corredores_clip,
    color = azul_corredor,
    linewidth = 0.35,
    alpha = 0.55
  ) +
  
  geom_sf(
    data = mio_pts,
    aes(shape = tipo, color = tipo),
    size = 2.1,
    alpha = 0.95,
    stroke = 0.6
  ) +
  scale_shape_manual(
    name = NULL,
    values = c("Paradas" = 16, "Terminales" = 17)
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Paradas" = azul_paradas, "Terminales" = azul_terminales)
  ) +
  guides(
    shape = "none",
    color = guide_legend(
      order = 1,
      override.aes = list(shape = c(16, 17))
    ),
    fill = guide_legend(order = 2)
  ) +
  
  geom_sf_text(
    data = lab_pts,
    aes(label = label),
    size = 3,
    fontface = "bold"
  ) +
  
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.2, "cm"),
    width  = unit(1.2, "cm")
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25
  ) +
  
  coord_sf(xlim = xlim, ylim = ylim) +
  labs(
    title = "Cali • Estrato predominante por comuna y red MIO",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.8),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "grey60"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  "cali_mio_estrato_comunas_corredores.png",
  map_final,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
