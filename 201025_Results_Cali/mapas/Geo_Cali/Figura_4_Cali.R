#############################################################################
## Figura 5: Georreferenciación de elección modal CALI x estrato x motivo  ##
#############################################################################

library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(forcats)
library(tidyr)
library(sf)
library(scatterpie)
library(ggnewscale)
library(ggspatial)
library(grid)
library(scales)
library(units)
library(FNN)
library(igraph)

options(encoding = "UTF-8")
try(Sys.setlocale("LC_CTYPE", "Spanish_Colombia.1252"), silent = TRUE)
try(Sys.setlocale("LC_CTYPE", "Spanish_Colombia.UTF-8"), silent = TRUE)
try(Sys.setlocale("LC_ALL",   "Spanish_Colombia.1252"), silent = TRUE)

base_font <- "Arial"
if ("package:plyr" %in% search()) detach("package:plyr", unload = TRUE)
if ("package:tidytable" %in% search()) detach("package:tidytable", unload = TRUE)

# -------------------------------------------------------------------
# 0) Rutas
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")

ruta_xlsx        <- "input_famd_cali_29102025.xlsx"
ruta_shp_comunas <- "mc_comunas.shp"

# ✅ shp de puntos (paradas/estaciones)
ruta_pts_mio <- "Estaciones_de_Parada_2025\\Estaciones_de_Parada_2025.shp"
# si quisieras terminales, podrías leerlas aparte y pegarlas, pero por ahora solo puntos

# -------------------------------------------------------------------
# 1) Datos encuesta
# -------------------------------------------------------------------
dataset <- readxl::read_excel(ruta_xlsx)
dataset$id     <- as.character(dataset$id)
dataset$medio  <- as.character(dataset$medio)
dataset$Comuna <- suppressWarnings(as.integer(str_extract(as.character(dataset$p19comuna), "\\d+")))
data <- dataset

# -------------------------------------------------------------------
# 2) Estrato predominante por comuna
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
tmp <- tmp[!is.na(tmp$Comuna) & !is.na(tmp$estrato_cat) & tmp$estrato_cat != "", ]
tmp$estrato_cat <- factor(tmp$estrato_cat, levels = c("Bajo","Medio","Alto"), ordered = TRUE)

estratos_comuna <- tmp %>%
  group_by(Comuna, estrato_cat) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  arrange(desc(n), estrato_cat) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(Comuna, categoria = as.character(estrato_cat))

# -------------------------------------------------------------------
# 3) Zonas (pies)
# -------------------------------------------------------------------
data$zona <- NA_character_
for (k in 1:nrow(data)) {
  if (data$Comuna[k] %in% c(1, 2, 3, 9))                data$zona[k] <- "Noroccidente"
  if (data$Comuna[k] %in% c(4, 5, 6, 7, 8))             data$zona[k] <- "Nororiente"
  if (data$Comuna[k] %in% c(11,12,13,14,15,16,21))      data$zona[k] <- "Oriente-aguablanca"
  if (data$Comuna[k] %in% c(10,17,18,19,20,22))         data$zona[k] <- "Sur"
}
data <- data[!is.na(data$zona), ]
data$zona <- factor(data$zona, levels = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur"))

coords_zona <- data.frame(
  zona = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur"),
  long = c(1060000-300, 1065000-200, 1065000-600, 1059.28*1000),
  lat  = c(875000-1050, 875000+600, 870.5*1000, 866.4*1000),
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------------
# 4) Motivo
# -------------------------------------------------------------------
if (!"p23_agregado" %in% names(data)) stop("No se encontró la columna 'p23_agregado'.")

data <- data %>%
  mutate(
    p23_agregado = trimws(as.character(p23_agregado)),
    motivo = fct_collapse(
      p23_agregado,
      "Trabajo"          = c("Trabajo"),
      "Compras/Tramites" = c("Compras y trámites","Compras y tramites","Compras y tr\u00e1mites","Compras y tramites "),
      "Tiempo personal"  = c("Recreación, salud y actividades personales",
                             "Recreacion, salud y actividades personales",
                             "Recreaci\u00f3n, salud y actividades personales",
                             "Visitas sociales"),
      "Estudio"          = c("Estudio"),
      "Cuidado"          = c(
        "Cuidado y familia (centro educativo, niños/as o jóvenes)",
        "Cuidado y familia (otro lugar, niños/as o jóvenes)",
        "Cuidado y familia (persona con discapacidad)",
        "Cuidado y familia (persona enferma)",
        "Cuidado y familia (recreación, niños)",
        "Cuidado y familia (salud, niños)",
        "Cuidado y familia (recreacion, ninos)",
        "Cuidado y familia (salud, ninos)",
        "Cuidado y familia (recreaci\u00f3n, ni\u00f1as/os)",
        "Cuidado y familia (salud, ni\u00f1as/os)"
      ),
      "Otros"            = c("Otro","Otros")
    )
  ) %>%
  filter(!is.na(motivo), motivo != "Otros")

data$motivo <- factor(
  as.character(data$motivo),
  levels = c("Trabajo","Estudio","Compras/Tramites","Tiempo personal","Cuidado")
)

# -------------------------------------------------------------------
# 5) Shape comunas + join estrato
# -------------------------------------------------------------------
shape <- sf::st_read(ruta_shp_comunas, quiet = TRUE)
col_com <- names(shape)[grepl("comuna", names(shape), ignore.case = TRUE)][1]
if (is.na(col_com)) stop("No se detectó columna con 'comuna' en el shapefile.")
shape$Comuna <- suppressWarnings(as.integer(str_extract(as.character(shape[[col_com]]), "\\d+")))
shape <- dplyr::left_join(shape, estratos_comuna, by = "Comuna")

crs_src <- sf::st_crs(shape)
if (is.na(crs_src)) stop("El shapefile de comunas no tiene CRS.")

shape_4326 <- sf::st_transform(shape, 4326)
shape_4326$categoria <- factor(shape_4326$categoria, levels = c("Bajo","Medio","Alto"))
cali_union <- sf::st_make_valid(sf::st_union(shape_4326))

# -------------------------------------------------------------------
# 6) Puntos MIO -> líneas punto-a-punto (kNN + MST) SIN puntos
# -------------------------------------------------------------------
mio_pts <- sf::st_read(ruta_pts_mio, quiet = TRUE)
if (is.na(sf::st_crs(mio_pts))) sf::st_crs(mio_pts) <- crs_src
mio_pts_4326 <- sf::st_transform(mio_pts, 4326)

mio_pts_clip <- tryCatch(
  sf::st_intersection(sf::st_make_valid(mio_pts_4326), cali_union),
  error = function(e) sf::st_crop(mio_pts_4326, sf::st_bbox(shape_4326))
)

# quita geometrías vacías y asegura puntos
mio_pts_clip <- mio_pts_clip[!sf::st_is_empty(mio_pts_clip), ]
mio_pts_clip <- sf::st_cast(mio_pts_clip, "POINT", warn = FALSE)

# trabajar en metros para distancias reales
mio_pts_m <- sf::st_transform(mio_pts_clip, 3857)

# si alguna geometría es MULTIPOINT rara, la convertimos a puntos
mio_pts_m <- sf::st_cast(mio_pts_m, "POINT", warn = FALSE)
mio_pts_m <- mio_pts_m[!sf::st_is_empty(mio_pts_m), ]

# índice propio 1..N (ESTO evita el error)
mio_pts_m$.idx <- seq_len(nrow(mio_pts_m))

xy <- sf::st_coordinates(mio_pts_m)
if (nrow(xy) < 3) stop("Muy pocos puntos dentro de Cali para construir líneas.")

# k vecinos
k <- 6
kn <- FNN::get.knn(xy, k = min(k, nrow(xy) - 1))

# aristas: from/to siempre en 1..N
edges <- do.call(rbind, lapply(seq_len(nrow(xy)), function(i){
  to <- kn$nn.index[i, ]
  w  <- kn$nn.dist[i, ]
  cbind(from = rep(i, length(to)), to = to, w = w)
}))
edges <- as.data.frame(edges)

# limpiar NAs / ceros raros
edges <- edges %>%
  filter(!is.na(from), !is.na(to), from >= 1, to >= 1, from <= nrow(xy), to <= nrow(xy))

# grafo usando vertices 1..N explícitos
verts <- data.frame(id = seq_len(nrow(xy)))
g <- igraph::graph_from_data_frame(edges[, c("from","to")], directed = FALSE, vertices = verts)
E(g)$weight <- edges$w

# MST
mst_g <- igraph::mst(g, weights = E(g)$weight)

# aristas del MST como enteros 1..N
mst_edges <- igraph::as_data_frame(mst_g, what = "edges")

# convertir aristas a LINESTRING
seg_geom <- lapply(seq_len(nrow(mst_edges)), function(r){
  i <- as.integer(mst_edges$from[r])
  j <- as.integer(mst_edges$to[r])
  sf::st_linestring(rbind(xy[i, ], xy[j, ]))
})

mio_lines_m <- sf::st_sfc(seg_geom, crs = 3857) %>% sf::st_as_sf()
mio_lines_4326 <- sf::st_transform(mio_lines_m, 4326)

# opcional: simplificar leve
mio_lines_4326 <- sf::st_simplify(mio_lines_4326, dTolerance = 0.0005, preserveTopology = TRUE)

# -------------------------------------------------------------------
# 7) Pies (scatterpie)
# -------------------------------------------------------------------
df_counts <- data %>%
  group_by(motivo, zona, medio) %>%
  summarise(n = dplyr::n(), .groups = "drop") %>%
  pivot_wider(names_from = medio, values_from = n, values_fill = 0) %>%
  left_join(coords_zona, by = "zona")

base_cols <- c("motivo","zona","long","lat")
cols_pie <- setdiff(names(df_counts), base_cols)

df_counts[cols_pie] <- lapply(df_counts[cols_pie], function(x) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  x[is.na(x)] <- 0
  x
})

pies_sf   <- st_as_sf(df_counts, coords = c("long","lat"), crs = crs_src, remove = FALSE)
pies_4326 <- st_transform(pies_sf, 4326)
pies_xy <- st_coordinates(pies_4326)
pies_df <- st_drop_geometry(pies_4326)
pies_df$long <- pies_xy[,1]
pies_df$lat  <- pies_xy[,2]

cols_pie <- cols_pie[cols_pie %in% names(pies_df)]
cols_pie <- cols_pie[sapply(pies_df[cols_pie], is.numeric)]

# -------------------------------------------------------------------
# 8) Colores
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

# azul tipo tu mapa 2 (ajusta si quieres más claro: "#6BAED6")
azul_lineas <- "#1F78B4"

# -------------------------------------------------------------------
# 9) Zoom + coordenadas limpias
# -------------------------------------------------------------------
bb <- st_bbox(shape_4326)
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])

xpad <- 0.02 * xspan
ypad <- 0.02 * yspan
xlim <- c(as.numeric(bb["xmin"]) - xpad, as.numeric(bb["xmax"]) + xpad)
ylim <- c(as.numeric(bb["ymin"]) - ypad, as.numeric(bb["ymax"]) + ypad)

r_pie <- 0.070 * min(xspan, yspan)

x_breaks <- pretty(xlim, n = 2)
y_breaks <- pretty(ylim, n = 2)
lab_x <- function(x) sprintf("%.1f°W", abs(x))
lab_y <- function(y) sprintf("%.1f°N", y)

# -------------------------------------------------------------------
# 10) Plot
# -------------------------------------------------------------------
map.cali.motivo <- ggplot() +
  
  geom_sf(
    data  = shape_4326,
    aes(fill = categoria),
    color = "#6E6E6E",
    linewidth = 0.25,
    alpha = 0.95
  ) +
  scale_fill_manual(
    name   = "Estrato predominante",
    values = colores_estrato,
    breaks = c("Bajo","Medio","Alto"),
    drop   = FALSE,
    na.value = "#FFFFFF",
    na.translate = FALSE
  ) +
  
  geom_sf(
    data = mio_lines_4326,
    aes(color = "Troncales MIO"),
    linewidth = 0.45,
    alpha = 0.95,
    lineend = "round",
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Troncales MIO" = azul_lineas),
    breaks = "Troncales MIO",
    guide  = guide_legend(override.aes = list(linewidth = 1.2, alpha = 1))
  ) +
  
  ggnewscale::new_scale_fill() +
  
  geom_scatterpie(
    data = pies_df,
    aes(x = long, y = lat, group = zona, r = r_pie),
    cols = cols_pie,
    color = "white",
    linewidth = 0.25,
    alpha = 0.92
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",
    values = colores_medio,
    breaks = breaks_medios,
    drop   = FALSE,
    guide  = guide_legend(override.aes = list(alpha = 1))
  ) +
  
  facet_wrap(~ motivo, ncol = 3, drop = TRUE) +
  
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering,
    height = unit(1.05, "cm"),
    width  = unit(1.05, "cm"),
    pad_x  = unit(0.18, "cm"),
    pad_y  = unit(0.18, "cm")
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.22,
    pad_x = unit(1.45, "cm"),
    pad_y = unit(0.18, "cm"),
    text_cex = 0.85
  ) +
  
  coord_sf(
    xlim = xlim, ylim = ylim,
    clip = "on",
    expand = FALSE,
    datum = st_crs(4326)
  ) +
  scale_x_continuous(breaks = x_breaks, labels = lab_x) +
  scale_y_continuous(breaks = y_breaks, labels = lab_y) +
  
  labs(
    title = "Elección modal por comuna - Cali (por motivo de viaje)",
    x = NULL, y = NULL
  ) +
  
  theme_minimal(base_size = 12, base_family = base_font) +
  theme(
    panel.border = element_rect(color = "grey30", fill = NA, linewidth = 0.7),
    axis.text  = element_text(size = 9, color = "grey20", family = base_font),
    axis.ticks = element_line(color = "grey25"),
    axis.ticks.length = unit(0.12, "cm"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.18),
    strip.text = element_text(face = "bold", family = base_font),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "grey70"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  plot = map.cali.motivo,
  filename = "map.cali_por_motivo_p23_agr5.png",
  width = 14, height = 8, dpi = 300, bg = "white"
)