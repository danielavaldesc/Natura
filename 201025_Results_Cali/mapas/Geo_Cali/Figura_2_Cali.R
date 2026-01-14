#############################################################################
## Figura 3: Georreferenciación de elección modal CALI x estrato x sexo    ##
#############################################################################

library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
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

# Evitar choques
if ("package:plyr" %in% search()) detach("package:plyr", unload = TRUE)
if ("package:tidytable" %in% search()) detach("package:tidytable", unload = TRUE)

# -------------------------------------------------------------------
# 0) Rutas
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
ruta_xlsx        <- "input_famd_cali_29102025.xlsx"
ruta_shp_comunas <- "mc_comunas.shp"
ruta_pts_mio <- "Estaciones_de_Parada_2025\\Estaciones_de_Parada_2025.shp"

# -------------------------------------------------------------------
# 1) Datos
# -------------------------------------------------------------------
data <- readxl::read_excel(ruta_xlsx) %>%
  mutate(
    id     = as.character(id),
    medio  = as.character(medio),
    Comuna = as.integer(gsub("\\D", "", as.character(p19comuna)))
  )

# -------------------------------------------------------------------
# 2) Estrato predominante por comuna (Bajo/Medio/Alto)
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
  summarise(n = n(), .groups = "drop_last") %>%
  arrange(desc(n), estrato_cat) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(Comuna, categoria = as.character(estrato_cat))

# -------------------------------------------------------------------
# 3) Zonas (para ubicar pies)
# -------------------------------------------------------------------
data$zona <- NA_character_
for (k in 1:nrow(data)) {
  if (data$Comuna[k] %in% c(1, 2, 3, 9))             data$zona[k] <- "Noroccidente"
  if (data$Comuna[k] %in% c(4, 5, 6, 7, 8))          data$zona[k] <- "Nororiente"
  if (data$Comuna[k] %in% c(11,12,13,14,15,16,21))   data$zona[k] <- "Oriente-aguablanca"
  if (data$Comuna[k] %in% c(10,17,18,19,20,22))      data$zona[k] <- "Sur"
}
data <- data[!is.na(data$zona), ]
data$zona <- factor(
  data$zona,
  levels = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur")
)

coords_zona <- data.frame(
  zona = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur"),
  long = c(1060000-300, 1065000-200, 1065000-600, 1059.28*1000),
  lat  = c(875000-1050, 875000+600, 870.5*1000, 866.4*1000),
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------------
# 4) p40 -> sexo (Hombre/Mujer)
# -------------------------------------------------------------------
if (!"p40" %in% names(data)) stop("No se encontró la columna p40 en la base.")
sexo_raw <- tolower(trimws(as.character(data$p40)))
data$sexo <- NA_character_
data$sexo[sexo_raw %in% c("hombre","masculino","male","m","1")] <- "Hombre"
data$sexo[sexo_raw %in% c("mujer","femenino","female","f","2")] <- "Mujer"
data <- data[!is.na(data$sexo), ]

# -------------------------------------------------------------------
# 5) Shape comunas + unión con estrato
# -------------------------------------------------------------------
shape <- sf::st_read(ruta_shp_comunas, quiet = TRUE)
columna_comuna_shape <- names(shape)[grepl("comuna", names(shape), ignore.case = TRUE)][1]
if (is.na(columna_comuna_shape)) stop("No se detectó columna con 'comuna' en el shapefile.")
shape$Comuna <- as.integer(gsub("\\D","", as.character(shape[[columna_comuna_shape]])))
shape <- left_join(shape, estratos_comuna, by = "Comuna")

crs_shape <- st_crs(shape)
if (is.na(crs_shape)) stop("El shapefile de comunas no tiene CRS. Asigna st_crs(shape) (ej. 3116).")

shape_4326 <- st_transform(shape, 4326)
shape_4326$categoria <- factor(shape_4326$categoria, levels = c("Bajo","Medio","Alto"))
cali_union <- sf::st_make_valid(sf::st_union(shape_4326))

# -------------------------------------------------------------------
# 6) Conteos por sexo-zona-medio + pivot (pies)
# -------------------------------------------------------------------
df_counts <- data %>%
  group_by(sexo, zona, medio) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = medio, values_from = n, values_fill = 0) %>%
  left_join(coords_zona, by = "zona")

cols_cand <- setdiff(names(df_counts), c("sexo","zona","long","lat"))
for (nm in cols_cand) {
  df_counts[[nm]] <- suppressWarnings(as.numeric(df_counts[[nm]]))
  df_counts[[nm]][is.na(df_counts[[nm]])] <- 0
}

# -------------------------------------------------------------------
# 7) Paletas
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

cols_pie <- intersect(cols_cand, names(colores_medio))
if (length(cols_pie) == 0) stop("cols_pie quedó vacío: revisa 'medio' en la base.")

# >>> Color líneas MIO (como en Figura 5)
azul_lineas <- "#1F78B4"

# -------------------------------------------------------------------
# 8) MIO -> líneas punto-a-punto (kNN + MST) SIN puntos (copiado de Figura 5)
# -------------------------------------------------------------------
mio_pts <- sf::st_read(ruta_pts_mio, quiet = TRUE)
if (is.na(sf::st_crs(mio_pts))) sf::st_crs(mio_pts) <- crs_shape
mio_pts_4326 <- sf::st_transform(mio_pts, 4326)

mio_pts_clip <- tryCatch(
  sf::st_intersection(sf::st_make_valid(mio_pts_4326), cali_union),
  error = function(e) sf::st_crop(mio_pts_4326, sf::st_bbox(shape_4326))
)

mio_pts_clip <- mio_pts_clip[!sf::st_is_empty(mio_pts_clip), ]
mio_pts_clip <- sf::st_cast(mio_pts_clip, "POINT", warn = FALSE)

mio_pts_m <- sf::st_transform(mio_pts_clip, 3857)
mio_pts_m <- sf::st_cast(mio_pts_m, "POINT", warn = FALSE)
mio_pts_m <- mio_pts_m[!sf::st_is_empty(mio_pts_m), ]
mio_pts_m$.idx <- seq_len(nrow(mio_pts_m))

xy <- sf::st_coordinates(mio_pts_m)
if (nrow(xy) < 3) stop("Muy pocos puntos dentro de Cali para construir líneas.")

k <- 6
kn <- FNN::get.knn(xy, k = min(k, nrow(xy) - 1))

edges <- do.call(rbind, lapply(seq_len(nrow(xy)), function(i){
  to <- kn$nn.index[i, ]
  w  <- kn$nn.dist[i, ]
  cbind(from = rep(i, length(to)), to = to, w = w)
}))
edges <- as.data.frame(edges)

edges <- edges %>%
  filter(!is.na(from), !is.na(to),
         from >= 1, to >= 1,
         from <= nrow(xy), to <= nrow(xy))

verts <- data.frame(id = seq_len(nrow(xy)))
g <- igraph::graph_from_data_frame(edges[, c("from","to")], directed = FALSE, vertices = verts)
E(g)$weight <- edges$w

mst_g <- igraph::mst(g, weights = E(g)$weight)
mst_edges <- igraph::as_data_frame(mst_g, what = "edges")

seg_geom <- lapply(seq_len(nrow(mst_edges)), function(r){
  i <- as.integer(mst_edges$from[r])
  j <- as.integer(mst_edges$to[r])
  sf::st_linestring(rbind(xy[i, ], xy[j, ]))
})

mio_lines_m    <- sf::st_sfc(seg_geom, crs = 3857) %>% sf::st_as_sf()
mio_lines_4326 <- sf::st_transform(mio_lines_m, 4326)
mio_lines_4326 <- sf::st_simplify(mio_lines_4326, dTolerance = 0.0005, preserveTopology = TRUE)

# -------------------------------------------------------------------
# 9) Pies -> transformar coords_zona (metros) a grados (igual que antes)
# -------------------------------------------------------------------
pies_sf_m  <- st_as_sf(df_counts, coords = c("long","lat"), crs = crs_shape, remove = FALSE)
pies_4326  <- st_transform(pies_sf_m, 4326)
df_counts$long <- st_coordinates(pies_4326)[,1]
df_counts$lat  <- st_coordinates(pies_4326)[,2]

bb    <- sf::st_bbox(shape_4326)
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])
r_pie <- 0.08 * min(xspan, yspan)

# -------------------------------------------------------------------
# 10) Mapa final
# -------------------------------------------------------------------
map.cali.sexo <- ggplot() +
  geom_sf(
    data = shape_4326,
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
  
  coord_sf(clip = "on") +
  
  ggnewscale::new_scale_fill() +
  geom_scatterpie(
    data = df_counts,
    aes(x = long, y = lat, group = zona, r = r_pie),
    cols = cols_pie,
    color = "white",
    linewidth = 0.25,
    alpha = 0.92
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",
    values = colores_medio[cols_pie],
    breaks = cols_pie,
    guide  = guide_legend(override.aes = list(alpha = 1))
  ) +
  
  facet_wrap(~ sexo) +
  
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style  = north_arrow_fancy_orienteering,
    height = unit(1.2, "cm"),
    width  = unit(1.2, "cm"),
    pad_x  = unit(0.2, "cm"),
    pad_y  = unit(0.2, "cm")
  ) +
  
  labs(
    x = NULL, y = NULL,
    title = "Eleccion modal por comuna - Cali (Hombres vs. Mujeres)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.35),
    panel.grid.minor = element_line(color = "grey94", linewidth = 0.20),
    axis.title       = element_blank(),
    axis.text        = element_text(size = 9, color = "grey20"),
    axis.ticks       = element_line(color = "grey20"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position  = "right"
  )

ggsave(
  plot = map.cali.sexo,
  filename = "map.cali_sexo_facet.png",
  width = 12, height = 8, dpi = 300, bg = "white"
)
