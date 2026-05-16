###########################################################
## Figura 2: Georreferenciación de elección modal CALI   ##
###########################################################

library(readxl)
library(ggplot2)
library(dplyr)
library(sf)
library(stringr)
library(scatterpie)
library(ggnewscale)
library(ggspatial)
library(grid)
library(scales)
library(units)
library(FNN)
library(igraph)

# -------------------------------------------------------------------
# 1) Datos + rutas
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")

ruta_xlsx        <- "input_famd_cali_29102025.xlsx"
ruta_shp_comunas <- "mc_comunas.shp"
ruta_pts_mio <- "Estaciones_de_Parada_2025\\Estaciones_de_Parada_2025.shp"

dataset <- readxl::read_excel(ruta_xlsx)

dataset$id     <- as.character(dataset$id)
dataset$medio  <- as.character(dataset$medio)
dataset$Comuna <- as.integer(gsub("\\D", "", as.character(dataset$p19comuna)))
data <- dataset

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

niveles <- c("Bajo","Medio","Alto")
tmp$estrato_cat <- factor(tmp$estrato_cat, levels = niveles, ordered = TRUE)

estratos_comuna <- tmp %>%
  group_by(Comuna, estrato_cat) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  arrange(desc(n), estrato_cat) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(Comuna, categoria = as.character(estrato_cat))

# -------------------------------------------------------------------
# 3) Zonas por comuna
# -------------------------------------------------------------------
data$zona <- NA_character_
for (k in 1:nrow(data)) {
  if (data$Comuna[k] %in% c(1, 2, 3, 9))              data$zona[k] <- "Noroccidente"
  if (data$Comuna[k] %in% c(4, 5, 6, 7, 8))           data$zona[k] <- "Nororiente"
  if (data$Comuna[k] %in% c(11,12,13,14,15,16,21))    data$zona[k] <- "Oriente-aguablanca"
  if (data$Comuna[k] %in% c(10,17,18,19,20,22))       data$zona[k] <- "Sur"
}
data <- data[!is.na(data$zona), ]
data$zona <- factor(
  data$zona,
  levels = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur")
)

# -------------------------------------------------------------------
# 4) Conteos por zona/medio + coordenadas de los pies (CRS local: metros)
# -------------------------------------------------------------------

table_data_mode <- as.data.frame.matrix(table(data$zona, data$medio))
table_data_mode$zona <- rownames(table_data_mode)

table_data_mode$long <- NA_real_
table_data_mode$long[table_data_mode$zona == "Noroccidente"]       <- 1060000 - 300
table_data_mode$long[table_data_mode$zona == "Nororiente"]         <- 1065000 - 200
table_data_mode$long[table_data_mode$zona == "Oriente-aguablanca"] <- 1065000 - 600
table_data_mode$long[table_data_mode$zona == "Sur"]                <- 1059.28 * 1000

table_data_mode$lat <- NA_real_
table_data_mode$lat[table_data_mode$zona == "Noroccidente"]       <- 875000 - 1050
table_data_mode$lat[table_data_mode$zona == "Nororiente"]         <- 875000 + 600
table_data_mode$lat[table_data_mode$zona == "Oriente-aguablanca"] <- 870.5 * 1000
table_data_mode$lat[table_data_mode$zona == "Sur"]                <- 866.4 * 1000

# -------------------------------------------------------------------
# 5) Paletas
# -------------------------------------------------------------------
colores_estrato <- c("Bajo"="#F4F4F4","Medio"="#E6E6E6","Alto"="#D6D6D6")

colores_medio <- c(
  "Auto privado"        = "#C86A62",
  "Modo activo"         = "#D4B86A",
  "Moto privada"        = "#5BA97A",
  "Taxi / Plataforma"   = "#5A9BB0",
  "Transporte informal" = "#9E77A3",
  "Transporte público"  = "#6C78A8"
)

norm_medio <- function(x){
  x0 <- stringr::str_squish(stringr::str_to_lower(as.character(x)))
  x0 <- stringr::str_replace_all(x0, "\\s+", " ")
  
  dplyr::case_when(
    stringr::str_detect(x0, "auto|carro|veh[ií]culo particular") ~ "Auto privado",
    stringr::str_detect(x0, "activo|camin|caminar|bici|bicic|peat") ~ "Modo activo",
    stringr::str_detect(x0, "moto(?!taxi)") ~ "Moto privada",
    
    # informal ANTES que taxi/público para no “capturarlo” mal
    stringr::str_detect(x0, "informal|pirata|mototaxi|colectivo|guala|jeep|buseta\\s*informal|camioneta\\s*informal") ~ "Transporte informal",
    
    stringr::str_detect(x0, "taxi|uber|didi|cabif|plataforma") ~ "Taxi / Plataforma",
    stringr::str_detect(x0, "public|p[uú]blic|mio|bus|brt|trole|masivo|troncal") ~ "Transporte público",
    TRUE ~ NA_character_
  )
}

data <- data %>%
  mutate(
    medio = norm_medio(medio),
    medio = factor(medio, levels = names(colores_medio))
  ) %>%
  filter(!is.na(medio))

# Color líneas MIO + label
azul_lineas <- "#1F78B4"
label_linea <- "Troncales MIO"

# -------------------------------------------------------------------
# 6) FIX scatterpie: cols_pie numéricos + filtrar a categorías conocidas
# -------------------------------------------------------------------
cols_cand <- setdiff(names(table_data_mode), c("zona","long","lat"))

for (nm in cols_cand) {
  table_data_mode[[nm]] <- suppressWarnings(as.numeric(table_data_mode[[nm]]))
  table_data_mode[[nm]][is.na(table_data_mode[[nm]])] <- 0
}

cols_pie <- intersect(cols_cand, names(colores_medio))
if (length(cols_pie) == 0) stop("cols_pie quedó vacío: revisa niveles de 'medio' o actualiza colores_medio.")

# -------------------------------------------------------------------
# 7) Shapes comunas + unión Cali
# -------------------------------------------------------------------
shape <- sf::st_read(ruta_shp_comunas, quiet = TRUE)
columna_comuna_shape <- names(shape)[grepl("comuna", names(shape), ignore.case = TRUE)][1]
if (is.na(columna_comuna_shape)) stop("No se detectó ninguna columna con 'comuna' en el shapefile.")
shape$Comuna <- as.integer(gsub("\\D","", as.character(shape[[columna_comuna_shape]])))
shape <- left_join(shape, estratos_comuna, by = "Comuna")

crs_shape <- st_crs(shape)
if (is.na(crs_shape)) stop("El shapefile de comunas no tiene CRS. Asigna st_crs(shape) (ej. 3116).")

shape_4326 <- st_transform(shape, 4326)
shape_4326$categoria <- factor(shape_4326$categoria, levels = c("Bajo","Medio","Alto"))
cali_union <- sf::st_make_valid(sf::st_union(shape_4326))

# -------------------------------------------------------------------
# 8) MIO -> líneas punto-a-punto (kNN + MST) SIN puntos
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

xy_mio <- sf::st_coordinates(mio_pts_m)
if (nrow(xy_mio) < 3) stop("Muy pocos puntos dentro de Cali para construir líneas.")

k <- 6
kn <- FNN::get.knn(xy_mio, k = min(k, nrow(xy_mio) - 1))

edges <- do.call(rbind, lapply(seq_len(nrow(xy_mio)), function(i){
  to <- kn$nn.index[i, ]
  w  <- kn$nn.dist[i, ]
  cbind(from = rep(i, length(to)), to = to, w = w)
})) |> as.data.frame()

edges <- edges %>%
  filter(!is.na(from), !is.na(to),
         from >= 1, to >= 1,
         from <= nrow(xy_mio), to <= nrow(xy_mio))

verts <- data.frame(id = seq_len(nrow(xy_mio)))
g <- igraph::graph_from_data_frame(edges[, c("from","to")], directed = FALSE, vertices = verts)
E(g)$weight <- edges$w

mst_g <- igraph::mst(g, weights = E(g)$weight)
mst_edges <- igraph::as_data_frame(mst_g, what = "edges")

seg_geom <- lapply(seq_len(nrow(mst_edges)), function(r){
  i <- as.integer(mst_edges$from[r])
  j <- as.integer(mst_edges$to[r])
  sf::st_linestring(rbind(xy_mio[i, ], xy_mio[j, ]))
})

mio_lines_m    <- sf::st_sfc(seg_geom, crs = 3857) %>% sf::st_as_sf()
mio_lines_4326 <- sf::st_transform(mio_lines_m, 4326)
mio_lines_4326 <- sf::st_simplify(mio_lines_4326, dTolerance = 0.0005, preserveTopology = TRUE)

# -------------------------------------------------------------------
# 9) Pies: coords (metros) -> grados (scatterpie usa x/y)
# -------------------------------------------------------------------
pies_sf_m    <- st_as_sf(table_data_mode, coords = c("long","lat"), crs = crs_shape, remove = FALSE)
pies_sf_4326 <- st_transform(pies_sf_m, 4326)

pies_df <- st_drop_geometry(pies_sf_4326)
xy_pies <- st_coordinates(pies_sf_4326)
pies_df$long <- xy_pies[,1]
pies_df$lat  <- xy_pies[,2]

for (nm in cols_pie) {
  pies_df[[nm]] <- suppressWarnings(as.numeric(pies_df[[nm]]))
  pies_df[[nm]][is.na(pies_df[[nm]])] <- 0
}

# -------------------------------------------------------------------
# 10) Radio del pie en grados
# -------------------------------------------------------------------
bb    <- sf::st_bbox(shape_4326)
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])
r_pie <- 0.08 * min(xspan, yspan)

# -------------------------------------------------------------------
# 11) Mapa final
# -------------------------------------------------------------------
map.cali <- ggplot() +
  
  geom_sf(data = shape_4326, aes(fill = categoria), color = "#BFBFBF", linewidth = 0.25) +
  scale_fill_manual(
    name   = "Estrato predominante",
    values = colores_estrato,
    breaks = c("Bajo","Medio","Alto"),
    na.value = "#FAFAFA"
  ) +
  
  # ✅ Líneas MIO + leyenda "Troncales MIO"
  geom_sf(
    data = mio_lines_4326,
    aes(color = label_linea),
    linewidth = 0.45,
    alpha = 0.95,
    lineend = "round",
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    name   = NULL,
    values = setNames(azul_lineas, label_linea),
    breaks = label_linea,
    guide  = guide_legend(override.aes = list(linewidth = 1.2, alpha = 1))
  ) +
  
  coord_sf(clip = "on") +
  
  ggnewscale::new_scale_fill() +
  
  geom_scatterpie(
    data = pies_df,
    aes(x = long, y = lat, group = zona, r = r_pie),
    cols = cols_pie,
    color = "white", linewidth = 0.25, alpha = 0.92
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",
    values = colores_medio[cols_pie],
    breaks = cols_pie,
    guide  = guide_legend(override.aes = list(alpha = 1))
  ) +
  
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style  = north_arrow_fancy_orienteering,
    height = unit(1.2, "cm"),
    width  = unit(1.2, "cm"),
    pad_x  = unit(0.2, "cm"),
    pad_y  = unit(0.2, "cm")
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(1.6, "cm"),
    pad_y = unit(0.2, "cm")
  ) +
  
  labs(x = NULL, y = NULL, title = "Eleccion modal por comuna - Cali") +
  
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
  "map.cali_modal.png",
  map.cali,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
