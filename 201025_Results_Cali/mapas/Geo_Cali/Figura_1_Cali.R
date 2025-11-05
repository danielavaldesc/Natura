###########################################################
## Figura 2: Georreferenciación de elección modal CALI   ##
###########################################################

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

# -------------------------------------------------------------------
# 1) Datos
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
dataset <- readxl::read_excel("input_famd_cali_29102025.xlsx")

# La base real trae: id, medio y p19comuna (tipo "Comuna 14")
dataset$id      <- as.character(dataset$id)
dataset$medio   <- as.character(dataset$medio)   # YA está categorizada; NO recodificar
dataset$Comuna  <- as.integer(gsub("\\D", "", as.character(dataset$p19comuna)))

data <- dataset

# -------------------------------------------------------------------
# [NUEVO] Estrato predominante por comuna (CATEGÓRICO: Alto/Medio/Bajo)
# -------------------------------------------------------------------
# Tolera ambos nombres que has mencionado en la conversación
nombre_estrato <- if ("p9_estrato3" %in% names(data)) "p9_estrato3" else
  if ("p9_estratro3" %in% names(data)) "p9_estratro3" else NA

if (is.na(nombre_estrato)) {
  stop("No se encontró la columna de estrato (p9_estrato3 / p9_estratro3) en la base.")
}

# Normalizar texto a Alto/Medio/Bajo y calcular la MODA por comuna
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
  dplyr::group_by(Comuna, estrato_cat) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop_last") %>%
  dplyr::arrange(dplyr::desc(n), estrato_cat) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(Comuna, categoria = as.character(estrato_cat))

# -------------------------------------------------------------------
# 2) Zonas a partir de Comuna (tus rangos)
# -------------------------------------------------------------------
data$zona <- NA_character_
for (k in 1:nrow(data)) {
  if (data$Comuna[k] %in% c(1, 2, 3, 9)) {
    data$zona[k] <- "Noroccidente"
  }
  if (data$Comuna[k] %in% c(4, 5, 6, 7, 8)) {
    data$zona[k] <- "Nororiente"
  }
  if (data$Comuna[k] %in% c(11, 12, 13, 14, 15, 16, 21)) {
    data$zona[k] <- "Oriente-aguablanca"
  }
  if (data$Comuna[k] %in% c(10, 17, 18, 19, 20, 22)) {
    data$zona[k] <- "Sur"
  }
}
data <- data[!is.na(data$zona), ]

# Fijar orden de zonas para que las coordenadas por índice coincidan
data$zona <- factor(
  data$zona,
  levels = c("Noroccidente", "Nororiente", "Oriente-aguablanca", "Sur")
)

# -------------------------------------------------------------------
# 3) Tabla de conteos por zona y medio (manteniendo tu enfoque base::table)
# -------------------------------------------------------------------
table_data_mode <- table(data$zona, data$medio)
table_data_mode <- as.data.frame.array(table_data_mode)
table_data_mode$zona <- rownames(table_data_mode)

# Coordenadas de los pies por índice de fila (tus valores)
table_data_mode$long <- NA_real_
table_data_mode$long[1] <- 1060000 - 300
table_data_mode$long[2] <- 1065000 - 200 
table_data_mode$long[3] <- 1065000 - 600
table_data_mode$long[4] <- 1059.28 * 1000

table_data_mode$lat <- NA_real_
table_data_mode$lat[1] <- 875000 - 1050
table_data_mode$lat[2] <- 875000 + 600 
table_data_mode$lat[3] <- 870.5 * 1000
table_data_mode$lat[4] <- 866.4 * 1000

# Columnas de los gajos del pie (todas excepto zona/long/lat)
cols_pie <- colnames(table_data_mode)[
  setdiff(seq_len(ncol(table_data_mode)),
          match(c("zona","long","lat"), colnames(table_data_mode)))
]

# -------------------------------------------------------------------
# 4) Shape y mapa (con sf; sin rgdal/fortify)
# -------------------------------------------------------------------
# Lee el shapefile (asegúrate que .shp/.dbf/.shx estén juntos)
shape <- sf::st_read("mc_comunas.shp", quiet = TRUE)

# Detectar columna de comuna en el shape y unir el estrato
columna_comuna_shape <- names(shape)[grepl("comuna", names(shape), ignore.case = TRUE)][1]
if (is.na(columna_comuna_shape)) {
  stop("No se detectó ninguna columna con 'comuna' en el shapefile.")
}
shape$Comuna <- as.integer(gsub("\\D","", as.character(shape[[columna_comuna_shape]])))
shape <- dplyr::left_join(shape, estratos_comuna, by = "Comuna")

palette <- brewer.pal(n = 4, name = "Greys")

# Colores para los medios y renombre de leyenda
colores_medio <- c(
  "Auto privado"        = "#E4572E",
  "Modo activo"         = "#F3A712",
  "Moto privada"        = "#44AF69",
  "Taxi / Plataforma"   = "#2E86AB",
  "Transporte informal" = "#A23B72",
  "Transporte público"  = "#665191"
)

map.cali <- ggplot() +
  geom_sf(data = shape, color = "black", fill = NA, linewidth = 0.3) +
  coord_sf() +
  geom_scatterpie(
    data = table_data_mode,
    aes(x = long, y = lat, group = zona, r = 190*6),
    cols = cols_pie
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",      # (1) renombre de la leyenda
    values = colores_medio,
    breaks = cols_pie
  ) +
  # (3) Etiqueta de estrato predominante en el centroide de cada comuna
  geom_text(
    data = {cent <- st_centroid(shape); cbind(cent, st_coordinates(cent))},
    aes(X, Y, label = categoria),
    color = "black", size = 4, fontface = "bold"
  ) +
  theme_minimal(base_size = 12) +
  labs(
    x = NULL, y = NULL,
    title = "Elección modal por zona - Cali"
  ) +
  theme(panel.grid = element_blank())

ggsave(
  plot = map.cali,
  filename = "map.cali.png",
  width = 9, height = 7, dpi = 300, bg = "transparent"
)

# -------------------------------------------------------------------
# 5) Chequeo de coordenadas (debug)
# -------------------------------------------------------------------
ggplot() +
  geom_sf(data = shape, color = "black", fill = NA, linewidth = 0.3) +
  geom_point(aes(x = 1065000 - 600,  y = 870.5 * 1000), colour = "red") +
  geom_point(aes(x = 1059.28 * 1000, y = 866.4 * 1000), colour = "green") +
  geom_point(aes(x = 1065000 - 200,  y = 875000 + 600), colour = "blue") +
  geom_point(aes(x = 1060000 - 300,  y = 875000 - 1050), colour = "yellow") +
  theme_minimal() +
  labs(title = "Chequeo de coordenadas de los pies", x = NULL, y = NULL)


