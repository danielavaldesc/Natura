###########################################################
## Figura 1: Georreferenciación de elección modal CALI   ##
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
library(ggnewscale)   # para dos escalas de fill (estrato y pies)

# -------------------------------------------------------------------
# 1) Datos
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
dataset <- readxl::read_excel("input_famd_cali_29102025.xlsx")

# La base real trae: id, medio y p19comuna (tipo "Comuna 14")
dataset$id      <- as.character(dataset$id)
dataset$medio   <- as.character(dataset$medio)   # YA categorizada; NO recodificar
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
  if (data$Comuna[k] %in% c(1, 2, 3, 9))        data$zona[k] <- "Noroccidente"
  if (data$Comuna[k] %in% c(4, 5, 6, 7, 8))     data$zona[k] <- "Nororiente"
  if (data$Comuna[k] %in% c(11,12,13,14,15,16,21)) data$zona[k] <- "Oriente-aguablanca"
  if (data$Comuna[k] %in% c(10,17,18,19,20,22)) data$zona[k] <- "Sur"
}
data <- data[!is.na(data$zona), ]
data$zona <- factor(
  data$zona,
  levels = c("Noroccidente", "Nororiente", "Oriente-aguablanca", "Sur")
)

# -------------------------------------------------------------------
# 3) Tabla de conteos por zona y medio (manteniendo base::table)
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

# Posiciones para rótulos de macrozona (derivadas de los pies)
coords_zona_lab <- table_data_mode[, c("zona","long","lat")]
coords_zona_lab$lat <- coords_zona_lab$lat + 1800  # pequeño desplazamiento

# -------------------------------------------------------------------
# 4) Shape y unión con estrato
# -------------------------------------------------------------------
shape <- sf::st_read("mc_comunas.shp", quiet = TRUE)
columna_comuna_shape <- names(shape)[grepl("comuna", names(shape), ignore.case = TRUE)][1]
if (is.na(columna_comuna_shape)) stop("No se detectó ninguna columna con 'comuna' en el shapefile.")
shape$Comuna <- as.integer(gsub("\\D","", as.character(shape[[columna_comuna_shape]])))
shape <- dplyr::left_join(shape, estratos_comuna, by = "Comuna")

# -------------------------------------------------------------------
# 5) Colores
# -------------------------------------------------------------------
# Estrato (relleno del polígono) — tonos grises claros
colores_estrato <- c(
  "Bajo"  = "#F2F2F2",
  "Medio" = "#DDDDDD",
  "Alto"  = "#C8C8C8"
)

# Medios (pies)
colores_medio <- c(
  "Auto privado"        = "#E4572E",
  "Modo activo"         = "#F3A712",
  "Moto privada"        = "#44AF69",
  "Taxi / Plataforma"   = "#2E86AB",
  "Transporte informal" = "#A23B72",
  "Transporte público"  = "#665191"
)

# -------------------------------------------------------------------
# 6) Mapa final (estrato gris + pies + rótulos de macrozonas)
# -------------------------------------------------------------------
map.cali <- ggplot() +
  # Polígonos por estrato (grises suaves para no competir con los pies)
  geom_sf(
    data = shape,
    aes(fill = categoria),
    color = "#6E6E6E",   # borde gris medio
    linewidth = 0.25,
    alpha = 0.95
  ) +
  scale_fill_manual(
    name   = "Estrato predominante",
    values = colores_estrato,
    na.value = "#F7F7F7"
  ) +
  coord_sf() +
  # Nueva escala de fill para los pies
  ggnewscale::new_scale_fill() +
  geom_scatterpie(
    data = table_data_mode,
    aes(x = long, y = lat, group = zona, r = 190*6),
    cols = cols_pie
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",
    values = colores_medio,
    breaks = cols_pie
  ) +
  # Rótulos de macrozonas
  geom_text(
    data = coords_zona_lab,
    aes(long, lat, label = zona),
    color = "grey15", size = 4.5, fontface = "bold"
  ) +
  theme_minimal(base_size = 12) +
  labs(
    x = NULL, y = NULL,
    title = "Elección modal por zona - Cali"
  ) +
  theme(
    panel.grid = element_blank(),
    legend.box = "vertical",
    legend.position = "right"
  )

ggsave(
  plot = map.cali,
  filename = "map.cali.png",
  width = 10, height = 8, dpi = 300, bg = "transparent"
)

# -------------------------------------------------------------------
# 7) Chequeo de coordenadas (debug opcional)
# -------------------------------------------------------------------
ggplot() +
  geom_sf(data = shape, color = "black", fill = NA, linewidth = 0.3) +
  geom_point(aes(x = 1065000 - 600,  y = 870.5 * 1000), colour = "red") +
  geom_point(aes(x = 1059.28 * 1000, y = 866.4 * 1000), colour = "green") +
  geom_point(aes(x = 1065000 - 200,  y = 875000 + 600), colour = "blue") +
  geom_point(aes(x = 1060000 - 300,  y = 875000 - 1050), colour = "yellow") +
  theme_minimal() +
  labs(title = "Chequeo de coordenadas de los pies", x = NULL, y = NULL)
