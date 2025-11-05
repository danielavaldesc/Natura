#############################################################################
## Figura 2: Georreferenciación de elección modal CALI x estrato x sexo    ##
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
  if (data$Comuna[k] %in% c(1, 2, 3, 9))  data$zona[k] <- "Noroccidente"
  if (data$Comuna[k] %in% c(4, 5, 6, 7, 8)) data$zona[k] <- "Nororiente"
  if (data$Comuna[k] %in% c(11,12,13,14,15,16,21)) data$zona[k] <- "Oriente-aguablanca"
  if (data$Comuna[k] %in% c(10,17,18,19,20,22)) data$zona[k] <- "Sur"
}
data <- data[!is.na(data$zona), ]
data$zona <- factor(data$zona,
                    levels = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur"))

# Coordenadas de los pies por ZONA (amarradas por nombre; robusto a filtros)
coords_zona <- data.frame(
  zona = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur"),
  long = c(1060000-300, 1065000-200, 1065000-600, 1059.28*1000),
  lat  = c(875000-1050, 875000+600, 870.5*1000, 866.4*1000),
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------------
# 3) Función auxiliar para armar la tabla y el mapa por sexo
# -------------------------------------------------------------------
colores_medio <- c(
  "Auto privado"        = "#E4572E",
  "Modo activo"         = "#F3A712",
  "Moto privada"        = "#44AF69",
  "Taxi / Plataforma"   = "#2E86AB",
  "Transporte informal" = "#A23B72",
  "Transporte público"  = "#665191"
)

hacer_mapa_por_sexo <- function(df_subset, titulo, archivo_salida, shape_con_estrato, breaks_colores = NULL) {
  # tabla de conteos
  t <- table(df_subset$zona, df_subset$medio)
  tb <- as.data.frame.matrix(t)                      # filas: zona; columnas: medio
  tb$zona <- rownames(tb)
  tb <- merge(tb, coords_zona, by = "zona", all.x = TRUE, sort = FALSE)
  cols_pie_loc <- setdiff(names(tb), c("zona","long","lat"))
  if (is.null(breaks_colores)) breaks_colores <- cols_pie_loc
  
  g <- ggplot() +
    geom_sf(data = shape_con_estrato, color = "black", fill = NA, linewidth = 0.3) +
    coord_sf() +
    geom_scatterpie(
      data = tb,
      aes(x = long, y = lat, group = zona, r = 190*6),
      cols = cols_pie_loc
    ) +
    scale_fill_manual(
      name   = "Medio de transporte",
      values = colores_medio,
      breaks = breaks_colores
    ) +
    geom_text(
      data = {cent <- st_centroid(shape_con_estrato); cbind(cent, st_coordinates(cent))},
      aes(X, Y, label = categoria),
      color = "black", size = 4, fontface = "bold"
    ) +
    theme_minimal(base_size = 12) +
    labs(x = NULL, y = NULL, title = titulo) +
    theme(panel.grid = element_blank())
  
  ggsave(plot = g, filename = archivo_salida, width = 9, height = 7, dpi = 300, bg = "transparent")
  invisible(g)
}

# -------------------------------------------------------------------
# 4) Shape y unión con estrato (una sola vez)
# -------------------------------------------------------------------
shape <- sf::st_read("mc_comunas.shp", quiet = TRUE)
columna_comuna_shape <- names(shape)[grepl("comuna", names(shape), ignore.case = TRUE)][1]
if (is.na(columna_comuna_shape)) stop("No se detectó ninguna columna con 'comuna' en el shapefile.")
shape$Comuna <- as.integer(gsub("\\D","", as.character(shape[[columna_comuna_shape]])))
shape <- dplyr::left_join(shape, estratos_comuna, by = "Comuna")

# -------------------------------------------------------------------
# 5) Normalizar p40 -> sexo (Hombre/Mujer) y generar mapas
# -------------------------------------------------------------------
if (!"p40" %in% names(data)) stop("No se encontró la columna p40 en la base.")

sexo_raw <- tolower(trimws(as.character(data$p40)))
data$sexo <- NA_character_
data$sexo[sexo_raw %in% c("hombre","masculino","male","m","1")] <- "Hombre"
data$sexo[sexo_raw %in% c("mujer","femenino","female","f","2")] <- "Mujer"
data <- data[!is.na(data$sexo), ]

# Filtrar y crear mapas
data_h <- subset(data, sexo == "Hombre")
data_m <- subset(data, sexo == "Mujer")

hacer_mapa_por_sexo(
  df_subset       = data_h,
  titulo          = "Elección modal por zona - Cali (Hombres)",
  archivo_salida  = "map.cali_hombres.png",
  shape_con_estrato = shape
)

hacer_mapa_por_sexo(
  df_subset       = data_m,
  titulo          = "Elección modal por zona - Cali (Mujeres)",
  archivo_salida  = "map.cali_mujeres.png",
  shape_con_estrato = shape
)

# (Opcional)
# mapa_por_sexo(data, "Elección modal por zona - Cali (Total)", "map.cali_total.png", shape)




