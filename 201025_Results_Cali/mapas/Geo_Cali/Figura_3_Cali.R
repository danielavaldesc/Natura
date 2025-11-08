#############################################################################
## Figura 3: Georreferenciación de elección modal CALI x estrato x edad    ##
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
library(ggnewscale)   # <-- dos escalas de fill (estrato y pies)

# -------------------------------------------------------------------
# 1) Datos
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
dataset <- read_excel("input_famd_cali_29102025.xlsx")

dataset$id      <- as.character(dataset$id)
dataset$medio   <- as.character(dataset$medio)         # YA categorizada
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

niveles_em <- c("Bajo","Medio","Alto")
tmp$estrato_cat <- factor(tmp$estrato_cat, levels = niveles_em, ordered = TRUE)

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

# Coordenadas de los pies por ZONA + posiciones para rótulos
coords_zona <- data.frame(
  zona = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur"),
  long = c(1060000-300, 1065000-200, 1065000-600, 1059.28*1000),
  lat  = c(875000-1050, 875000+600, 870.5*1000, 866.4*1000),
  stringsAsFactors = FALSE
)
coords_zona_lab <- transform(coords_zona, lat = lat + 1800)

# -------------------------------------------------------------------
# 3) RANGO DE EDAD: usar edad_r2 con niveles dados
# -------------------------------------------------------------------
if (!"edad_r2" %in% names(data)) stop("No se encontró la columna 'edad_r2'.")
data$rango_edad <- factor(
  trimws(as.character(data$edad_r2)),
  levels = c("18 - 34 años","35 - 54 años","55 - 80 años")
)
data <- data[!is.na(data$rango_edad), ]
data$rango_edad <- droplevels(data$rango_edad)

# -------------------------------------------------------------------
# 4) Shape y unión con estrato (una sola vez)
# -------------------------------------------------------------------
shape <- st_read("mc_comunas.shp", quiet = TRUE)
columna_comuna_shape <- names(shape)[grepl("comuna", names(shape), ignore.case = TRUE)][1]
if (is.na(columna_comuna_shape)) stop("No se detectó columna con 'comuna' en el shapefile.")
shape$Comuna <- as.integer(gsub("\\D","", as.character(shape[[columna_comuna_shape]])))
shape <- dplyr::left_join(shape, estratos_comuna, by = "Comuna")

# -------------------------------------------------------------------
# 5) Conteos por rango_edad-zona-medio (wide para scatterpie)
# -------------------------------------------------------------------
df_counts <- data %>%
  dplyr::group_by(rango_edad, zona, medio) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = medio, values_from = n, values_fill = 0) %>%
  dplyr::left_join(coords_zona, by = "zona")

cols_pie <- setdiff(names(df_counts), c("rango_edad","zona","long","lat"))

# -------------------------------------------------------------------
# 6) Colores
# -------------------------------------------------------------------
# Estrato (relleno del polígono) — **grises claros**
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
breaks_medios <- intersect(names(colores_medio), cols_pie)

# -------------------------------------------------------------------
# 7) Mapa facetado por rango de edad (estrato gris + pies + rótulos)
# -------------------------------------------------------------------
map.cali.edad <- ggplot() +
  # Polígonos por estrato (grises suaves; se repiten por faceta)
  geom_sf(
    data = shape,
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
  coord_sf() +
  # Nueva escala de fill para los pies
  ggnewscale::new_scale_fill() +
  geom_scatterpie(
    data = df_counts,
    aes(x = long, y = lat, group = zona, r = 190*6),
    cols = cols_pie
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",
    values = colores_medio,
    breaks = breaks_medios
  ) +
  # Rótulos de macrozonas
  geom_text(
    data = coords_zona_lab,
    aes(long, lat, label = zona),
    color = "grey15", size = 4.5, fontface = "bold"
  ) +
  facet_wrap(~ rango_edad, ncol = 3) +
  theme_minimal(base_size = 12) +
  labs(x = NULL, y = NULL, title = "Elección modal por zona - Cali (por rango de edad)") +
  theme(
    panel.grid = element_blank(),
    legend.box = "vertical",
    legend.position = "right"
  )

ggsave(
  plot = map.cali.edad,
  filename = "map.cali_por_edad.png",
  width = 14, height = 8, dpi = 300, bg = "transparent"
)
