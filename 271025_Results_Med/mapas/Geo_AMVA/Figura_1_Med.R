###########################################################
## Figura 2: Georreferenciación de elección modal MED    ##
###########################################################

# Paquetes
library(readxl)
library(ggplot2)
library(dplyr)
library(sf)
library(stringr)
library(scatterpie)
library(ggnewscale)
library(grid)
library(units)
library(tidyr)

# ------------------------------------------------------------
# 0) Rutas
# ------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")
ruta_xlsx <- "input_famd_med_29102025.xlsx"
ruta_shp  <- "LimiteComunaCorregimiento_2014.shp"

# ------------------------------------------------------------
# 1) Datos base
# ------------------------------------------------------------
dataset <- readxl::read_excel(ruta_xlsx)

dataset$id      <- as.character(dataset$id)
dataset$medio   <- as.character(dataset$medio)
dataset$Comuna  <- suppressWarnings(
  as.integer(str_extract(as.character(dataset$p19comuna), "\\d+"))
)

data <- dataset %>%
  filter(!is.na(medio), !is.na(Comuna), Comuna %in% 1:16)

# ------------------------------------------------------------
# 2) Estrato predominante por comuna (Bajo/Medio/Alto)
# ------------------------------------------------------------
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

lista_por_comuna <- split(tmp$estrato_cat, tmp$Comuna)

estratos_comuna_list <- lapply(names(lista_por_comuna), function(cc) {
  tb <- table(lista_por_comuna[[cc]])
  tb <- sort(tb, decreasing = TRUE)  # estrato más frecuente primero
  
  data.frame(
    Comuna    = as.integer(cc),
    categoria = names(tb)[1],
    stringsAsFactors = FALSE
  )
})

estratos_comuna <- do.call(rbind, estratos_comuna_list)

# ------------------------------------------------------------
# 3) Shape Medellín → 16 comunas urbanas con CÓDIGO real
# ------------------------------------------------------------
shape_med <- sf::st_read(ruta_shp, quiet = TRUE)

# El CRS ya viene en MAGNA-SIRGAS zona Bogotá (3116); lo aceptamos:
if (is.na(st_crs(shape_med))) {
  shape_med <- st_set_crs(shape_med, 3116)
}

# Nos quedamos solo con las comunas urbanas 1–16
# CODIGO = "01", "02", ..., "16"
shape_comunas <- shape_med %>%
  filter(IDENTIFICA %in% paste("Comuna", 1:16)) %>%
  mutate(Comuna = as.integer(CODIGO)) %>%   #  "01" -> 1, "02" -> 2, ...
  arrange(Comuna)

# Pasamos a WGS84 para dibujar y unimos el estrato
shape <- shape_comunas %>%
  st_transform(4326) %>%
  left_join(estratos_comuna, by = "Comuna")

# (si quieres comprobar:)
# shape %>% st_drop_geometry() %>% select(Comuna, NOMBRE, IDENTIFICA, categoria) %>% arrange(Comuna)

# ------------------------------------------------------------
# 4) Pies por “cuadrantes” automáticos (NW/NE/SW/SE)
# ------------------------------------------------------------
# Centroides de comunas para asignar cuadrante
cent <- st_coordinates(st_centroid(shape))
shape$cx <- cent[,1]
shape$cy <- cent[,2]

bb   <- st_bbox(shape)
xmid <- (bb["xmin"] + bb["xmax"])/2
ymid <- (bb["ymin"] + bb["ymax"])/2

shape$cuadrante <- ifelse(
  shape$cy >= ymid & shape$cx <  xmid, "NW",
  ifelse(
    shape$cy >= ymid & shape$cx >= xmid, "NE",
    ifelse(
      shape$cy <  ymid & shape$cx <  xmid, "SW",
      "SE"
    )
  )
)

# Tabla de modos por cuadrante
table_data_mode <- data %>%
  mutate(Comuna = as.integer(Comuna)) %>%
  inner_join(
    shape %>% st_drop_geometry() %>% dplyr::select(Comuna, cuadrante),
    by = "Comuna"
  ) %>%
  dplyr::count(cuadrante, medio) %>%
  tidyr::pivot_wider(
    names_from  = medio,
    values_from = n,
    values_fill = 0
  )

cols_pie <- setdiff(names(table_data_mode), "cuadrante")

# Coordenadas fijas de los pies (centros de cada cuadrante del bbox)
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])

pie_pos <- data.frame(
  cuadrante = c("NW","NE","SW","SE"),
  long = c((bb["xmin"]+xmid)/2, (xmid+bb["xmax"])/2,
           (bb["xmin"]+xmid)/2, (xmid+bb["xmax"])/2),
  lat  = c((ymid+bb["ymax"])/2, (ymid+bb["ymax"])/2,
           (bb["ymin"]+ymid)/2, (bb["ymin"]+ymid)/2)
)

table_data_mode <- table_data_mode %>%
  left_join(pie_pos, by = "cuadrante")

# Radio de los pies relativo al tamaño del mapa
r_pie <- 0.060 * min(xspan, yspan)   # tamaño legible (ajustable)

# ------------------------------------------------------------
# 5) Paletas
# ------------------------------------------------------------
colores_estrato <- c("Bajo"="#F4F4F4","Medio"="#E6E6E6","Alto"="#D6D6D6")
colores_medio <- c(
  "Auto privado"        = "#C86A62",
  "Modo activo"         = "#D4B86A",
  "Moto privada"        = "#5BA97A",
  "Taxi / Plataforma"   = "#5A9BB0",
  "Transporte informal" = "#9E77A3",
  "Transporte público"  = "#6C78A8"
)

# ------------------------------------------------------------
# 6) Brújula con flechas
# ------------------------------------------------------------
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

# Ubicación brújula (arriba-derecha)
bxmin <- as.numeric(bb["xmin"]) + 0.82 * xspan
bxmax <- as.numeric(bb["xmin"]) + 0.92 * xspan
bymin <- as.numeric(bb["ymin"]) + 0.78 * yspan
bymax <- as.numeric(bb["ymin"]) + 0.94 * yspan

# ------------------------------------------------------------
# 7) Mapa final
# ------------------------------------------------------------
map.med.modal <- ggplot() +
  geom_sf(data = shape, aes(fill = categoria), color = "#BFBFBF", linewidth = 0.25) +
  scale_fill_manual(
    name   = "Estrato predominante",
    values = colores_estrato,
    breaks = c("Bajo","Medio","Alto"),
    na.value = "#FAFAFA"
  ) +
  coord_sf(clip = "off") +
  ggnewscale::new_scale_fill() +
  geom_scatterpie(
    data = table_data_mode,
    aes(x = long, y = lat, group = cuadrante, r = r_pie),
    cols = cols_pie,
    color = "white", linewidth = 0.25, alpha = 0.92
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",
    values = colores_medio[cols_pie],
    breaks = cols_pie,
    guide  = guide_legend(override.aes = list(alpha = 1))
  ) +
  labs(x = NULL, y = NULL, title = "Elección modal por comuna - Medellín") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position  = "right",
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    legend.title     = element_text(colour = "grey15"),
    legend.text      = element_text(colour = "grey20")
  ) +
  annotation_custom(
    grob = arrow_compass(color = compass_brown, txt = 0.80, lwd = 1.6, alen = 0.08),
    xmin = bxmin, xmax = bxmax, ymin = bymin, ymax = bymax
  )

ggsave("map.med.modal.png", map.med.modal, width = 10, height = 8, dpi = 300, bg = "white")
