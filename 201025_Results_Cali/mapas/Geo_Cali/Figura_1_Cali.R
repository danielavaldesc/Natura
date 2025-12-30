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

# -------------------------------------------------------------------
# 1) Datos
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
dataset <- readxl::read_excel("input_famd_cali_29102025.xlsx")

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

# Coordenadas manuales EN METROS (CRS del shapefile de comunas)
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

azul_paradas    <- "#6BAED6"
azul_terminales <- "#08519C"

# -------------------------------------------------------------------
# 6) FIX scatterpie: cols_pie numéricos + filtrar a categorías conocidas
# -------------------------------------------------------------------
cols_cand <- setdiff(names(table_data_mode), c("zona","long","lat"))

for (nm in cols_cand) {
  table_data_mode[[nm]] <- suppressWarnings(as.numeric(table_data_mode[[nm]]))
  table_data_mode[[nm]][is.na(table_data_mode[[nm]])] <- 0
}

cols_pie <- intersect(cols_cand, names(colores_medio))
if (length(cols_pie) == 0) {
  stop("cols_pie quedó vacío: revisa niveles de 'medio' o actualiza colores_medio.")
}

# -------------------------------------------------------------------
# 7) Shapes: comunas + MIO
# -------------------------------------------------------------------
shape <- sf::st_read("mc_comunas.shp", quiet = TRUE)
columna_comuna_shape <- names(shape)[grepl("comuna", names(shape), ignore.case = TRUE)][1]
if (is.na(columna_comuna_shape)) stop("No se detectó ninguna columna con 'comuna' en el shapefile.")
shape$Comuna <- as.integer(gsub("\\D","", as.character(shape[[columna_comuna_shape]])))
shape <- left_join(shape, estratos_comuna, by = "Comuna")

terminales <- sf::st_read("terminales\\terminales.shp", quiet = TRUE)
paradas    <- sf::st_read("Estaciones_de_Parada_2025\\Estaciones_de_Parada_2025.shp", quiet = TRUE)

# -------------------------------------------------------------------
# 8) CRS → WGS84 (grados) + pies a data.frame (NO sf) para scatterpie
# -------------------------------------------------------------------
crs_shape <- st_crs(shape)
if (is.na(crs_shape)) stop("El shapefile de comunas no tiene CRS. Asigna st_crs(shape) (ej. 3116).")

if (is.na(st_crs(paradas)))    st_crs(paradas)    <- crs_shape
if (is.na(st_crs(terminales))) st_crs(terminales) <- crs_shape

shape_4326      <- st_transform(shape, 4326)
paradas_4326    <- st_transform(paradas, 4326)
terminales_4326 <- st_transform(terminales, 4326)

# Pies: sf solo para transformar coords, luego data.frame sin geometry
pies_sf_m    <- st_as_sf(table_data_mode, coords = c("long","lat"), crs = crs_shape, remove = FALSE)
pies_sf_4326 <- st_transform(pies_sf_m, 4326)

pies_df <- st_drop_geometry(pies_sf_4326)
xy <- st_coordinates(pies_sf_4326)
pies_df$long <- xy[,1]
pies_df$lat  <- xy[,2]

for (nm in cols_pie) {
  pies_df[[nm]] <- suppressWarnings(as.numeric(pies_df[[nm]]))
  pies_df[[nm]][is.na(pies_df[[nm]])] <- 0
}

# -------------------------------------------------------------------
# 9) Radio del pie en grados (MÁS GRANDE)
# -------------------------------------------------------------------
bb    <- sf::st_bbox(shape_4326)
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])

# antes: 0.03; ahora: más visible
r_pie <- 0.08 * min(xspan, yspan)

# -------------------------------------------------------------------
# 10) Mapa final
# -------------------------------------------------------------------
map.cali <- ggplot() +
  
  # Estrato predominante
  geom_sf(data = shape_4326, aes(fill = categoria), color = "#BFBFBF", linewidth = 0.25) +
  scale_fill_manual(
    name   = "Estrato predominante",
    values = colores_estrato,
    breaks = c("Bajo","Medio","Alto"),
    na.value = "#FAFAFA"
  ) +
  
  coord_sf(clip = "on") +
  
  # Nuevo fill para los pies
  ggnewscale::new_scale_fill() +
  
  # Pies elección modal (data.frame, no sf)
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
  
  # MIO (triángulos reales)
  geom_sf(
    data = paradas_4326,
    aes(color = "Paradas MIO"),
    shape = 16,
    size  = 1.8,
    alpha = 0.95
  ) +
  geom_sf(
    data = terminales_4326,
    aes(color = "Terminales MIO"),
    shape = 17,
    size  = 3.0,
    alpha = 0.98
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Paradas MIO"    = azul_paradas,
      "Terminales MIO" = azul_terminales
    ),
    breaks = c("Paradas MIO","Terminales MIO"),
    guide = guide_legend(override.aes = list(shape = c(16, 17), size = c(3, 3)))
  ) +
  
  # Brújula + escala
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

