#############################################################################
## Figura 4: Georreferenciación de elección modal CALI x estrato x edad    ##
#############################################################################

library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(scatterpie)
library(ggnewscale)
library(sf)
library(ggspatial)
library(grid)

# Evitar choques
if ("package:plyr" %in% search()) detach("package:plyr", unload = TRUE)
if ("package:tidytable" %in% search()) detach("package:tidytable", unload = TRUE)

# -------------------------------------------------------------------
# 0) Rutas
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
ruta_xlsx <- "input_famd_cali_29102025.xlsx"
ruta_shp_comunas <- "mc_comunas.shp"
ruta_shp_terminales <- "terminales\\terminales.shp"
ruta_shp_paradas    <- "Estaciones_de_Parada_2025\\Estaciones_de_Parada_2025.shp"

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
if (is.na(nombre_estrato)) stop("No se encontro la columna de estrato (p9_estrato3 / p9_estratro3).")

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
# 3) Zonas + coords fijas (en CRS de comunas, metros)
# -------------------------------------------------------------------
data$zona <- NA_character_
for (k in 1:nrow(data)) {
  if (data$Comuna[k] %in% c(1, 2, 3, 9))           data$zona[k] <- "Noroccidente"
  if (data$Comuna[k] %in% c(4, 5, 6, 7, 8))        data$zona[k] <- "Nororiente"
  if (data$Comuna[k] %in% c(11,12,13,14,15,16,21)) data$zona[k] <- "Oriente-aguablanca"
  if (data$Comuna[k] %in% c(10,17,18,19,20,22))    data$zona[k] <- "Sur"
}
data <- data[!is.na(data$zona), ]
data$zona <- factor(data$zona,
                    levels = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur"))

coords_zona <- data.frame(
  zona = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur"),
  long = c(1060000-300, 1065000-200, 1065000-600, 1059.28*1000),
  lat  = c(875000-1050, 875000+600, 870.5*1000, 866.4*1000),
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------------
# 4) RANGO DE EDAD (sin depender de "años"/ñ en el script)
# -------------------------------------------------------------------
if (!"edad_r2" %in% names(data)) stop("No se encontro la columna 'edad_r2'.")

edad_raw <- trimws(as.character(data$edad_r2))
data$rango_edad <- NA_character_
data$rango_edad[str_detect(edad_raw, "\\b18\\s*[-–]\\s*34\\b")] <- "18_34"
data$rango_edad[str_detect(edad_raw, "\\b35\\s*[-–]\\s*54\\b")] <- "35_54"
data$rango_edad[str_detect(edad_raw, "\\b55\\s*[-–]\\s*80\\b")] <- "55_80"

data <- data[!is.na(data$rango_edad), ]
data$rango_edad <- factor(data$rango_edad, levels = c("18_34","35_54","55_80"))

# Labels “bonitos” sin escribir ñ literal (unicode)
label_anos <- paste0("a", "\u00f1", "os")
edad_labels <- c(
  "18_34" = paste("18 - 34", label_anos),
  "35_54" = paste("35 - 54", label_anos),
  "55_80" = paste("55 - 80", label_anos)
)

# -------------------------------------------------------------------
# 5) Shapes: comunas + union estrato + MIO
# -------------------------------------------------------------------
shape <- sf::st_read(ruta_shp_comunas, quiet = TRUE)
columna_comuna_shape <- names(shape)[grepl("comuna", names(shape), ignore.case = TRUE)][1]
if (is.na(columna_comuna_shape)) stop("No se detecto columna con 'comuna' en el shapefile.")
shape$Comuna <- as.integer(gsub("\\D","", as.character(shape[[columna_comuna_shape]])))
shape <- left_join(shape, estratos_comuna, by = "Comuna")

crs_shape <- sf::st_crs(shape)
if (is.na(crs_shape)) stop("El shapefile de comunas no tiene CRS. Debes asignarlo (ej. 3116).")

terminales <- sf::st_read(ruta_shp_terminales, quiet = TRUE)
paradas    <- sf::st_read(ruta_shp_paradas, quiet = TRUE)

# Si vienen sin CRS, asumir el de comunas
if (is.na(sf::st_crs(paradas)))    sf::st_crs(paradas)    <- crs_shape
if (is.na(sf::st_crs(terminales))) sf::st_crs(terminales) <- crs_shape

# Asegurar POINT (por si vienen multipoint)
paradas    <- sf::st_cast(paradas, "POINT", warn = FALSE)
terminales <- sf::st_cast(terminales, "POINT", warn = FALSE)

# Transformar todo a WGS84 para grados + cuadro
shape_4326      <- sf::st_transform(shape, 4326)
paradas_4326    <- sf::st_transform(paradas, 4326)
terminales_4326 <- sf::st_transform(terminales, 4326)

# -------------------------------------------------------------------
# 6) Conteos por rango_edad-zona-medio (wide para scatterpie)
# -------------------------------------------------------------------
df_counts <- data %>%
  group_by(rango_edad, zona, medio) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = medio, values_from = n, values_fill = 0) %>%
  left_join(coords_zona, by = "zona")

cols_pie_all <- setdiff(names(df_counts), c("rango_edad","zona","long","lat"))

# FIX scatterpie: todo numérico sí o sí
df_counts[cols_pie_all] <- lapply(df_counts[cols_pie_all], function(x){
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x)] <- 0
  x
})

# Paletas
colores_estrato <- c("Bajo"="#F2F2F2","Medio"="#DDDDDD","Alto"="#C8C8C8")
colores_medio <- c(
  "Auto privado"        = "#C86A62",
  "Modo activo"         = "#D4B86A",
  "Moto privada"        = "#5BA97A",
  "Taxi / Plataforma"   = "#5A9BB0",
  "Transporte informal" = "#9E77A3",
  "Transporte publico"  = "#6C78A8"
)

# Alias por si viene con tilde en la base (sin escribir tilde literal)
if ("Transporte publico" %in% names(colores_medio) && ("Transporte público" %in% cols_pie_all)) {
  colores_medio <- c(colores_medio, "Transporte público" = colores_medio[["Transporte publico"]])
}

cols_pie <- intersect(cols_pie_all, names(colores_medio))
if (length(cols_pie) == 0) stop("No hay columnas de medio que coincidan con la paleta. Revisa valores de 'medio'.")
breaks_medios <- cols_pie

# -------------------------------------------------------------------
# 7) Convertir coords_zona (metros) -> grados (para que el pie no se pierda)
# -------------------------------------------------------------------
pies_sf_m <- sf::st_as_sf(df_counts, coords = c("long","lat"), crs = crs_shape, remove = FALSE)
pies_4326 <- sf::st_transform(pies_sf_m, 4326)
coords <- sf::st_coordinates(pies_4326)
df_counts$long <- coords[,1]
df_counts$lat  <- coords[,2]

# Radio del pie en grados (MAS GRANDE)
bb    <- sf::st_bbox(shape_4326)
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])
r_pie <- 0.10 * min(xspan, yspan)  # sube/baja: 0.08–0.12

# MIO colores (dos azules)
azul_paradas    <- "#6BAED6"
azul_terminales <- "#08519C"

# -------------------------------------------------------------------
# 8) Mapa final: cuadro + grados + brujula + MIO + pies
# -------------------------------------------------------------------
map.cali.edad <- ggplot() +
  
  # Estrato predominante
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
    breaks = c("Bajo","Medio","Alto"),
    na.value = "#F7F7F7"
  ) +
  
  # Cuadro + ejes en grados
  coord_sf(clip = "on") +
  
  ggnewscale::new_scale_fill() +
  
  # Pies
  geom_scatterpie(
    data = df_counts,
    aes(x = long, y = lat, group = zona, r = r_pie),
    cols = cols_pie,
    color = "white", linewidth = 0.25, alpha = 0.92
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",
    values = colores_medio[cols_pie],
    breaks = breaks_medios,
    guide  = guide_legend(override.aes = list(alpha = 1))
  ) +
  
  # MIO: circulos + triangulos (forzados)
  geom_sf(
    data = paradas_4326,
    aes(color = "Paradas MIO"),
    shape = 16,
    size  = 1.4,
    alpha = 0.95
  ) +
  geom_sf(
    data = terminales_4326,
    aes(color = "Terminales MIO"),
    shape = 17,
    size  = 2.7,
    alpha = 0.98
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Paradas MIO" = azul_paradas, "Terminales MIO" = azul_terminales),
    breaks = c("Paradas MIO","Terminales MIO"),
    guide = guide_legend(override.aes = list(shape = c(16, 17), size = c(3, 3)))
  ) +
  
  # Brújula
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style  = north_arrow_fancy_orienteering,
    height = unit(1.2, "cm"),
    width  = unit(1.2, "cm"),
    pad_x  = unit(0.2, "cm"),
    pad_y  = unit(0.2, "cm")
  ) +
  
  # Facetas (labels bonitos)
  facet_wrap(
    ~ rango_edad, ncol = 3,
    labeller = labeller(rango_edad = edad_labels)
  ) +
  
  labs(
    x = NULL, y = NULL,
    title = "Eleccion modal por comuna - Cali (por rango de edad)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    # Marco del mapa
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.8),
    
    # Rejilla + grados
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
  plot = map.cali.edad,
  filename = "map.cali_por_edad.png",
  width = 14, height = 8, dpi = 300, bg = "white"
)
