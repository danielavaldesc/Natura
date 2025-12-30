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

# -------------------------------------------------------------------
# 0) Rutas
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")

ruta_xlsx <- "input_famd_cali_29102025.xlsx"
ruta_shp_comunas <- "mc_comunas.shp"
ruta_shp_terminales <- "terminales\\terminales.shp"
ruta_shp_paradas <- "Estaciones_de_Parada_2025\\Estaciones_de_Parada_2025.shp"

# -------------------------------------------------------------------
# 1) Datos
# -------------------------------------------------------------------
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
  group_by(Comuna, estrato_cat) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  arrange(desc(n), estrato_cat) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(Comuna, categoria = as.character(estrato_cat))

# -------------------------------------------------------------------
# 3) Zonas (solo para ubicar pies; NO rotulamos)
# -------------------------------------------------------------------
data$zona <- NA_character_
for (k in 1:nrow(data)) {
  if (data$Comuna[k] %in% c(1, 2, 3, 9))                data$zona[k] <- "Noroccidente"
  if (data$Comuna[k] %in% c(4, 5, 6, 7, 8))             data$zona[k] <- "Nororiente"
  if (data$Comuna[k] %in% c(11,12,13,14,15,16,21))      data$zona[k] <- "Oriente-aguablanca"
  if (data$Comuna[k] %in% c(10,17,18,19,20,22))         data$zona[k] <- "Sur"
}
data <- data[!is.na(data$zona), ]
data$zona <- factor(
  data$zona,
  levels = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur")
)

# Coordenadas fijas (CRS original del shapefile: metros)
coords_zona <- data.frame(
  zona = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur"),
  long = c(1060000-300, 1065000-200, 1065000-600, 1059.28*1000),
  lat  = c(875000-1050, 875000+600, 870.5*1000, 866.4*1000),
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------------
# 4) Motivo (p23_agregado) INCLUYE "CUIDADO" y ELIMINA NA/OTROS
# -------------------------------------------------------------------
if (!"p23_agregado" %in% names(data)) stop("No se encontró la columna 'p23_agregado' en la base.")

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

niv_motivo <- c("Trabajo","Estudio","Compras/Tramites","Tiempo personal","Cuidado")
data <- data %>%
  mutate(motivo = factor(as.character(motivo), levels = niv_motivo)) %>%
  filter(!is.na(motivo)) %>%
  droplevels()

# -------------------------------------------------------------------
# 5) Shapes: comunas + unión estrato
# -------------------------------------------------------------------
shape <- sf::st_read(ruta_shp_comunas, quiet = TRUE)
columna_comuna_shape <- names(shape)[grepl("comuna", names(shape), ignore.case = TRUE)][1]
if (is.na(columna_comuna_shape)) stop("No se detectó columna con 'comuna' en el shapefile.")
shape$Comuna <- as.integer(gsub("\\D","", as.character(shape[[columna_comuna_shape]])))
shape <- dplyr::left_join(shape, estratos_comuna, by = "Comuna")

# CRS base
crs_src <- sf::st_crs(shape)
if (is.na(crs_src)) stop("El shapefile de comunas no tiene CRS (defínelo antes de transformar).")

# -------------------------------------------------------------------
# 6) Paradas y terminales (MIO)
# -------------------------------------------------------------------
terminales <- sf::st_read(ruta_shp_terminales, quiet = TRUE)
paradas    <- sf::st_read(ruta_shp_paradas, quiet = TRUE)

if (is.na(sf::st_crs(paradas)))    sf::st_crs(paradas)    <- crs_src
if (is.na(sf::st_crs(terminales))) sf::st_crs(terminales) <- crs_src

# A WGS84 para grados + cuadro
shape_4326      <- sf::st_transform(shape, 4326)
paradas_4326    <- sf::st_transform(paradas, 4326)
terminales_4326 <- sf::st_transform(terminales, 4326)

paradas_4326$tipo    <- "Paradas MIO"
terminales_4326$tipo <- "Terminales MIO"
mio_4326 <- dplyr::bind_rows(paradas_4326, terminales_4326) %>%
  mutate(tipo = factor(tipo, levels = c("Paradas MIO","Terminales MIO")))

# -------------------------------------------------------------------
# 7) Conteos por motivo-zona-medio (wide para scatterpie)
# -------------------------------------------------------------------
df_counts <- data %>%
  group_by(motivo, zona, medio) %>%
  summarise(n = dplyr::n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = medio, values_from = n, values_fill = 0) %>%
  left_join(coords_zona, by = "zona")

# --- FIX #1: definir cols_pie solo con columnas de medios reales (excluye todo lo demás) ---
base_cols <- c("motivo","zona","long","lat")
cols_pie <- setdiff(names(df_counts), base_cols)

# --- FIX #2: forzar numéricos en df_counts (antes de convertir a sf) ---
df_counts[cols_pie] <- lapply(df_counts[cols_pie], function(x) {
  # por si viene labelled/character
  x <- suppressWarnings(as.numeric(as.character(x)))
  x[is.na(x)] <- 0
  x
})

# Convertir coords de pies a sf y a WGS84
pies_sf   <- sf::st_as_sf(df_counts, coords = c("long","lat"), crs = crs_src, remove = FALSE)
pies_4326 <- sf::st_transform(pies_sf, 4326)

# --- FIX #3 (EL CLAVE): scatterpie NO necesita sf. Pasamos data.frame “limpio” ---
# Extraer coords ya transformadas y quedarnos con columnas numéricas + llaves
pies_xy <- sf::st_coordinates(pies_4326)
pies_df <- sf::st_drop_geometry(pies_4326)
pies_df$long <- pies_xy[,1]
pies_df$lat  <- pies_xy[,2]

# --- FIX #4: recalcular cols_pie como SOLO numéricas en el objeto final ---
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

azul_paradas    <- "#6BAED6"
azul_terminales <- "#08519C"

# -------------------------------------------------------------------
# 9) Radio del pie (en grados) + recorte
# -------------------------------------------------------------------
bb <- sf::st_bbox(shape_4326)
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])

r_pie <- 0.070 * min(xspan, yspan)   # pies grandes

xlim <- c(bb["xmin"] + 0.01*xspan, bb["xmax"] - 0.01*xspan)
ylim <- c(bb["ymin"] + 0.01*yspan, bb["ymax"] - 0.01*yspan)

# -------------------------------------------------------------------
# 10) Plot (cuadro + grados + brujula + escala) + facets
# -------------------------------------------------------------------
map.cali.motivo <- ggplot() +
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
  coord_sf(xlim = xlim, ylim = ylim, clip = "on") +
  ggnewscale::new_scale_fill() +
  
  # ✅ scatterpie con data.frame limpio (ya NO falla rowSums)
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
    guide  = guide_legend(override.aes = list(alpha = 1))
  ) +
  
  # MIO: círculos y triángulos (leyenda con shape)
  geom_sf(
    data = mio_4326,
    aes(shape = tipo, color = tipo),
    size = 2.0,
    alpha = 0.95,
    stroke = 0.6
  ) +
  scale_shape_manual(values = c("Paradas MIO" = 16, "Terminales MIO" = 17), name = NULL) +
  scale_color_manual(values = c("Paradas MIO" = azul_paradas, "Terminales MIO" = azul_terminales), name = NULL) +
  guides(shape = guide_legend(order = 3), color = "none") +
  
  # Brújula + escala (abajo-izq)
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
  
  facet_wrap(~ motivo, ncol = 3, drop = TRUE) +
  labs(x = NULL, y = NULL, title = "Elección modal por comuna - Cali (por motivo de viaje)") +
  
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
    legend.position  = "right",
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text       = element_text(colour = "grey20", face = "bold")
  )

ggsave(
  plot = map.cali.motivo,
  filename = "map.cali_por_motivo_p23_agr5.png",
  width = 14, height = 8, dpi = 300, bg = "white"
)

