###########################################################
## Figura 2: Georreferenciación de elección modal MED    ##
###########################################################

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
library(scales)
library(ggspatial)

# ------------------------------------------------------------
# 0) Rutas
# ------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")
ruta_xlsx  <- "input_famd_med_29102025.xlsx"
ruta_shp   <- "LimiteComunaCorregimiento_2014.shp"

# ✅ CAMBIO: SITVA (vías + estaciones) + paleta nueva (como Figura 4/3)
ruta_vias       <- "shapes_transportepublico\\vias_sitva_MED.shp"
ruta_estaciones <- "shapes_transportepublico\\estaciones_sitva_MED.shp"

# ------------------------------------------------------------
# 1) Datos base
# ------------------------------------------------------------
dataset <- read_excel(ruta_xlsx) %>%
  mutate(
    id     = as.character(id),
    medio  = as.character(medio),
    Comuna = suppressWarnings(as.integer(str_extract(as.character(p19comuna), "\\d+")))
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

estratos_comuna <- tmp %>%
  group_by(Comuna, estrato_cat) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  arrange(desc(n), estrato_cat) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(Comuna, categoria = as.character(estrato_cat))

# ------------------------------------------------------------
# 3) Shape Medellín → 16 comunas urbanas con CÓDIGO real
# ------------------------------------------------------------
shape_med <- st_read(ruta_shp, quiet = TRUE)
if (is.na(st_crs(shape_med))) shape_med <- st_set_crs(shape_med, 3116)

shape <- shape_med %>%
  filter(IDENTIFICA %in% paste("Comuna", 1:16)) %>%
  mutate(Comuna = as.integer(CODIGO)) %>%
  arrange(Comuna) %>%
  st_transform(4326) %>%
  left_join(estratos_comuna, by = "Comuna")

shape$categoria <- factor(shape$categoria, levels = c("Bajo","Medio","Alto"))

med_union <- st_make_valid(st_union(shape))

# ------------------------------------------------------------
# 4) Pies por cuadrantes (NW/NE/SW/SE)
# ------------------------------------------------------------
cent <- st_coordinates(st_centroid(shape))
shape$cx <- cent[,1]
shape$cy <- cent[,2]

bb   <- st_bbox(shape)
xmid <- (bb["xmin"] + bb["xmax"]) / 2
ymid <- (bb["ymin"] + bb["ymax"]) / 2

shape$cuadrante <- ifelse(
  shape$cy >= ymid & shape$cx <  xmid, "NW",
  ifelse(shape$cy >= ymid & shape$cx >= xmid, "NE",
         ifelse(shape$cy <  ymid & shape$cx <  xmid, "SW", "SE"))
)

# ---- FIX COLORES TORTA: forzar factor con los niveles de la paleta
colores_medio <- c(
  "Auto privado"        = "#C86A62",
  "Modo activo"         = "#D4B86A",
  "Moto privada"        = "#5BA97A",
  "Taxi / Plataforma"   = "#5A9BB0",
  "Transporte informal" = "#9E77A3",
  "Transporte público"  = "#6C78A8"
)

data <- data %>%
  mutate(
    medio = str_squish(as.character(medio)),
    medio = factor(medio, levels = names(colores_medio))
  ) %>%
  filter(!is.na(medio))   # <- si venían medios fuera del set, se caen

table_data_mode <- data %>%
  mutate(Comuna = as.integer(Comuna)) %>%
  inner_join(shape %>% st_drop_geometry() %>% dplyr::select(Comuna, cuadrante), by = "Comuna") %>%
  count(cuadrante, medio) %>%
  pivot_wider(names_from = medio, values_from = n, values_fill = 0)

# columnas a dibujar (en el ORDEN de la paleta)
cols_pie <- intersect(names(colores_medio), names(table_data_mode))

# posiciones de pies
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])

pie_pos <- data.frame(
  cuadrante = c("NW","NE","SW","SE"),
  long = c((bb["xmin"]+xmid)/2, (xmid+bb["xmax"])/2,
           (bb["xmin"]+xmid)/2, (xmid+bb["xmax"])/2),
  lat  = c((ymid+bb["ymax"])/2, (ymid+bb["ymax"])/2,
           (bb["ymin"]+ymid)/2, (bb["ymin"]+ymid)/2)
)

table_data_mode <- table_data_mode %>% left_join(pie_pos, by = "cuadrante")
r_pie <- 0.060 * min(xspan, yspan)

# ------------------------------------------------------------
# 5) SITVA: vías (líneas) + estaciones + paleta nueva
# ------------------------------------------------------------

# helpers (idénticos a Figura 4)
pick_col <- function(x, patterns){
  cand <- names(x)[grepl(paste(patterns, collapse="|"), names(x), ignore.case = TRUE)]
  if (length(cand) == 0) return(NA_character_)
  cand[1]
}

map_labels <- c(
  "1" = "BRT 1",
  "2" = "BRT 2",
  "0" = "BRT O",
  "A" = "Metro A",
  "B" = "Metro B",
  "C" = "Metro C",
  "H" = "Metrocable H",
  "J" = "Metrocable J",
  "K" = "Metrocable K",
  "L" = "Metrocable L",
  "M" = "Metrocable M",
  "O" = "Metrocable O",
  "P" = "Metrocable P",
  "T" = "Tram T"
)

make_linea_pretty <- function(x){
  x <- str_squish(as.character(x))
  x[is.na(x) | x == ""] <- "Sin nombre"
  
  code <- x %>%
    str_to_upper() %>%
    str_replace_all("L[IÍ]NEA\\s*", "") %>%
    str_replace_all("^LINE\\s*", "") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
  
  token <- str_extract(code, "\\b(BRT\\s*[0120]|METRO\\s*[ABC]|METROCABLE\\s*[HJKLMOP]|TRAM\\s*T|TRANV[IÍ]A|[012ABC HJKLMOPT])\\b")
  token <- str_replace_all(token, "\\s+", "")
  token <- str_replace(token, "^TRANV[IÍ]A$", "T")
  token <- str_replace(token, "^TRAMT$", "T")
  token <- str_replace(token, "^METRO([ABC])$", "\\1")
  token <- str_replace(token, "^BRT([0120])$", "\\1")
  
  pretty <- ifelse(token %in% names(map_labels), map_labels[token], code)
  
  pretty %>%
    str_replace_all("^METRO\\s*([A-Z0-9]+)$", "Metro \\1") %>%
    str_replace_all("^METROCABLE\\s*([A-Z0-9]+)$", "Metrocable \\1") %>%
    str_replace_all("^BRT\\s*([A-Z0-9]+)$", "BRT \\1") %>%
    str_replace_all("^TRAM\\s*([A-Z0-9]+)$", "Tram \\1") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

# carga vías (líneas)
vias_ln <- st_read(ruta_vias, quiet = TRUE)
if (is.na(st_crs(vias_ln))) vias_ln <- st_set_crs(vias_ln, 3116)
vias_ln <- st_transform(vias_ln, 4326)

col_linea_vias <- pick_col(vias_ln, c("linea","línea","line","route","nombre","name","codigo","sigla","tipo","mode","serv"))
if (is.na(col_linea_vias)) {
  vias_ln$linea_plot <- "SITVA"
} else {
  vias_ln$linea_plot <- make_linea_pretty(vias_ln[[col_linea_vias]])
}
vias_ln$linea_plot <- factor(vias_ln$linea_plot)

metro_clip <- tryCatch(
  st_intersection(st_make_valid(vias_ln), med_union),
  error = function(e) st_crop(vias_ln, st_bbox(shape))
)
metro_clip$linea_plot <- droplevels(factor(metro_clip$linea_plot))

# carga estaciones
est_pts <- st_read(ruta_estaciones, quiet = TRUE)
if (is.na(st_crs(est_pts))) est_pts <- st_set_crs(est_pts, 3116)
est_pts <- st_transform(est_pts, 4326)

est_clip <- tryCatch(
  st_intersection(st_make_valid(est_pts), med_union),
  error = function(e) st_crop(est_pts, st_bbox(shape))
)

# paleta SITVA (la nueva)
pal_base <- c(
  "BRT 1"        = "#67B7E1",
  "BRT 2"        = "#2F7FBF",
  "BRT O"        = "#08306B",
  "Metro A"      = "#6A51A3",
  "Metro B"      = "#4D4D4D",
  "Metro C"      = "#9E9E9E",
  "Metrocable H" = "#FCA5A5",
  "Metrocable J" = "#F87171",
  "Metrocable K" = "#EF4444",
  "Metrocable L" = "#B91C1C",
  "Metrocable M" = "#7F1D1D",
  "Metrocable O" = "#6B1F2B",
  "Metrocable P" = "#4C0519",
  "Tram T"       = "#16A34A",
  "SITVA"        = "#444444"
)

niv_m <- levels(metro_clip$linea_plot)
pal_metro <- setNames(hue_pal()(length(niv_m)), niv_m)
pal_metro[names(pal_base)[names(pal_base) %in% niv_m]] <- pal_base[names(pal_base) %in% niv_m]

orden_leyenda <- c(
  "BRT 1","BRT 2","BRT O",
  "Metro A","Metro B","Metro C",
  "Metrocable H","Metrocable J","Metrocable K","Metrocable L","Metrocable M","Metrocable O","Metrocable P",
  "Tram T","SITVA"
)
orden_leyenda <- orden_leyenda[orden_leyenda %in% niv_m]
metro_clip$linea_plot <- factor(metro_clip$linea_plot, levels = orden_leyenda)
pal_metro <- pal_metro[orden_leyenda]

# ------------------------------------------------------------
# 6) Estrato paleta + BBOX para ejes y zoom
# ------------------------------------------------------------
colores_estrato <- c("Bajo"="#F4F4F4","Medio"="#E6E6E6","Alto"="#D6D6D6")

# un poquito de margen para que los grados no queden “pegados”
xpad <- 0.02 * xspan
ypad <- 0.02 * yspan
xlim <- c(as.numeric(bb["xmin"]) - xpad, as.numeric(bb["xmax"]) + xpad)
ylim <- c(as.numeric(bb["ymin"]) - ypad, as.numeric(bb["ymax"]) + ypad)

# ------------------------------------------------------------
# 7) Mapa final
#   FIX (grados): NO borres axis.text/axis.ticks
# ------------------------------------------------------------
map.med.modal <- ggplot() +
  
  # comunas (estrato)
  geom_sf(data = shape, aes(fill = categoria), color = "#BFBFBF", linewidth = 0.25) +
  scale_fill_manual(
    name   = "Estrato predominante",
    values = colores_estrato,
    breaks = c("Bajo","Medio","Alto"),
    na.value = "#FAFAFA"
  ) +
  
  # SITVA (líneas)
  geom_sf(
    data = metro_clip,
    aes(color = linea_plot),
    linewidth = 1.05,
    alpha = 0.95,
    lineend = "round",
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    name   = "Líneas",
    values = pal_metro,
    breaks = orden_leyenda,
    drop = FALSE
  ) +
  
  # ✅ estaciones SITVA (puntos) — discretas para no tapar
  geom_sf(
    data = est_clip,
    shape = 21,
    size = 1.8,
    stroke = 0.35,
    color = "black",
    fill  = "white",
    alpha = 0.95,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  
  # Pies (2da escala fill)
  ggnewscale::new_scale_fill() +
  geom_scatterpie(
    data = table_data_mode,
    aes(x = long, y = lat, group = cuadrante, r = r_pie),
    cols = cols_pie,                      # <- orden fijo
    color = "white",
    linewidth = 0.25,
    alpha = 0.92
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",
    values = colores_medio[cols_pie],      # <- colores correctos
    breaks = cols_pie,
    guide  = guide_legend(override.aes = list(alpha = 1))
  ) +
  
  # Brújula + escala (tipo Cali)
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering,
    height = unit(1.15, "cm"),
    width  = unit(1.15, "cm"),
    pad_x  = unit(0.25, "cm"),
    pad_y  = unit(0.25, "cm")
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.22,
    pad_x = unit(1.55, "cm"),
    pad_y = unit(0.30, "cm"),
    text_cex = 0.85
  ) +
  
  # FIX grados: usar coord_sf con datum WGS84 + xlim/ylim
  coord_sf(
    xlim = xlim, ylim = ylim,
    expand = FALSE,
    clip = "on",
    datum = st_crs(4326)
  ) +
  
  labs(x = NULL, y = NULL, title = "Elección modal por comuna - Medellín") +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # caja del panel
    panel.border     = element_rect(color = "grey30", fill = NA, linewidth = 0.6),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # FIX grados: DEJAR ejes visibles
    axis.text        = element_text(size = 9, color = "grey20"),
    axis.ticks       = element_line(color = "grey20"),
    axis.ticks.length = unit(0.12, "cm"),
    
    legend.position  = "right",
    legend.title     = element_text(colour = "grey15"),
    legend.text      = element_text(colour = "grey20")
  )

ggsave("map.med.modal_con_metro.png", map.med.modal, width = 10, height = 8, dpi = 300, bg = "white")
