######################################################
## Figura 6: Georreferenciación comunas y Metro Med ##
######################################################

library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggspatial)
library(units)
library(scales)

# --------------------------------------------------
# 0) Encoding / fuente (arregla tildes)
# --------------------------------------------------
options(encoding = "UTF-8")
try(Sys.setlocale("LC_CTYPE", "Spanish_Colombia.1252"), silent = TRUE)
try(Sys.setlocale("LC_CTYPE", "Spanish_Colombia.UTF-8"), silent = TRUE)
try(Sys.setlocale("LC_ALL",   "Spanish_Colombia.1252"), silent = TRUE)

base_font <- "Arial"

if ("package:plyr" %in% search()) detach("package:plyr", unload = TRUE)
if ("package:tidytable" %in% search()) detach("package:tidytable", unload = TRUE)

# ==========================================================
# 1) RUTAS
# ==========================================================
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")

ruta_xlsx    <- "input_famd_med_29102025.xlsx"
ruta_comunas <- "LimiteComunaCorregimiento_2014.shp"
ruta_metro   <- "Lineas_Sistema_Metro_-OD\\Lineas_Sistema_Metro_-OD.shp"

# ==========================================================
# 2) DATOS: estrato predominante por comuna (moda)
# ==========================================================
data <- read_excel(ruta_xlsx)

data$Comuna <- suppressWarnings(as.integer(str_extract(as.character(data$p19comuna), "\\d+")))
data <- data %>% filter(!is.na(Comuna), Comuna %in% 1:16)

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
tmp <- tmp[!is.na(tmp$Comuna) & !is.na(tmp$estrato_cat) & tmp$estrato_cat != "", ]

niveles <- c("Bajo","Medio","Alto")
tmp$estrato_cat <- factor(tmp$estrato_cat, levels = niveles, ordered = TRUE)

estratos_comuna <- tmp %>%
  group_by(Comuna, estrato_cat) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  arrange(desc(n), estrato_cat) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(Comuna, categoria = as.character(estrato_cat))

# ==========================================================
# 3) SHAPE COMUNAS (solo urbanas 1–16) + JOIN estrato
# ==========================================================
shape_med <- st_read(ruta_comunas, quiet = TRUE)
if (is.na(st_crs(shape_med))) shape_med <- st_set_crs(shape_med, 3116)

shape_comunas <- shape_med %>%
  filter(IDENTIFICA %in% paste("Comuna", 1:16)) %>%
  mutate(Comuna = as.integer(CODIGO)) %>%
  arrange(Comuna) %>%
  st_transform(4326) %>%
  left_join(estratos_comuna, by = "Comuna")

shape_comunas$categoria <- factor(shape_comunas$categoria, levels = c("Bajo","Medio","Alto"))
med_union <- st_make_valid(st_union(shape_comunas))

lab_pts <- st_point_on_surface(shape_comunas) %>%
  mutate(label = as.character(Comuna))

# ==========================================================
# 4) SHAPE LÍNEAS METRO + mapeo a etiquetas tipo imagen 2
# ==========================================================
metro_ln <- st_read(ruta_metro, quiet = TRUE)
if (is.na(st_crs(metro_ln))) metro_ln <- st_set_crs(metro_ln, 3116)
metro_ln <- st_transform(metro_ln, 4326)

# Detectar columna de línea (siempre varía por SHP)
cand <- names(metro_ln)[grepl("linea|línea|line|nombre|name|route|codigo|sigla|tipo|mode", names(metro_ln), ignore.case = TRUE)]
if (length(cand) == 0) stop("No pude detectar la columna con el nombre/código de línea en el SHP del Metro.")
col_linea <- cand[1]

# 4.1) Extraer un "código corto" (A, B, H, 1, 2, etc.)
codigo_raw <- str_squish(as.character(metro_ln[[col_linea]]))
codigo_raw[is.na(codigo_raw) | codigo_raw == ""] <- "Sin nombre"

# Normalizar: quitar palabras tipo "Línea", "Linea", "Line", etc. y dejar tokens relevantes
codigo <- codigo_raw %>%
  str_to_upper() %>%
  str_replace_all("L[IÍ]NEA\\s*", "") %>%
  str_replace_all("^LINE\\s*", "") %>%
  str_replace_all("\\s+", " ") %>%
  str_trim()

# Si viene algo como "BRT 1", "METRO A", "METROCABLE H", etc. lo dejamos.
# Si viene solo "A", "B", "H", "1", "2", lo convertimos a etiqueta bonita.
map_labels <- c(
  "1" = "BRT 1",
  "2" = "BRT 2",
  "0" = "BRT O",          # si existiera
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

# Regla: si ya viene con palabra (ej "METRO A", "BRT 1"), lo respetamos pero lo capitalizamos bonito.
codigo2 <- ifelse(
  codigo %in% names(map_labels),
  map_labels[codigo],
  codigo
)

# Un poco de “pretty” para casos tipo "METRO A", "METROCABLE H", "TRAM T"
codigo2 <- codigo2 %>%
  str_replace_all("^METRO\\s*([A-Z0-9]+)$", "Metro \\1") %>%
  str_replace_all("^METROCABLE\\s*([A-Z0-9]+)$", "Metrocable \\1") %>%
  str_replace_all("^BRT\\s*([A-Z0-9]+)$", "BRT \\1") %>%
  str_replace_all("^TRAM\\s*([A-Z0-9]+)$", "Tram \\1") %>%
  str_replace_all("\\s+", " ") %>%
  str_trim()

metro_ln$linea_plot <- factor(codigo2)

# 4.2) Recorte a comunas
metro_clip <- tryCatch({
  st_intersection(st_make_valid(metro_ln), med_union)
}, error = function(e) {
  st_crop(metro_ln, st_bbox(shape_comunas))
})
metro_clip$linea_plot <- droplevels(metro_clip$linea_plot)

# ==========================================================
# 5) Paletas (estrato + líneas Metro como en imagen 2)
# ==========================================================
colores_estrato <- c("Bajo"="#F2F2F2","Medio"="#D9D9D9","Alto"="#BFBFBF")

# Paleta fija (si una línea no existe en tu shap, no pasa nada)
pal_base <- c(
  "BRT 1"        = "#F8766D",
  "BRT 2"        = "#D89000",
  "BRT O"        = "#00BFC4",
  "Metro A"      = "#619CFF",
  "Metro B"      = "#00BA38",
  "Metro C"      = "#7CAE00",
  "Metrocable H" = "#00C08B",
  "Metrocable J" = "#00BFC4",
  "Metrocable K" = "#00A9FF",
  "Metrocable L" = "#00A0FF",
  "Metrocable M" = "#A3A5FF",
  "Metrocable O" = "#C77CFF",
  "Metrocable P" = "#F564E3",
  "Tram T"       = "#FF61C3"
)

niv <- levels(metro_clip$linea_plot)

# paleta final: lo que tengamos en pal_base; si aparece algo nuevo, le asignamos automático
pal_metro <- setNames(hue_pal()(length(niv)), niv)
pal_metro[names(pal_base)[names(pal_base) %in% niv]] <- pal_base[names(pal_base) %in% niv]

# Orden de leyenda “bonito” (solo los que existan)
orden_leyenda <- c(
  "BRT 1","BRT 2","BRT O",
  "Metro A","Metro B","Metro C",
  "Metrocable H","Metrocable J","Metrocable K","Metrocable L","Metrocable M","Metrocable O","Metrocable P",
  "Tram T"
)
orden_leyenda <- orden_leyenda[orden_leyenda %in% niv]
metro_clip$linea_plot <- factor(metro_clip$linea_plot, levels = orden_leyenda)
pal_metro <- pal_metro[orden_leyenda]

# ==========================================================
# 6) BBOX con margen (zoom)
# ==========================================================
bb <- st_bbox(shape_comunas)
xpad <- 0.02 * as.numeric(bb["xmax"] - bb["xmin"])
ypad <- 0.02 * as.numeric(bb["ymax"] - bb["ymin"])
xlim <- c(as.numeric(bb["xmin"]) - xpad, as.numeric(bb["xmax"]) + xpad)
ylim <- c(as.numeric(bb["ymin"]) - ypad, as.numeric(bb["ymax"]) + ypad)

# ==========================================================
# 7) MAPA FINAL
# ==========================================================
map_med_metro <- ggplot() +
  
  geom_sf(
    data = shape_comunas,
    aes(fill = categoria),
    color = "grey35",
    linewidth = 0.30
  ) +
  scale_fill_manual(
    name = enc2utf8("Estrato predominante"),
    values = colores_estrato,
    breaks = c("Bajo","Medio","Alto"),
    drop = FALSE,
    na.value = "#FFFFFF"
  ) +
  
  geom_sf(
    data = metro_clip,
    aes(color = linea_plot),
    linewidth = 1.10,
    alpha = 0.95,
    lineend = "round"
  ) +
  scale_color_manual(
    values = pal_metro,
    name = enc2utf8("Lines"),   # si quieres "Lines" como en tu figura 2
    breaks = orden_leyenda,
    drop = FALSE
  ) +
  
  geom_sf_text(
    data = lab_pts,
    aes(label = label),
    size = 4.0,
    fontface = "bold",
    family = base_font,
    color = "black"
  ) +
  
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.1, "cm"),
    width  = unit(1.1, "cm"),
    pad_x  = unit(0.2, "cm"),
    pad_y  = unit(0.2, "cm")
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(1.6, "cm"),
    pad_y = unit(0.2, "cm")
  ) +
  
  coord_sf(xlim = xlim, ylim = ylim, clip = "on") +
  labs(
    title = enc2utf8("Medellín • Sistema Metro (líneas) sobre comunas urbanas"),
    x = NULL, y = NULL
  ) +
  
  theme_minimal(base_size = 12, base_family = base_font) +
  theme(
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.30),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.20),
    axis.text  = element_text(size = 9, color = "grey20", family = base_font),
    axis.ticks = element_line(color = "grey20"),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "grey60"),
    plot.title = element_text(face = "bold", family = base_font)
  )

ggsave(
  filename = "med_metro_estrato_comunas.png",
  plot = map_med_metro,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
