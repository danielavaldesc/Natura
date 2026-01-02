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

ruta_xlsx      <- "input_famd_med_29102025.xlsx"
ruta_comunas   <- "LimiteComunaCorregimiento_2014.shp"

# NUEVOS SHAPES SITVA
ruta_estaciones <- "shapes_transportepublico\\estaciones_sitva_MED.shp"
ruta_vias       <- "shapes_transportepublico\\vias_sitva_MED.shp"

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
# 4) HELPERS: detectar columna + mapear a nombres completos
# ==========================================================
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
  
  # captura algo útil aunque venga "METRO A", "BRT 1", "A", etc.
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

# ==========================================================
# 5) CARGA SITVA: vías (líneas) + estaciones (puntos)
# ==========================================================
vias_ln <- st_read(ruta_vias, quiet = TRUE)
if (is.na(st_crs(vias_ln))) vias_ln <- st_set_crs(vias_ln, 3116)
vias_ln <- st_transform(vias_ln, 4326)

est_pts <- st_read(ruta_estaciones, quiet = TRUE)
if (is.na(st_crs(est_pts))) est_pts <- st_set_crs(est_pts, 3116)
est_pts <- st_transform(est_pts, 4326)

# columna para identificar línea/servicio en VIAS
col_linea_vias <- pick_col(vias_ln, c("linea","línea","line","route","nombre","name","codigo","sigla","tipo","mode","serv"))
if (is.na(col_linea_vias)) {
  # si no hay ninguna candidata, igual dibujo en un solo color (sin leyenda)
  vias_ln$linea_plot <- "SITVA"
} else {
  vias_ln$linea_plot <- make_linea_pretty(vias_ln[[col_linea_vias]])
}

vias_ln$linea_plot <- factor(vias_ln$linea_plot)

# recorte a comunas
vias_clip <- tryCatch(
  st_intersection(st_make_valid(vias_ln), med_union),
  error = function(e) st_crop(vias_ln, st_bbox(shape_comunas))
)
vias_clip$linea_plot <- droplevels(factor(vias_clip$linea_plot))

est_clip <- tryCatch(
  st_intersection(st_make_valid(est_pts), med_union),
  error = function(e) st_crop(est_pts, st_bbox(shape_comunas))
)

# ==========================================================
# 6) PALETAS
# ==========================================================
colores_estrato <- c("Bajo"="#F2F2F2","Medio"="#D9D9D9","Alto"="#BFBFBF")

pal_base <- c(
  "BRT 1"        = "#67B7E1",  # azul claro
  "BRT 2"        = "#2F7FBF",  # azul medio
  "BRT O"        = "#08306B",  # azul oscuro (BRT O)
  "Metro A"      = "#6A51A3",  # morado
  "Metro B"      = "#4D4D4D",  # gris
  "Metro C"      = "#9E9E9E",  # gris claro (si aparece)
  "Metrocable H" = "#FCA5A5",  # rojo muy claro
  "Metrocable J" = "#F87171",  # rojo claro
  "Metrocable K" = "#EF4444",  # rojo
  "Metrocable L" = "#B91C1C",  # rojo oscuro
  "Metrocable M" = "#7F1D1D",  # burdeos
  "Metrocable O" = "#6B1F2B",  # vino (si aparece)
  "Metrocable P" = "#4C0519",  # vino oscuro
  "Tram T"       = "#16A34A",  # verde
  "SITVA"        = "#444444"   
)

niv <- levels(vias_clip$linea_plot)
pal_vias <- setNames(hue_pal()(length(niv)), niv)
pal_vias[names(pal_base)[names(pal_base) %in% niv]] <- pal_base[names(pal_base) %in% niv]

orden_leyenda <- c(
  "BRT 1","BRT 2","BRT O",
  "Metro A","Metro B","Metro C",
  "Metrocable H","Metrocable J","Metrocable K","Metrocable L","Metrocable M","Metrocable O","Metrocable P",
  "Tram T","SITVA"
)
orden_leyenda <- orden_leyenda[orden_leyenda %in% niv]
vias_clip$linea_plot <- factor(vias_clip$linea_plot, levels = orden_leyenda)
pal_vias <- pal_vias[orden_leyenda]

# ==========================================================
# 7) BBOX con margen (zoom)
# ==========================================================
bb <- st_bbox(shape_comunas)
xpad <- 0.02 * as.numeric(bb["xmax"] - bb["xmin"])
ypad <- 0.02 * as.numeric(bb["ymax"] - bb["ymin"])
xlim <- c(as.numeric(bb["xmin"]) - xpad, as.numeric(bb["xmax"]) + xpad)
ylim <- c(as.numeric(bb["ymin"]) - ypad, as.numeric(bb["ymax"]) + ypad)

# ==========================================================
# 8) MAPA FINAL (comunas + vías SITVA + estaciones)
# ==========================================================
map_med_sitva <- ggplot() +
  
  # comunas estrato
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
  
  # vías SITVA (líneas)
  geom_sf(
    data = vias_clip,
    aes(color = linea_plot),
    linewidth = 1.05,
    alpha = 0.95,
    lineend = "round"
  ) +
  scale_color_manual(
    values = pal_vias,
    name = enc2utf8("Líneas SITVA"),
    breaks = orden_leyenda,
    drop = FALSE
  ) +
  
  # estaciones (puntos) — discretas para no tapar
  geom_sf(
    data = est_clip,
    shape = 21,
    size = 2.0,
    stroke = 0.35,
    color = "black",
    fill  = "white",
    alpha = 0.95,
    inherit.aes = FALSE
  ) +
  
  # etiqueta de comuna (número)
  geom_sf_text(
    data = lab_pts,
    aes(label = label),
    size = 4.0,
    fontface = "bold",
    family = base_font,
    color = "black"
  ) +
  
  # brújula + escala
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
    title = enc2utf8("Medellín • SITVA (vías y estaciones) sobre comunas urbanas"),
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
  filename = "med_sitva_comunas_estaciones_vias.png",
  plot = map_med_sitva,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
