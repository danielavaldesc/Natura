#############################################################################
## Figura 4: Georreferenciación de elección modal MED x estrato x edad     ##
#############################################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(tidyr)
library(scatterpie)
library(ggnewscale)
library(grid)
library(units)
library(scales)
library(ggspatial)

# -------------------------------------------------------------------
# 0) Encoding / fuente (arregla tildes)
# -------------------------------------------------------------------
options(encoding = "UTF-8")
try(Sys.setlocale("LC_CTYPE", "Spanish_Colombia.1252"), silent = TRUE)
try(Sys.setlocale("LC_CTYPE", "Spanish_Colombia.UTF-8"), silent = TRUE)
try(Sys.setlocale("LC_ALL",   "Spanish_Colombia.1252"), silent = TRUE)

base_font <- "Arial"

if ("package:plyr" %in% search()) detach("package:plyr", unload = TRUE)
if ("package:tidytable" %in% search()) detach("package:tidytable", unload = TRUE)

# -------------------------------------------------------------------
# 0) Rutas
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")
ruta_xlsx  <- "input_famd_med_29102025.xlsx"
ruta_shp   <- "LimiteComunaCorregimiento_2014.shp"

# NUEVAS RUTAS SITVA (Figura 6)
ruta_estaciones <- "shapes_transportepublico\\estaciones_sitva_MED.shp"
ruta_vias       <- "shapes_transportepublico\\vias_sitva_MED.shp"

# -------------------------------------------------------------------
# 1) Datos base
# -------------------------------------------------------------------
dataset <- read_excel(ruta_xlsx) %>%
  mutate(
    id     = as.character(id),
    medio  = str_squish(as.character(medio)),
    Comuna = suppressWarnings(as.integer(str_extract(as.character(p19comuna), "\\d+")))
  )

data <- dataset %>%
  filter(!is.na(medio), !is.na(Comuna), Comuna %in% 1:16)

# -------------------------------------------------------------------
# 2) Estrato predominante por comuna (MISMO MÉTODO Figura 3)
#     OJO: el group_by(Comuna) es CLAVE
# -------------------------------------------------------------------
nombre_estrato <- if ("p9_estrato3" %in% names(data)) "p9_estrato3" else
  if ("p9_estratro3" %in% names(data)) "p9_estratro3" else NA
if (is.na(nombre_estrato)) stop("No se encontró la columna de estrato (p9_estrato3 / p9_estratro3).")

tmp <- data.frame(
  Comuna = as.integer(data$Comuna),
  estrato_cat = trimws(tolower(as.character(data[[nombre_estrato]]))),
  stringsAsFactors = FALSE
)

tmp$estrato_cat[tmp$estrato_cat %in% c("alto","alta")]   <- "Alto"
tmp$estrato_cat[tmp$estrato_cat %in% c("medio","media")] <- "Medio"
tmp$estrato_cat[tmp$estrato_cat %in% c("bajo","baja")]   <- "Bajo"
tmp <- tmp[!is.na(tmp$Comuna) & !is.na(tmp$estrato_cat) & tmp$estrato_cat != "", ]

niveles_em <- c("Bajo","Medio","Alto")
tmp$estrato_cat <- factor(tmp$estrato_cat, levels = niveles_em, ordered = TRUE)

estratos_comuna <- tmp %>%
  group_by(Comuna, estrato_cat) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  arrange(desc(n), estrato_cat) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(Comuna = as.integer(Comuna), categoria = as.character(estrato_cat))

# -------------------------------------------------------------------
# 3) Rango de edad
# -------------------------------------------------------------------
if (!"edad_r2" %in% names(data)) stop("No se encontró la columna 'edad_r2'.")

data$rango_edad <- factor(
  trimws(as.character(data$edad_r2)),
  levels = c("18 - 34 años","35 - 54 años","55 - 80 años")
)
data <- data %>% filter(!is.na(rango_edad))

# -------------------------------------------------------------------
# 4) Shape comunas (1–16) + join estrato
# -------------------------------------------------------------------
shape_med <- st_read(ruta_shp, quiet = TRUE)
if (is.na(st_crs(shape_med))) shape_med <- st_set_crs(shape_med, 3116)

shape <- shape_med %>%
  filter(IDENTIFICA %in% paste("Comuna", 1:16)) %>%
  mutate(Comuna = as.integer(CODIGO)) %>%
  arrange(Comuna) %>%
  st_transform(4326) %>%
  left_join(estratos_comuna, by = "Comuna")

shape$categoria <- factor(shape$categoria, levels = c("Bajo","Medio","Alto"))

# unión para recorte
med_union <- st_make_valid(st_union(shape))

# -------------------------------------------------------------------
# 5) Pies: MISMA lógica Figura 3 (orden fijo de colores)
# -------------------------------------------------------------------
colores_medio <- c(
  "Auto privado"        = "#C86A62",
  "Modo activo"         = "#D4B86A",
  "Moto privada"        = "#5BA97A",
  "Taxi / Plataforma"   = "#5A9BB0",
  "Transporte informal" = "#9E77A3",
  "Transporte público"  = "#6C78A8"
)

# IMPORTANTÍSIMO: factor con niveles = names(colores_medio)
data <- data %>%
  mutate(
    medio = factor(str_squish(as.character(medio)), levels = names(colores_medio))
  ) %>%
  filter(!is.na(medio))

# cuadrantes
cent <- st_coordinates(st_centroid(shape))
shape$cx <- cent[,1]; shape$cy <- cent[,2]
bb <- st_bbox(shape)
xmid <- (bb["xmin"] + bb["xmax"]) / 2
ymid <- (bb["ymin"] + bb["ymax"]) / 2

shape$cuadrante <- with(shape,
                        ifelse(cy >= ymid & cx < xmid, "NW",
                               ifelse(cy >= ymid & cx >= xmid, "NE",
                                      ifelse(cy <  ymid & cx < xmid, "SW", "SE")))
)

df_counts <- data %>%
  inner_join(shape %>% st_drop_geometry() %>% dplyr::select(Comuna, cuadrante), by = "Comuna") %>%
  count(rango_edad, cuadrante, medio, name = "n") %>%
  pivot_wider(names_from = medio, values_from = n, values_fill = 0)

# columnas pie EN ORDEN paleta (y solo las que existen)
cols_pie <- intersect(names(colores_medio), names(df_counts))

# posiciones fijas pies
pie_pos <- data.frame(
  cuadrante = c("NW","NE","SW","SE"),
  long = c((bb["xmin"]+xmid)/2, (xmid+bb["xmax"])/2,
           (bb["xmin"]+xmid)/2, (xmid+bb["xmax"])/2),
  lat  = c((ymid+bb["ymax"])/2, (ymid+bb["ymax"])/2,
           (bb["ymin"]+ymid)/2, (bb["ymin"]+ymid)/2)
)
df_counts <- df_counts %>% left_join(pie_pos, by = "cuadrante")

xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])
r_pie <- 0.060 * min(xspan, yspan)

# -------------------------------------------------------------------
# 6) Paleta estrato (igual Figura 3)
# -------------------------------------------------------------------
colores_estrato <- c("Bajo"="#F2F2F2","Medio"="#DDDDDD","Alto"="#C8C8C8")

# -------------------------------------------------------------------
# 7) SITVA: vías (líneas) + estaciones (puntos) — desde Figura 6
# -------------------------------------------------------------------

# helpers (idénticos a Figura 6)
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

# carga vías (líneas) + estaciones (puntos)
vias_ln <- st_read(ruta_vias, quiet = TRUE)
if (is.na(st_crs(vias_ln))) vias_ln <- st_set_crs(vias_ln, 3116)
vias_ln <- st_transform(vias_ln, 4326)

est_pts <- st_read(ruta_estaciones, quiet = TRUE)
if (is.na(st_crs(est_pts))) est_pts <- st_set_crs(est_pts, 3116)
est_pts <- st_transform(est_pts, 4326)

# columna para identificar línea/servicio en VIAS
col_linea_vias <- pick_col(vias_ln, c("linea","línea","line","route","nombre","name","codigo","sigla","tipo","mode","serv"))
if (is.na(col_linea_vias)) {
  vias_ln$linea_plot <- "SITVA"
} else {
  vias_ln$linea_plot <- make_linea_pretty(vias_ln[[col_linea_vias]])
}
vias_ln$linea_plot <- factor(vias_ln$linea_plot)

# recorte a comunas
metro_clip <- tryCatch(
  st_intersection(st_make_valid(vias_ln), med_union),
  error = function(e) st_crop(vias_ln, st_bbox(shape))
)
metro_clip$linea_plot <- droplevels(factor(metro_clip$linea_plot))

est_clip <- tryCatch(
  st_intersection(st_make_valid(est_pts), med_union),
  error = function(e) st_crop(est_pts, st_bbox(shape))
)

# NUEVA PALETA (Figura 6)
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

# -------------------------------------------------------------------
# 8) Grados + cuadro + zoom con margen
# -------------------------------------------------------------------
xpad <- 0.02 * xspan
ypad <- 0.02 * yspan
xlim <- c(as.numeric(bb["xmin"]) - xpad, as.numeric(bb["xmax"]) + xpad)
ylim <- c(as.numeric(bb["ymin"]) - ypad, as.numeric(bb["ymax"]) + ypad)

# -------------------------------------------------------------------
# 9) Mapa final (edad)
# -------------------------------------------------------------------
map.med.edad <- ggplot() +

  # comunas (estrato)
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
    breaks = c("Bajo","Medio","Alto"),
    drop = FALSE,
    na.value = "#FFFFFF"
  ) +

  # líneas SITVA
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

  # estaciones SITVA (puntos) — discretas para no tapar
  geom_sf(
    data = est_clip,
    shape = 21,
    size = 1.8,
    stroke = 0.35,
    color = "black",
    fill  = "white",
    alpha = 0.95,
    inherit.aes = FALSE
  ) +

  # pies (2da escala fill)
  ggnewscale::new_scale_fill() +
  geom_scatterpie(
    data = df_counts,
    aes(x = long, y = lat, group = cuadrante, r = r_pie),
    cols = cols_pie,
    color = "white",
    linewidth = 0.25,
    alpha = 0.92
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",
    values = colores_medio[cols_pie],
    breaks = cols_pie,
    drop = FALSE
  ) +

  facet_wrap(~ rango_edad, ncol = 3) +

  # brújula + escala
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

  coord_sf(
    xlim = xlim, ylim = ylim,
    clip = "on",
    expand = FALSE,
    datum = st_crs(4326)
  ) +

  labs(
    title = "Elección modal por comuna - Medellín (por rango de edad)",
    x = NULL, y = NULL
  ) +

  theme_minimal(base_size = 12, base_family = base_font) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

    # CUADRO por panel
    panel.border     = element_rect(color = "grey30", fill = NA, linewidth = 0.6),

    # GRADOS visibles
    axis.text        = element_text(size = 9, color = "grey20", family = base_font),
    axis.ticks       = element_line(color = "grey20"),
    axis.ticks.length = unit(0.12, "cm"),

    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text       = element_text(face = "bold", family = base_font),

    legend.position  = "right",
    legend.background = element_rect(fill = "white", color = "grey70")
  )

ggsave(
  plot = map.med.edad,
  filename = "map.med_edad_con_metro.png",
  width = 14, height = 8, dpi = 300, bg = "white"
)

