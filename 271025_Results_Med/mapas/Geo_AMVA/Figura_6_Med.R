######################################
## Figura 7: Mapa de calor MEDELLÍN ##
######################################

library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(ggspatial)
library(grid)
library(units)

# ==========================================================
# 0) RUTAS
# ==========================================================
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")
ruta_tabla   <- "Datos_comunas y mpios.xlsx"
ruta_comunas <- "LimiteComunaCorregimiento_2014.shp"

# ==========================================================
# 1) HELPERS
# ==========================================================
to_num_personas <- function(x){
  x <- as.character(x)
  x <- str_replace_all(x, "\\s+", "")
  x <- str_replace_all(x, "\\.", "")
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

fmt_punto <- function(x){
  format(x, big.mark=".", decimal.mark=",", scientific=FALSE, trim=TRUE)
}

# ==========================================================
# 2) TABLA
# ==========================================================
tab <- readxl::read_excel(ruta_tabla)

col_com <- names(tab)[grepl("^comuna", names(tab), ignore.case = TRUE)][1]
if (is.na(col_com)) stop("No encuentro columna 'Comuna' en el Excel.")

col_m <- names(tab)[grepl("muj", names(tab), ignore.case = TRUE)][1]
col_h <- names(tab)[grepl("homb", names(tab), ignore.case = TRUE)][1]
col_t <- names(tab)[grepl("total", names(tab), ignore.case = TRUE)][1]

tab2 <- tab %>%
  transmute(
    Comuna  = suppressWarnings(as.integer(str_extract(as.character(.data[[col_com]]), "\\d+"))),
    mujeres = to_num_personas(.data[[col_m]]),
    hombres = to_num_personas(.data[[col_h]]),
    total   = if (!is.na(col_t)) to_num_personas(.data[[col_t]]) else NA_real_
  ) %>%
  mutate(total = ifelse(is.na(total), hombres + mujeres, total)) %>%
  filter(!is.na(Comuna), Comuna %in% 1:16) %>%
  filter(!is.na(hombres), !is.na(mujeres), !is.na(total))

# ==========================================================
# 3) SHAPE + JOIN
# ==========================================================
shape_med <- st_read(ruta_comunas, quiet = TRUE)
if (is.na(st_crs(shape_med))) shape_med <- st_set_crs(shape_med, 3116)

shape_comunas <- shape_med %>%
  filter(IDENTIFICA %in% paste("Comuna", 1:16)) %>%
  mutate(Comuna = as.integer(CODIGO)) %>%
  arrange(Comuna) %>%
  st_transform(4326) %>%
  left_join(tab2, by = "Comuna") %>%
  mutate(total_miles = total / 1000)  # <<< en miles

# ==========================================================
# 4) PUNTOS (H y M)
# ==========================================================
pts <- st_point_on_surface(shape_comunas)
xy  <- st_coordinates(pts)

centros <- pts %>%
  st_drop_geometry() %>%
  mutate(x = xy[,1], y = xy[,2]) %>%
  dplyr::select(Comuna, x, y, hombres, mujeres, total) %>%
  filter(!is.na(hombres), !is.na(mujeres), !is.na(total)) %>%
  mutate(
    txt_h = paste0("\u25B2 ", fmt_punto(hombres)),
    txt_m = paste0("\u2605 ", fmt_punto(mujeres))
  )

dy <- 0.0022
h_df <- centros %>% transmute(x, y = y + dy, txt = txt_h)
m_df <- centros %>% transmute(x, y = y - dy, txt = txt_m)

# ==========================================================
# 5) PALETA AZULES (CAMBIO)
# ==========================================================
pal_azules <- c(
  "#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1",
  "#6BAED6","#4292C6","#2171B5","#08519C","#08306B"
)

# ==========================================================
# 6) BBOX
# ==========================================================
bb <- st_bbox(shape_comunas)
xpad <- 0.02 * as.numeric(bb["xmax"] - bb["xmin"])
ypad <- 0.02 * as.numeric(bb["ymax"] - bb["ymin"])
xlim <- c(as.numeric(bb["xmin"]) - xpad, as.numeric(bb["xmax"]) + xpad)
ylim <- c(as.numeric(bb["ymin"]) - ypad, as.numeric(bb["ymax"]) + ypad)

# ==========================================================
# 7) MAPA FINAL (solo cambia paleta + label)
# ==========================================================
p <- ggplot() +
  geom_sf(
    data = shape_comunas,
    aes(fill = total_miles),
    color = "grey40",
    linewidth = 0.25
  ) +
  scale_fill_gradientn(
    colors = pal_azules,
    name   = "Densidad poblacional (miles)",  # <<< CAMBIO
    na.value = "white",
    labels = function(x) fmt_punto(round(x, 1))
  ) +
  
  geom_text(data = h_df, aes(x = x, y = y, label = txt),
            color = "black", size = 2.6, fontface = "bold", lineheight = 0.95) +
  geom_text(data = m_df, aes(x = x, y = y, label = txt),
            color = "black", size = 2.6, fontface = "bold", lineheight = 0.95) +
  
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
  
  coord_sf(
    xlim = xlim, ylim = ylim,
    clip = "on",
    expand = FALSE,
    datum = st_crs(4326)
  ) +
  
  labs(
    title = "Medellín • Hombres y Mujeres por comuna",
    subtitle = "▲ Hombres  ★ Mujeres (valores en miles)",
    x = NULL, y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.8),
    axis.text  = element_text(size = 9, color = "grey30"),
    axis.ticks = element_line(color = "grey30"),
    axis.ticks.length = unit(0.12, "cm"),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.35),
    panel.grid.minor = element_line(color = "grey94", linewidth = 0.20),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

ggsave(
  "medellin_hombres_mujeres_por_comuna_calor.png",
  p,
  width = 12, height = 8,
  dpi = 300,
  bg = "white"
)
