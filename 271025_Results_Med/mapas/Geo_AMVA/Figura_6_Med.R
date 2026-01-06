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

fmt_miles <- function(x, dec = 1){
  format(
    round(x, dec),
    decimal.mark = ",",
    big.mark = ".",
    scientific = FALSE,
    trim = TRUE,
    nsmall = dec
  )
}

# ==========================================================
# 2) TABLA
# ==========================================================
tab <- read_excel(ruta_tabla)

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
  mutate(total_miles = total / 1000)

# ==========================================================
# 4) PUNTOS 
# ==========================================================
pts <- st_point_on_surface(shape_comunas)
xy  <- st_coordinates(pts)

centros <- pts %>%
  st_drop_geometry() %>%
  mutate(x = xy[,1], y = xy[,2]) %>%
  dplyr::select(Comuna, x, y, hombres, mujeres, total) %>%
  filter(!is.na(hombres), !is.na(mujeres)) %>%
  mutate(
    txt_h = paste0("\u25B2 ", fmt_miles(hombres / 1000, 1)),
    txt_m = paste0("\u2605 ", fmt_miles(mujeres / 1000, 1))
  )


dy <- 0.0012

h_df <- centros %>% transmute(x, y = y + dy, txt = txt_h)
m_df <- centros %>% transmute(x, y = y - dy, txt = txt_m)

# ==========================================================
# 5) PALETA AZULES
# ==========================================================
pal_azules <- c(
  "#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1",
  "#6BAED6","#4292C6","#2171B5","#08519C","#08306B"
)

# ==========================================================
# 6) BBOX
# ==========================================================
bb <- st_bbox(shape_comunas)
xpad <- 0.02 * (bb$xmax - bb$xmin)
ypad <- 0.02 * (bb$ymax - bb$ymin)

# ==========================================================
# 7) MAPA FINAL
# ==========================================================
p <- ggplot() +
  geom_sf(
    data = shape_comunas,
    aes(fill = total_miles),
    color = "grey40",
    linewidth = 0.3
  ) +
  scale_fill_gradientn(
    colors = pal_azules,
    name   = "Densidad poblacional (miles)",
    labels = function(x) fmt_miles(x, 1),
    na.value = "white"
  ) +
  geom_text(
    data = h_df,
    aes(x = x, y = y, label = txt),
    size = 3.4,
    fontface = "bold",
    color = "black",
    hjust = 0.5,
    vjust = 0.5
  ) +
  geom_text(
    data = m_df,
    aes(x = x, y = y, label = txt),
    size = 3.4,
    fontface = "bold",
    color = "black",
    hjust = 0.5,
    vjust = 0.5
  ) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.1, "cm"),
    width  = unit(1.1, "cm")
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(1.6, "cm")
  ) +
  coord_sf(
    xlim = c(bb$xmin - xpad, bb$xmax + xpad),
    ylim = c(bb$ymin - ypad, bb$ymax + ypad),
    expand = FALSE
  ) +
  labs(
    title = "Medellín • Hombres y Mujeres por comuna",
    subtitle = "▲ Hombres  ★ Mujeres (valores en miles)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.8),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

# ==========================================================
# 8) EXPORTAR
# ==========================================================
ggsave(
  "medellin_hombres_mujeres_por_comuna_calor.png",
  p,
  width = 12, height = 8, dpi = 300, bg = "white"
)
