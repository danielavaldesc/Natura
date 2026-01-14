###################################
## Figura 7: Mapa de calor CALI  ##
###################################

library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(units)

# ==========================================================
# 0) RUTAS
# ==========================================================
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
ruta_tabla   <- "Comunas.xlsx"
ruta_comunas <- "mc_comunas.shp"

# ==========================================================
# 1) FORMATO NUMÉRICO
# ==========================================================
fmt_miles <- function(x, dec = 1){
  format(
    round(x, dec),
    decimal.mark = ",",
    big.mark = ".",
    scientific = FALSE,
    nsmall = dec
  )
}

# ==========================================================
# 2) LEER TABLA (YA LIMPIA)
# ==========================================================
tab <- read_excel(ruta_tabla)

tab2 <- tab %>%
  transmute(
    Comuna    = as.integer(Comuna),
    poblacion = as.numeric(Poblacion),
    pct_h     = as.numeric(Hombres),
    pct_m     = as.numeric(Mujeres)
  ) %>%
  mutate(
    hombres = round(poblacion * pct_h),
    mujeres = poblacion - hombres,        
    poblacion_miles = poblacion / 1000
  )

# ==========================================================
# 3) SHAPE + JOIN
# ==========================================================
shape_cali <- st_read(ruta_comunas, quiet = TRUE)

# detectar columna comuna en el SHP
col_comuna <- names(shape_cali)[
  grepl("comuna", names(shape_cali), ignore.case = TRUE)
][1]

if (is.na(col_comuna)) {
  stop("❌ No se encontró columna de comuna en el shapefile")
}

shape_cali <- shape_cali %>%
  mutate(
    Comuna = as.integer(gsub("\\D", "", as.character(.data[[col_comuna]])))
  ) %>%
  left_join(tab2, by = "Comuna") %>%
  st_transform(4326)

# ==========================================================
# 4) CENTROIDES PARA TEXTO
# ==========================================================
pts <- st_point_on_surface(shape_cali)
xy  <- st_coordinates(pts)

centros <- pts %>%
  st_drop_geometry() %>%
  mutate(
    x = xy[,1],
    y = xy[,2],
    txt_h = paste0("▲ ", fmt_miles(hombres / 1000, 1)),
    txt_m = paste0("★ ", fmt_miles(mujeres / 1000, 1))
  ) %>%
  filter(!is.na(hombres), !is.na(mujeres))

dy <- 0.002
h_df <- centros %>% transmute(x, y = y + dy, txt = txt_h)
m_df <- centros %>% transmute(x, y = y - dy, txt = txt_m)

# ==========================================================
# 5) BBOX
# ==========================================================
bb <- st_bbox(shape_cali)
xpad <- 0.08 * (bb$xmax - bb$xmin)
ypad <- 0.08 * (bb$ymax - bb$ymin)

# ==========================================================
# 6) MAPA
# ==========================================================
p <- ggplot() +
  geom_sf(
    data = shape_cali,
    aes(fill = poblacion_miles),
    color = "grey40",
    linewidth = 0.25
  ) +
  scale_fill_gradientn(
    colors = c(
      "#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1",
      "#6BAED6","#4292C6","#2171B5","#08519C","#08306B"
    ),
    name = "Densidad poblacional (miles)",
    labels = function(x) fmt_miles(x, 1),
    na.value = "white"
  ) +
  geom_text(
    data = h_df,
    aes(x = x, y = y, label = txt),
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(
    data = m_df,
    aes(x = x, y = y, label = txt),
    size = 3.5,
    fontface = "bold"
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
    width_hint = 0.25
  ) +
  coord_sf(
    xlim = c(bb$xmin - xpad, bb$xmax + xpad),
    ylim = c(bb$ymin - ypad, bb$ymax + ypad),
    clip = "off"
  ) +
  labs(
    title = "Cali • Hombres y Mujeres por comuna",
    subtitle = "▲ Hombres  ★ Mujeres (valores en miles)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "grey20", fill = NA),
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.margin = margin(15, 55, 15, 15)
  )

# ==========================================================
# 7) GUARDAR GRÁFICA
# ==========================================================
ggsave(
  filename = "cali_hombres_mujeres_por_comuna.png",
  plot = p,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

