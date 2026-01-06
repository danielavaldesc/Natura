###################################
## Figura 7: Mapa de calor CALI  ##
###################################

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
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
ruta_tabla   <- "Comunas.xlsx"
ruta_comunas <- "mc_comunas.shp"

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

to_prop_pct <- function(x){
  x <- as.character(x)
  x <- str_replace_all(x, "\\s+", "")
  x <- str_replace_all(x, "%", "")
  x <- str_replace_all(x, "\\.", "")
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))/100
}

# Formato corto en miles (1 decimal)
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

col_pob <- if ("Población" %in% names(tab)) "Población" else if ("Poblacion" %in% names(tab)) "Poblacion" else NA
if (is.na(col_pob)) stop("No encuentro la columna de población (Población / Poblacion) en Comunas.xlsx")

tab2 <- tab %>%
  transmute(
    Comuna    = suppressWarnings(as.integer(Comuna)),
    poblacion = to_num_personas(.data[[col_pob]]),
    pct_h     = to_prop_pct(Hombres),
    pct_m     = to_prop_pct(Mujeres)
  ) %>%
  filter(!is.na(Comuna), !is.na(poblacion), !is.na(pct_h), !is.na(pct_m)) %>%
  mutate(
    hombres = round(poblacion * pct_h),
    mujeres = round(poblacion * pct_m),
    poblacion_miles = poblacion / 1000
  )

# ==========================================================
# 3) SHAPE + JOIN
# ==========================================================
shape_cali <- st_read(ruta_comunas, quiet = TRUE)

col_comuna <- names(shape_cali)[grepl("comuna", names(shape_cali), ignore.case = TRUE)][1]
if (is.na(col_comuna)) stop("No se detectó columna comuna en el SHP de comunas.")

shape_cali <- shape_cali %>%
  mutate(Comuna = suppressWarnings(as.integer(gsub("\\D","", as.character(.data[[col_comuna]]))))) %>%
  left_join(tab2, by = "Comuna")

crs_src <- st_crs(shape_cali)
if (is.na(crs_src)) stop("El shapefile de comunas no tiene CRS.")
shape_cali <- st_transform(shape_cali, 4326)

# ==========================================================
# 4) PUNTOS (H y M) — EN MILES + CENTRADOS
# ==========================================================
pts <- st_point_on_surface(shape_cali)
xy  <- st_coordinates(pts)

centros <- pts %>%
  st_drop_geometry() %>%
  mutate(x = xy[,1], y = xy[,2]) %>%
  dplyr::select(Comuna, x, y, hombres, mujeres, poblacion) %>%
  filter(!is.na(hombres), !is.na(mujeres), !is.na(poblacion)) %>%
  mutate(
    txt_h = paste0("\u25B2 ", fmt_miles(hombres / 1000, 1)), # ▲
    txt_m = paste0("\u2605 ", fmt_miles(mujeres / 1000, 1))  # ★
  )

# separación
dy <- 0.0020
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
# 6) BBOX (MUCHO más aire para evitar “recortes” en bordes)
# ==========================================================
bb <- st_bbox(shape_cali)

# subo padding para que el texto jamás quede pegado al panel
xpad <- 0.08 * (bb$xmax - bb$xmin)
ypad <- 0.08 * (bb$ymax - bb$ymin)

xlim <- c(bb$xmin - xpad, bb$xmax + xpad)
ylim <- c(bb$ymin - ypad, bb$ymax + ypad)

# ==========================================================
# 7) MAPA FINAL (estilo Medellín, sin cajas)
# ==========================================================
p <- ggplot() +
  geom_sf(
    data = shape_cali,
    aes(fill = poblacion_miles),
    color = "grey40",
    linewidth = 0.25
  ) +
  scale_fill_gradientn(
    colors = pal_azules,
    name   = "Densidad poblacional (miles)",
    na.value = "white",
    labels = function(x) fmt_miles(x, 1)
  ) +
  
  # Texto Hombres / Mujeres (negro, más grande, centrado)
  geom_text(
    data = h_df,
    aes(x = x, y = y, label = txt),
    color = "black",
    size = 3.5,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0.5,
    lineheight = 0.95
  ) +
  geom_text(
    data = m_df,
    aes(x = x, y = y, label = txt),
    color = "black",
    size = 3.5,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0.5,
    lineheight = 0.95
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
  
  coord_sf(
    xlim = xlim, ylim = ylim,
    clip = "off",     # clave: que el panel NO recorte texto
    expand = FALSE
  ) +
  
  labs(
    title = "Cali • Hombres y Mujeres por comuna",
    subtitle = "▲ Hombres  ★ Mujeres (valores en miles)",
    x = NULL, y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.35),
    panel.grid.minor = element_line(color = "grey94", linewidth = 0.20),
    axis.text  = element_text(size = 9, color = "grey30"),
    axis.ticks = element_line(color = "grey30"),
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    
    # más margen externo para evitar recorte por el borde del plot
    plot.margin = margin(15, 55, 15, 15)
  )

# ==========================================================
# 8) EXPORTAR
# ==========================================================
ggsave(
  "cali_hombres_mujeres_por_comuna.png",
  p,
  width = 12, height = 8,
  dpi = 300,
  bg = "white"
)

