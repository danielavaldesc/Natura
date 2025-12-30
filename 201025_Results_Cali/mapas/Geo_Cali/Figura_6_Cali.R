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

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
ruta_tabla   <- "Comunas.xlsx"
ruta_comunas <- "mc_comunas.shp"

# ---------- helpers ----------
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

fmt_punto <- function(x){
  format(x, big.mark=".", decimal.mark=",", scientific=FALSE, trim=TRUE)
}

# ---------- tabla ----------
tab <- readxl::read_excel(ruta_tabla)

# OJO: tu excel a veces dice Población o Poblacion
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
    mujeres = round(poblacion * pct_m)
  )

# ---------- shape ----------
shape_cali <- st_read(ruta_comunas, quiet = TRUE)
col_comuna <- names(shape_cali)[grepl("comuna", names(shape_cali), ignore.case = TRUE)][1]
if (is.na(col_comuna)) stop("No se detectó columna comuna en el SHP de comunas.")

shape_cali <- shape_cali %>%
  mutate(Comuna = suppressWarnings(as.integer(gsub("\\D","", as.character(.data[[col_comuna]]))))) %>%
  left_join(tab2, by = "Comuna")

crs_src <- st_crs(shape_cali)
if (is.na(crs_src)) stop("El shapefile de comunas no tiene CRS.")
shape_cali <- st_transform(shape_cali, 4326)

# ---------- puntos dentro de cada comuna ----------
pts <- st_point_on_surface(shape_cali)
xy  <- st_coordinates(pts)

centros <- pts %>%
  st_drop_geometry() %>%
  mutate(x = xy[,1], y = xy[,2]) %>%
  dplyr::select(Comuna, x, y, hombres, mujeres, poblacion) %>%
  filter(!is.na(hombres), !is.na(mujeres), !is.na(poblacion)) %>%
  mutate(
    txt_h = paste0("\u25B2 ", fmt_punto(hombres)), # ▲
    txt_m = paste0("\u2605 ", fmt_punto(mujeres))  # ★
  )

# offset chiquito para separar H y M dentro de la comuna
dy <- 0.0022
h_df <- centros %>% transmute(x, y = y + dy, txt = txt_h)
m_df <- centros %>% transmute(x, y = y - dy, txt = txt_m)

# ---------- paleta calor ----------
pal_calor <- c("#FEE8C8", "#FDBB84", "#FC8D59", "#E34A33", "#B30000")

bb <- st_bbox(shape_cali)
xpad <- 0.02 * as.numeric(bb["xmax"] - bb["xmin"])
ypad <- 0.02 * as.numeric(bb["ymax"] - bb["ymin"])
xlim <- c(as.numeric(bb["xmin"]) - xpad, as.numeric(bb["xmax"]) + xpad)
ylim <- c(as.numeric(bb["ymin"]) - ypad, as.numeric(bb["ymax"]) + ypad)

# ==========================================================
# MAPA (sin cajas blancas; símbolo a la izquierda del número)
# ==========================================================
p <- ggplot() +
  geom_sf(
    data = shape_cali,
    aes(fill = poblacion),
    color = "grey40",
    linewidth = 0.25
  ) +
  scale_fill_gradientn(
    colors = pal_calor,
    name = "Densidad poblacional",
    na.value = "white"
  ) +
  
  # Texto Hombres / Mujeres (solo negro)
  geom_text(
    data = h_df,
    aes(x = x, y = y, label = txt),
    color = "black",
    size = 2.5,
    fontface = "bold",
    lineheight = 0.95
  ) +
  geom_text(
    data = m_df,
    aes(x = x, y = y, label = txt),
    color = "black",
    size = 2.5,
    fontface = "bold",
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
  
  coord_sf(xlim = xlim, ylim = ylim, clip = "on") +
  labs(
    title = "Cali • Hombres y Mujeres por comuna",
    subtitle = "▲ Hombres  ★ Mujeres  (valores en miles)",
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
    plot.title = element_text(face = "bold")
  )

ggsave("cali_hombres_mujeres_por_comuna.png", p, width = 12, height = 8, dpi = 300, bg = "white")

