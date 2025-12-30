######################################################
## Figura 1: Georreferenciación tiempo total CALI   ##
######################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(ggspatial)
library(grid)

# Evitar choques
if ("package:plyr" %in% search()) detach("package:plyr", unload = TRUE)
if ("package:tidytable" %in% search()) detach("package:tidytable", unload = TRUE)

# Rutas
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
ruta_xlsx <- "input_famd_cali_29102025.xlsx"
ruta_shp  <- "mc_comunas.shp"

ruta_shp_terminales <- "terminales\\terminales.shp"
ruta_shp_paradas    <- "Estaciones_de_Parada_2025\\Estaciones_de_Parada_2025.shp"

# ============ 1) Datos ============
df <- readxl::read_excel(ruta_xlsx) %>%
  mutate(
    p40 = str_to_title(trimws(as.character(p40))),
    p40 = ifelse(p40 %in% c("Hombre","Mujer"), p40, NA_character_),
    p19comuna = str_extract(as.character(p19comuna), "\\d+"),
    p19comuna = suppressWarnings(as.integer(p19comuna)),
    tiempo_total = suppressWarnings(as.numeric(tiempo_total))
  ) %>%
  filter(!is.na(p40), !is.na(p19comuna), !is.na(tiempo_total))

agg <- df %>%
  group_by(p19comuna, p40) %>%
  summarise(
    n         = length(tiempo_total),
    mean_time = mean(tiempo_total, na.rm = TRUE),
    .groups   = "drop"
  )

# ============ 2) Shape de Cali ============
shape_cali <- sf::st_read(ruta_shp, quiet = TRUE)

cand <- names(shape_cali)
cand <- cand[grepl("comuna|cod_?comuna|id_?comuna", cand, ignore.case = TRUE)]
if (length(cand) == 0) stop("No encuentro una columna de comuna en el SHP.")
col_comuna <- cand[1]

shape_cali <- shape_cali %>%
  mutate(
    comuna_join_chr = as.character(.data[[col_comuna]]),
    comuna_join_num = suppressWarnings(as.integer(str_extract(comuna_join_chr, "\\d+")))
  )

# ============ 3) Join ============
shape_join <- shape_cali %>%
  left_join(agg, by = c("comuna_join_num" = "p19comuna"))

# ============ 4) Leer paradas y terminales ============
terminales <- sf::st_read(ruta_shp_terminales, quiet = TRUE)
paradas    <- sf::st_read(ruta_shp_paradas, quiet = TRUE)

# ============ 5) CRS → WGS84 (grados) para TODO ============
crs_src <- sf::st_crs(shape_join)
if (is.na(crs_src)) stop("El shapefile de comunas no tiene CRS.")

if (is.na(sf::st_crs(paradas)))    sf::st_crs(paradas)    <- crs_src
if (is.na(sf::st_crs(terminales))) sf::st_crs(terminales) <- crs_src

shape_join <- sf::st_transform(shape_join, 4326)
paradas    <- sf::st_transform(paradas, 4326)
terminales <- sf::st_transform(terminales, 4326)

# ---- Unir MIO en un solo sf para controlar SHAPE ----
paradas$tipo    <- "Paradas"
terminales$tipo <- "Terminales"
mio <- dplyr::bind_rows(paradas, terminales) %>%
  mutate(tipo = factor(tipo, levels = c("Paradas","Terminales")))

azul_paradas    <- "grey"
azul_terminales <- "#000000"

# ============ 6) Parámetros visuales ============
lims <- range(shape_join$mean_time, na.rm = TRUE)
mid  <- mean(shape_join$mean_time, na.rm = TRUE)

# ============ 7) Plot ============
p <- ggplot(shape_join) +
  geom_sf(aes(fill = mean_time), color = "grey70", linewidth = 0.25) +
  scale_fill_gradient2(
    name = "Tiempo promedio (min)",
    limits = lims, midpoint = mid,
    low = "#2E7D32", mid = "#F4D03F", high = "#C62828",
    na.value = "grey90",
    guide = guide_colorbar(barheight = grid::unit(60, "pt"))
  ) +
  
  # ---- Paradas/Terminales (aquí ya sale el triángulo) ----
geom_sf(
  data = mio,
  aes(shape = tipo, color = tipo),
  size = 2.0,
  alpha = 0.95,
  stroke = 0.6
) +
  scale_shape_manual(
    values = c("Paradas" = 16, "Terminales" = 17),
    breaks = c("Paradas","Terminales"),
    labels = c("Paradas MIO","Terminales MIO"),
    name = NULL
  ) +
  scale_color_manual(
    values = c("Paradas" = azul_paradas, "Terminales" = azul_terminales),
    breaks = c("Paradas","Terminales"),
    labels = c("Paradas MIO","Terminales MIO"),
    name = NULL
  ) +
  guides(shape = guide_legend(order = 1), color = "none") +

  facet_wrap(~ p40, nrow = 1) +
  labs(title = "Cali • Tiempo promedio de viaje (min) por comuna") +
  
  coord_sf(clip = "on") +
  
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
  
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.35),
    panel.grid.minor = element_line(color = "grey94", linewidth = 0.20),
    axis.title       = element_blank(),
    axis.text        = element_text(size = 9, color = "grey20"),
    axis.ticks       = element_line(color = "grey20"),
    legend.position  = "right",
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text       = element_text(colour = "grey20", face = "bold")
  )

ggsave(
  "cali_tiempo_continuo.png",
  p,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
