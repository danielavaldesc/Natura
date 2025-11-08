######################################################
## Figura 1: Georreferenciación tiempo total MED    ##
## (Usando sólo el layer de Comunas de Medellín)
######################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(stringr)

# === Rutas de tus datos ===
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")
ruta_xlsx  <- "input_famd_med_29102025.xlsx"

# === 1) Datos: p40 (H/M), p19comuna (1..16), tiempo_total numérico ===
df <- read_excel(ruta_xlsx) %>%
  mutate(
    p40 = str_to_title(trimws(as.character(p40))),
    p40 = ifelse(p40 %in% c("Hombre","Mujer"), p40, NA_character_),
    p19comuna   = str_extract(as.character(p19comuna), "\\d+"),
    p19comuna   = suppressWarnings(as.integer(p19comuna)),
    tiempo_total = suppressWarnings(as.numeric(tiempo_total))
  ) %>%
  filter(!is.na(p40), !is.na(p19comuna), !is.na(tiempo_total))

# resumen por comuna y sexo (evita n() fuera de contexto usando dplyr::n())
agg <- df %>%
  group_by(p19comuna, p40) %>%
  summarise(
    n = dplyr::n(),
    mean_time = mean(tiempo_total, na.rm = TRUE),
    .groups = "drop"
  )

# === 2) Comunas de Medellín (solo Medellín) desde FeatureServer (layer 3) ===
# Fuente: Estaciones_mallavial_comunas_de_Medellin / Admin_Comunas (3)
# GeoJSON de consulta con todos los campos (4326)
url_comunas_geojson <- paste0(
  "https://services1.arcgis.com/Qrk4Z5vQ94JXkdYM/arcgis/rest/services/",
  "Estaciones_mallavial_comunas_de_Medellin/FeatureServer/3/query?",
  "where=1%3D1&outFields=*&outSR=4326&f=geojson"
)

shape_med <- st_read(url_comunas_geojson, quiet = TRUE)

# Detecta campo de comuna (casi siempre "COMUNA" o similar). Extrae dígitos 1..16
cand <- names(shape_med)[grepl("comun", names(shape_med), ignore.case = TRUE)]
if (length(cand) == 0) stop("No encontré un campo de 'comuna' en el layer de Medellín.")

# toma la primera candidata y parsea a entero
col_comuna <- cand[1]
shape_med <- shape_med %>%
  mutate(
    comuna_chr = as.character(.data[[col_comuna]]),
    comuna_num = suppressWarnings(as.integer(str_extract(comuna_chr, "\\d+")))
  ) %>%
  filter(!is.na(comuna_num), comuna_num %in% 1:16) %>%
  group_by(comuna_num) %>%                      # por si el layer trae sub-polígonos
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  arrange(comuna_num)

# === 3) Repetimos geometría para H/M y unimos promedios ===
sex_levels <- c("Hombre","Mujer")
shape_sex  <- shape_med[rep(1:nrow(shape_med), each = length(sex_levels)), ]
shape_sex$p40 <- factor(rep(sex_levels, times = nrow(shape_med)), levels = sex_levels)

shape_join <- shape_sex %>%
  left_join(agg, by = c("comuna_num" = "p19comuna", "p40" = "p40"))

# === 4) Mapa facetado con una sola escala de color (viridis) ===
lims <- range(shape_join$mean_time, na.rm = TRUE)

p <- ggplot(shape_join) +
  geom_sf(aes(fill = mean_time), color = "white", linewidth = 0.4) +
  geom_sf_text(aes(label = comuna_num), size = 3.8, fontface = "bold", color = "white") +
  scale_fill_viridis_c(
    name = "Tiempo promedio (min)",
    limits = lims,           # misma escala en H/M
    direction = -1,
    na.value = "grey90"
  ) +
  facet_wrap(~ p40, nrow = 1, drop = FALSE) +
  labs(
    title = "Medellín • Tiempo promedio de viaje (min) por comuna",
    subtitle = "Variable continua: tiempo_total • Facetas por sexo (p40)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

ggsave("medellin_tiempo_continuo_facet.png", p,
       width = 10, height = 6, dpi = 300, bg = "transparent")

# --- Mensajes útiles
cat("Campo usado como comuna en el layer:", col_comuna, "\n")
cat("Comunas del layer (1..16):", paste(sort(unique(shape_med$comuna_num)), collapse = ", "), "\n")
cat("Polígonos con datos después del join:", sum(!is.na(shape_join$mean_time)), "\n")


