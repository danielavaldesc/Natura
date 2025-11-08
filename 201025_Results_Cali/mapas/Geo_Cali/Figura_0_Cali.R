######################################################
## Figura 1: Georreferenciación tiempo total CALI   ##
######################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
ruta_xlsx <- "input_famd_cali_29102025.xlsx"
ruta_shp  <- "mc_comunas.shp"

# 1) Datos
df <- readxl::read_excel(ruta_xlsx)

df <- df %>%
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
    n = dplyr::n(),
    mean_time = mean(tiempo_total, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Shape de Cali
shape_cali <- sf::st_read(ruta_shp, quiet = TRUE)

# detectar columna de comuna en el SHP
cand <- names(shape_cali)
cand <- cand[grepl("comuna|cod_?comuna|id_?comuna", cand, ignore.case = TRUE)]
if (length(cand) == 0) stop("No encuentro una columna de comuna en el SHP.")
col_comuna <- cand[1]

# extraer dígitos para empatar con p19comuna
shape_cali <- shape_cali %>%
  mutate(
    comuna_join_chr = as.character(.data[[col_comuna]]),
    comuna_join_num = str_extract(comuna_join_chr, "\\d+"),
    comuna_join_num = suppressWarnings(as.integer(comuna_join_num))
  )

# 3) Join
shape_join <- shape_cali %>%
  left_join(agg, by = c("comuna_join_num" = "p19comuna"))

# 4) Mapa facetado (grises suaves)
lims <- range(shape_join$mean_time, na.rm = TRUE)

p <- ggplot() +
  # capa base tenue para todo el polígono (fondo claro)
  geom_sf(data = shape_cali, fill = "#F7F7F7", color = "#D0D0D0", linewidth = 0.35) +
  # choropleth por tiempo (mismo tono, distintas intensidades)
  geom_sf(data = shape_join, aes(fill = mean_time), color = "white", linewidth = 0.35) +
  scale_fill_gradient(
    name = "Tiempo promedio (min)",
    low  = "#ECECEC",   # gris muy claro
    high = "#4A4A4A",   # gris medio-oscuro
    limits = lims,
    na.value = "#EEEEEE"
  ) +
  facet_wrap(~ p40, nrow = 1) +
  labs(
    title = "Cali • Tiempo promedio de viaje (min) por comuna",
    subtitle = "Variable continua: tiempo_total • Facetas por sexo (p40)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    panel.grid.major  = element_line(color = "#E8E8E8", linewidth = 0.2),
    panel.grid.minor  = element_blank(),
    axis.title        = element_blank(),
    axis.text         = element_text(color = "#7A7A7A"),
    legend.position   = "right",
    legend.title      = element_text(color = "#404040"),
    legend.text       = element_text(color = "#4F4F4F")
  )

ggsave("cali_tiempo_continuo_facet.png", p, width = 10, height = 6, dpi = 300, bg = "white")

