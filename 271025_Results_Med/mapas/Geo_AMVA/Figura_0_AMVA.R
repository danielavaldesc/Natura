##########################################################
## Figura 1: Georreferenciación tiempo total MEDELLÍN   ##
##########################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(stringr)

# Archivos 
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")
ruta_xlsx <- "input_famd_med_29102025.xlsx"
ruta_shp  <- "ZONAS SIT.shp"  

# 1) Datos (solo Hombre/Mujer; comunas enteras 1..16)
df <- readxl::read_excel(ruta_xlsx) %>%
  mutate(
    p40 = str_to_title(trimws(as.character(p40))),
    p40 = ifelse(p40 %in% c("Hombre","Mujer"), p40, NA_character_),
    p19comuna   = str_extract(as.character(p19comuna), "\\d+"),
    p19comuna   = suppressWarnings(as.integer(p19comuna)),
    tiempo_total = suppressWarnings(as.numeric(tiempo_total))
  ) %>%
  filter(!is.na(p40), !is.na(p19comuna), !is.na(tiempo_total))

agg <- df %>%
  group_by(p19comuna, p40) %>%
  summarise(
    n = n(),
    mean_time = mean(tiempo_total, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Leer shape y detectar columna de comuna
shape_raw <- sf::st_read(ruta_shp, quiet = TRUE)
cands <- names(shape_raw)
cands <- cands[grepl("comuna|cod_?comuna|id_?comuna|zona|macro|nueva_zona",
                     cands, ignore.case = TRUE)]
stopifnot(length(cands) > 0)
col_comuna <- cands[1]

# 3) Parsear número de comuna y DISOLVER a 16 polígonos
shape_diss <- shape_raw %>%
  mutate(
    comuna_chr = as.character(.data[[col_comuna]]),
    comuna_num = suppressWarnings(as.integer(stringr::str_extract(comuna_chr, "\\d+")))
  ) %>%
  filter(!is.na(comuna_num)) %>%
  # si el shapefile tiene muchas zonas, esto las “disuelve” por comuna
  group_by(comuna_num) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  # nos quedamos con las comunas 1..16 (Medellín)
  filter(comuna_num %in% 1:16) %>%
  arrange(comuna_num)

# 4) Duplicar el sf para Hombre/Mujer SIN perder geometría
sex_levels <- c("Hombre","Mujer")
shape_sex <- shape_diss[rep(1:nrow(shape_diss), each = length(sex_levels)), ]
shape_sex$p40 <- factor(rep(sex_levels, times = nrow(shape_diss)),
                        levels = sex_levels)

# 5) Join por comuna + sexo
shape_join <- shape_sex %>%
  left_join(agg, by = c("comuna_num" = "p19comuna", "p40" = "p40"))

# Chequeo en consola
cat("Columna detectada en SHP:", col_comuna, "\n")
cat("Comunas datos: ", sort(unique(agg$p19comuna)), "\n")
cat("Comunas shp:   ", sort(unique(shape_diss$comuna_num)), "\n")
cat("Matches:       ", sum(!is.na(shape_join$mean_time)), "polígonos-sexo\n")

# 6) Mapa facetado (misma escala de color)
lims <- range(shape_join$mean_time, na.rm = TRUE)

p <- ggplot(shape_join) +
  geom_sf(aes(fill = mean_time), color = NA) +
  scale_fill_viridis_c(
    name = "Tiempo promedio (min)",
    limits = lims,
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

ggsave("medellin_tiempo_continuo_facet.png", p, width = 10, height = 6, dpi = 300, bg = "transparent")