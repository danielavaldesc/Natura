######################################################
## Figura 1: Georreferenciación tiempo total CALI   ##
######################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(stringr)

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
ruta_xlsx <- "input_famd_cali_29102025.xlsx"
ruta_shp  <- "mc_comunas.shp"

# 1) Datos
df <- readxl::read_excel(ruta_xlsx)

df <- df %>%
  mutate(
    # Normaliza sexo (por si viene con minúsculas, espacios, etc.)
    p40 = str_to_title(trimws(as.character(p40))),
    # Quédate con Hombre/Mujer
    p40 = ifelse(p40 %in% c("Hombre","Mujer"), p40, NA_character_),
    # Comunas a entero (si vinieran como "Comuna 1", extrae dígitos)
    p19comuna = as.character(p19comuna),
    p19comuna = str_extract(p19comuna, "\\d+"),
    p19comuna = suppressWarnings(as.integer(p19comuna)),
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

# 2) Shape de Cali
shape_cali <- sf::st_read(ruta_shp, quiet = TRUE)

# --- detectar columna de comuna en el SHP ---
cand <- names(shape_cali)
cand <- cand[grepl("comuna|cod_?comuna|id_?comuna", cand, ignore.case = TRUE)]
if (length(cand) == 0) stop("No encuentro una columna de comuna en el SHP. Renómbrala o dime cómo se llama.")

# si hay varias candidatas, toma la primera; puedes fijarla si sabes el nombre exacto
col_comuna <- cand[1]

# extrae dígitos y pásalo a entero para empatar con p19comuna
shape_cali <- shape_cali %>%
  mutate(
    comuna_join_chr = as.character(.data[[col_comuna]]),
    comuna_join_num = str_extract(comuna_join_chr, "\\d+"),
    comuna_join_num = suppressWarnings(as.integer(comuna_join_num))
  )

# 3) Join
shape_join <- shape_cali %>%
  left_join(agg, by = c("comuna_join_num" = "p19comuna"))

# Chequeos 
cat("Candidata de columna en SHP:", col_comuna, "\n")
cat("Comunas únicas en SHP (parseadas):", sort(unique(shape_join$comuna_join_num)), "\n")
cat("Comunas únicas en datos:", sort(unique(agg$p19comuna)), "\n")
cat("Filas con datos después del join:", sum(!is.na(shape_join$mean_time)), "\n")

# 4) Mapa facetado con una sola escala
lims <- range(shape_join$mean_time, na.rm = TRUE)
p <- ggplot(shape_join) +
  geom_sf(aes(fill = mean_time), color = NA) +
  scale_fill_viridis_c(
    name = "Mean time (min)",
    limits = lims,
    direction = -1,
    na.value = "grey90"
  ) +
  facet_wrap(~ p40, nrow = 1) +
  labs(
    title = "Cali • Tiempo promedio de viaje (min) por comuna",
    subtitle = "Variable continua: tiempo_total • Facet por sexo (p40)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

ggsave("cali_tiempo_continuo_facet.png", p, width = 10, height = 6, dpi = 300, bg = "transparent")


