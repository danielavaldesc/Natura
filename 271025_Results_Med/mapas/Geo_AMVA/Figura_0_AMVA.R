##########################################################
## Figura 1: Georreferenciación tiempo total MEDELLÍN   ##
##########################################################
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(stringr)
library(stringi)

# Archivos 
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")
ruta_xlsx <- "input_famd_med_29102025.xlsx"
ruta_shp  <- "ZONAS SIT.shp" 

# === 1) Datos (solo Hombre/Mujer; comunas 1..16) ===
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
  summarise(n = n(), mean_time = mean(tiempo_total, na.rm = TRUE), .groups = "drop")

# === 2) Leer SHP y FILTRAR MUNICIPIO = MEDELLÍN ===
raw <- sf::st_read(ruta_shp, quiet = TRUE)

# detectar columna de municipio (MUNICIPIO, NOM_MPIO, MPIO, CIUDAD, etc.)
cand_mpio <- names(raw)[grepl("muni|mpio|municip|ciud", names(raw), ignore.case = TRUE)]
if (length(cand_mpio) == 0) stop("No encontré columna de municipio en el SHP del AMVA.")

# normalizar (quitar tildes y pasar a mayúsculas)
norm_txt <- function(x) toupper(str_trim(stri_trans_general(as.character(x), "Latin-ASCII")))
mpio_col <- cand_mpio[1]
raw$MPIO_NORM <- norm_txt(raw[[mpio_col]])

# quedarnos SOLO con MEDELLIN
raw_med <- raw %>% filter(MPIO_NORM == "MEDELLÍN")
if (nrow(raw_med) == 0) stop("El filtro por municipio no encontró 'MEDELLIN'. Revisa el nombre exacto del campo/valor.")

# === 3) Elegir la MEJOR columna de “comuna” dentro de MEDELLÍN ===
cands <- names(raw_med)[grepl("comuna|cod_?comuna|id_?comuna|zona|macro|nueva_zona", names(raw_med), ignore.case = TRUE)]

# función de puntaje: +intersección con 1..16 y –0.01*#unicos (penaliza cientos)
score_col <- function(col) {
  vnum <- suppressWarnings(as.integer(str_extract(as.character(raw_med[[col]]), "\\d+")))
  inter <- length(intersect(1:16, unique(na.omit(vnum))))
  uniq  <- length(unique(na.omit(vnum)))
  inter - 0.01 * uniq
}

scores <- sapply(cands, score_col)
col_comuna <- cands[which.max(scores)]

# === 4) Parsear número de comuna, DISOLVER a 16 comunas ===
shape_diss <- raw_med %>%
  mutate(
    comuna_num = suppressWarnings(as.integer(str_extract(as.character(.data[[col_comuna]]), "\\d+")))
  ) %>%
  filter(!is.na(comuna_num), comuna_num %in% 1:16) %>%
  group_by(comuna_num) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  arrange(comuna_num)

# === 5) Replicar para H/M sin perder geometría y unir promedios ===
sex_levels <- c("Hombre","Mujer")
shape_sex <- shape_diss[rep(1:nrow(shape_diss), each = length(sex_levels)), ]
shape_sex$p40 <- factor(rep(sex_levels, times = nrow(shape_diss)), levels = sex_levels)

shape_join <- shape_sex %>%
  left_join(agg, by = c("comuna_num" = "p19comuna", "p40" = "p40"))

# Chequeo
cat("Columna municipio usada:", mpio_col, "\n")
cat("Columna de comuna elegida:", col_comuna, "\n")
cat("Comunas en SHP (ya filtrado a Medellín):", sort(unique(shape_diss$comuna_num)), "\n")
cat("Matches:", sum(!is.na(shape_join$mean_time)), "polígonos-sexo\n")

# === 6) Mapa facetado (misma escala de color) ===
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
