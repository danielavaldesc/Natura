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
    comuna_join_num = str_extract(comuna_join_chr, "\\d+"),
    comuna_join_num = suppressWarnings(as.integer(comuna_join_num))
  )

# ============ 3) Join ============
shape_join <- shape_cali %>%
  left_join(agg, by = c("comuna_join_num" = "p19comuna"))

# ============ 4) Parámetros visuales ============
lims <- range(shape_join$mean_time, na.rm = TRUE)
mid  <- mean(shape_join$mean_time, na.rm = TRUE)
compass_brown <- "#6F3E2B"

# === Brújula con FLECHAS en ambos extremos ===
arrow_compass <- function(color = "#6F3E2B", txt = 0.85, lwd = 1.8, alen = 0.08){
  grobTree(
    # brazos con puntas (ends = "both")
    segmentsGrob(x0 = 0.5, y0 = 0.20, x1 = 0.5, y1 = 0.80,
                 gp = gpar(col = color, lwd = lwd),
                 arrow = arrow(type = "closed", ends = "both",
                               length = unit(alen, "npc"))),
    segmentsGrob(x0 = 0.20, y0 = 0.5, x1 = 0.80, y1 = 0.5,
                 gp = gpar(col = color, lwd = lwd),
                 arrow = arrow(type = "closed", ends = "both",
                               length = unit(alen, "npc"))),
    # letras (ligeramente dentro para que no se recorten)
    textGrob("N", x = 0.50, y = 0.96, gp = gpar(col = color, cex = txt, fontface = "bold")),
    textGrob("S", x = 0.50, y = 0.04, gp = gpar(col = color, cex = txt, fontface = "bold")),
    textGrob("E", x = 0.96, y = 0.50, gp = gpar(col = color, cex = txt, fontface = "bold")),
    textGrob("W", x = 0.04, y = 0.50, gp = gpar(col = color, cex = txt, fontface = "bold"))
  )
}

# ============ 5) Colocación de la brújula ============
bb <- sf::st_bbox(shape_join)
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])
bxmin <- as.numeric(bb["xmin"]) + 0.82 * xspan
bxmax <- as.numeric(bb["xmin"]) + 0.92 * xspan
bymin <- as.numeric(bb["ymin"]) + 0.78 * yspan
bymax <- as.numeric(bb["ymin"]) + 0.94 * yspan

# ============ 6) Plot ============
p <- ggplot(shape_join) +
  geom_sf(aes(fill = mean_time), color = "grey70", linewidth = 0.25) +
  scale_fill_gradient2(
    name = "Tiempo promedio (min)",
    limits = lims, midpoint = mid,
    low = "#2E7D32", mid = "#F4D03F", high = "#C62828",
    na.value = "grey90",
    guide = guide_colorbar(barheight = grid::unit(60, "pt"))
  ) +
  facet_wrap(~ p40, nrow = 1) +
  labs(title = "Cali • Tiempo promedio de viaje (min) por comuna") +
  coord_sf(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank(),   # sin números de coordenadas
    axis.ticks       = element_blank(),   # sin ticks
    legend.position  = "right",
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text       = element_text(colour = "grey20", face = "bold")
  ) +
  annotation_custom(
    grob = arrow_compass(color = compass_brown, txt = 0.80, lwd = 1.6, alen = 0.08),
    xmin = bxmin, xmax = bxmax, ymin = bymin, ymax = bymax
  )

ggsave("cali_tiempo_continuo_facet.png", p,
       width = 10, height = 6, dpi = 300, bg = "white")



