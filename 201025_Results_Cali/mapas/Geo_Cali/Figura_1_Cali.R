###########################################################
## Figura 2: Georreferenciación de elección modal CALI   ##
###########################################################

# Paquetes
library(readxl)
library(ggplot2)
library(dplyr)
library(sf)
library(stringr)
library(scatterpie)
library(ggnewscale)
library(grid)

# -------------------------------------------------------------------
# 1) Datos
# -------------------------------------------------------------------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
dataset <- readxl::read_excel("input_famd_cali_29102025.xlsx")

dataset$id      <- as.character(dataset$id)
dataset$medio   <- as.character(dataset$medio)
dataset$Comuna  <- as.integer(gsub("\\D", "", as.character(dataset$p19comuna)))
data <- dataset

# -------------------------------------------------------------------
# Estrato predominante por comuna (Bajo/Medio/Alto)
# -------------------------------------------------------------------
nombre_estrato <- if ("p9_estrato3" %in% names(data)) "p9_estrato3" else
  if ("p9_estratro3" %in% names(data)) "p9_estratro3" else NA
if (is.na(nombre_estrato)) stop("No se encontró la columna de estrato (p9_estrato3 / p9_estratro3).")

tmp <- data.frame(
  Comuna = data$Comuna,
  estrato_cat = trimws(tolower(as.character(data[[nombre_estrato]]))),
  stringsAsFactors = FALSE
)
tmp$estrato_cat[tmp$estrato_cat %in% c("alto","alta")]   <- "Alto"
tmp$estrato_cat[tmp$estrato_cat %in% c("medio","media")] <- "Medio"
tmp$estrato_cat[tmp$estrato_cat %in% c("bajo","baja")]   <- "Bajo"
tmp <- tmp[!is.na(tmp$estrato_cat) & tmp$estrato_cat != "", ]

niveles <- c("Bajo","Medio","Alto")
tmp$estrato_cat <- factor(tmp$estrato_cat, levels = niveles, ordered = TRUE)

estratos_comuna <- tmp %>%
  group_by(Comuna, estrato_cat) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  arrange(desc(n), estrato_cat) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(Comuna, categoria = as.character(estrato_cat))

# -------------------------------------------------------------------
# 2) Zonas por comuna
# -------------------------------------------------------------------
data$zona <- NA_character_
for (k in 1:nrow(data)) {
  if (data$Comuna[k] %in% c(1, 2, 3, 9))              data$zona[k] <- "Noroccidente"
  if (data$Comuna[k] %in% c(4, 5, 6, 7, 8))           data$zona[k] <- "Nororiente"
  if (data$Comuna[k] %in% c(11,12,13,14,15,16,21))    data$zona[k] <- "Oriente-aguablanca"
  if (data$Comuna[k] %in% c(10,17,18,19,20,22))       data$zona[k] <- "Sur"
}
data <- data[!is.na(data$zona), ]
data$zona <- factor(data$zona,
                    levels = c("Noroccidente","Nororiente","Oriente-aguablanca","Sur"))

# -------------------------------------------------------------------
# 3) Conteos por zona/medio + coordenadas de los pies
# -------------------------------------------------------------------
table_data_mode <- as.data.frame.matrix(table(data$zona, data$medio))
table_data_mode$zona <- rownames(table_data_mode)

# Coordenadas (en el CRS del shapefile)
table_data_mode$long <- NA_real_
table_data_mode$long[table_data_mode$zona == "Noroccidente"]       <- 1060000 - 300
table_data_mode$long[table_data_mode$zona == "Nororiente"]         <- 1065000 - 200
table_data_mode$long[table_data_mode$zona == "Oriente-aguablanca"] <- 1065000 - 600
table_data_mode$long[table_data_mode$zona == "Sur"]                <- 1059.28 * 1000

table_data_mode$lat <- NA_real_
table_data_mode$lat[table_data_mode$zona == "Noroccidente"]       <- 875000 - 1050
table_data_mode$lat[table_data_mode$zona == "Nororiente"]         <- 875000 + 600
table_data_mode$lat[table_data_mode$zona == "Oriente-aguablanca"] <- 870.5 * 1000
table_data_mode$lat[table_data_mode$zona == "Sur"]                <- 866.4 * 1000

cols_pie <- setdiff(names(table_data_mode), c("zona","long","lat"))

# -------------------------------------------------------------------
# 4) Shape + unión con estrato
# -------------------------------------------------------------------
shape <- sf::st_read("mc_comunas.shp", quiet = TRUE)
columna_comuna_shape <- names(shape)[grepl("comuna", names(shape), ignore.case = TRUE)][1]
if (is.na(columna_comuna_shape)) stop("No se detectó ninguna columna con 'comuna' en el shapefile.")
shape$Comuna <- as.integer(gsub("\\D","", as.character(shape[[columna_comuna_shape]])))
shape <- left_join(shape, estratos_comuna, by = "Comuna")

# -------------------------------------------------------------------
# 5) Paletas
# -------------------------------------------------------------------
colores_estrato <- c("Bajo"="#F4F4F4","Medio"="#E6E6E6","Alto"="#D6D6D6")
colores_medio <- c(
  "Auto privado"        = "#C86A62",
  "Modo activo"         = "#D4B86A",
  "Moto privada"        = "#5BA97A",
  "Taxi / Plataforma"   = "#5A9BB0",
  "Transporte informal" = "#9E77A3",
  "Transporte público"  = "#6C78A8"
)

# -------------------------------------------------------------------
# 6) Brújula con flechas 
# -------------------------------------------------------------------
compass_brown <- "#6F3E2B"
arrow_compass <- function(color = "#6F3E2B", txt = 0.85, lwd = 1.8, alen = 0.08){
  grobTree(
    segmentsGrob(x0 = 0.5, y0 = 0.20, x1 = 0.5, y1 = 0.80,
                 gp = gpar(col = color, lwd = lwd),
                 arrow = arrow(type = "closed", ends = "both", length = unit(alen, "npc"))),
    segmentsGrob(x0 = 0.20, y0 = 0.5, x1 = 0.80, y1 = 0.5,
                 gp = gpar(col = color, lwd = lwd),
                 arrow = arrow(type = "closed", ends = "both", length = unit(alen, "npc"))),
    textGrob("N", x = 0.50, y = 0.96, gp = gpar(col = color, cex = txt, fontface = "bold")),
    textGrob("S", x = 0.50, y = 0.04, gp = gpar(col = color, cex = txt, fontface = "bold")),
    textGrob("E", x = 0.96, y = 0.50, gp = gpar(col = color, cex = txt, fontface = "bold")),
    textGrob("W", x = 0.04, y = 0.50, gp = gpar(col = color, cex = txt, fontface = "bold"))
  )
}

# BBox para ubicar brújula y escalar tamaño de los pies
bb    <- sf::st_bbox(shape)
xspan <- as.numeric(bb["xmax"] - bb["xmin"])
yspan <- as.numeric(bb["ymax"] - bb["ymin"])
r_pie <- 0.060 * min(xspan, yspan)   # pies más grandes

bxmin <- as.numeric(bb["xmin"]) + 0.82 * xspan
bxmax <- as.numeric(bb["xmin"]) + 0.92 * xspan
bymin <- as.numeric(bb["ymin"]) + 0.78 * yspan
bymax <- as.numeric(bb["ymin"]) + 0.94 * yspan

# -------------------------------------------------------------------
# 7) Mapa final 
# -------------------------------------------------------------------
map.cali <- ggplot() +
  geom_sf(data = shape, aes(fill = categoria), color = "#BFBFBF", linewidth = 0.25) +
  scale_fill_manual(
    name   = "Estrato predominante",
    values = colores_estrato,
    breaks = c("Bajo","Medio","Alto"),
    na.value = "#FAFAFA"
  ) +
  coord_sf(clip = "off") +
  ggnewscale::new_scale_fill() +
  geom_scatterpie(
    data = table_data_mode,
    aes(x = long, y = lat, group = zona, r = r_pie),
    cols = cols_pie,
    color = "white", linewidth = 0.25, alpha = 0.92
  ) +
  scale_fill_manual(
    name   = "Medio de transporte",
    values = colores_medio[cols_pie],
    breaks = cols_pie,
    guide  = guide_legend(override.aes = list(alpha = 1))
  ) +
  labs(x = NULL, y = NULL, title = "Elección modal por comuna - Cali") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position  = "right",
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    legend.title     = element_text(colour = "grey15"),
    legend.text      = element_text(colour = "grey20")
  ) +
  annotation_custom(
    grob = arrow_compass(color = compass_brown, txt = 0.80, lwd = 1.6, alen = 0.08),
    xmin = bxmin, xmax = bxmax, ymin = bymin, ymax = bymax
  )

ggsave("map.cali.png", map.cali, width = 10, height = 8, dpi = 300, bg = "white")





