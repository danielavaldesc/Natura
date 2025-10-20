############################################################
## MAPA COMPARATIVO: MEDELLÍN vs CALI (coordenadas p47*) ##
############################################################

# Paquetes
libs <- c("tidyverse","readxl","janitor","leaflet","leaflet.extras",
          "viridis","hexbin","ggplot2","plotly")
invisible(lapply(setdiff(libs, rownames(installed.packages())), install.packages))
lapply(libs, library, character.only = TRUE)

# Ruta
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\")
dir.create("Output/Comparativo", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Cargar y limpiar
# ---------------------------------------------------------
load_and_clean <- function(path, ciudad){
  readxl::read_excel(path) %>%
    clean_names() %>%
    rename(lat = p47latitude, lon = p47longitude,
           alt = p47altitude, acc = p47accuracy) %>%
    mutate(lat = as.numeric(lat),
           lon = as.numeric(lon),
           alt = as.numeric(alt),
           acc = as.numeric(acc),
           ciudad = ciudad) %>%
    filter(!is.na(lat), !is.na(lon),
           lat >= -90 & lat <= 90,
           lon >= -180 & lon <= 180)
}

med <- load_and_clean("Output\\base_Med_2025.xlsx",  "Medellín")
cal <- load_and_clean("Output\\base_Cali_2025.xlsx", "Cali")

# ---------------------------------------------------------
# 2) Filtros geográficos (ajusta si lo necesitas)
# ---------------------------------------------------------
in_bbox_med <- function(lat,lon) (lat >= 6.10 & lat <= 6.42) & (lon >= -75.75 & lon <= -75.45)
in_bbox_cali<- function(lat,lon) (lat >= 3.35 & lat <= 3.55) & (lon >= -76.60 & lon <= -76.40)

med <- med %>% filter(in_bbox_med(lat,lon))
cal <- cal %>% filter(in_bbox_cali(lat,lon))

df <- bind_rows(med, cal)

# ---------------------------------------------------------
# 3) LEAFLET: puntos + heatmap con capas por ciudad
# ---------------------------------------------------------
pal_med <- colorNumeric(viridis(100), med$alt, na.color = "transparent")
pal_cal <- colorNumeric(magma(100),  cal$alt, na.color = "transparent")

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Medellín puntos
  addCircleMarkers(data = med, lng = ~lon, lat = ~lat,
                   radius = 4, stroke = FALSE, fillOpacity = 0.8,
                   color = ~pal_med(alt), group = "Medellín - puntos",
                   popup = ~paste0("<b>Altitud:</b> ", round(alt,1),
                                   "<br><b>Precisión:</b> ", round(acc,1))) %>%
  # Cali puntos
  addCircleMarkers(data = cal, lng = ~lon, lat = ~lat,
                   radius = 4, stroke = FALSE, fillOpacity = 0.8,
                   color = ~pal_cal(alt), group = "Cali - puntos",
                   popup = ~paste0("<b>Altitud:</b> ", round(alt,1),
                                   "<br><b>Precisión:</b> ", round(acc,1))) %>%
  # Heatmaps
  addHeatmap(data = med, lng = ~lon, lat = ~lat, blur = 25, radius = 18,
             max = 0.8, group = "Medellín - heatmap") %>%
  addHeatmap(data = cal, lng = ~lon, lat = ~lat, blur = 25, radius = 18,
             max = 0.8, group = "Cali - heatmap") %>%
  addLayersControl(
    overlayGroups = c("Medellín - puntos","Cali - puntos",
                      "Medellín - heatmap","Cali - heatmap"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addScaleBar(position = "bottomleft")
htmlwidgets::saveWidget(m, "Output/Comparativo/Mapa_Comparativo_Med_Cali.html",
                        selfcontained = TRUE)

# ---------------------------------------------------------
# 4) HEXBIN comparativo (estático)
# ---------------------------------------------------------
p_hex <- ggplot(df, aes(x = lon, y = lat)) +
  stat_binhex(bins = 35) +
  coord_equal() +
  facet_wrap(~ciudad, ncol = 2) +
  labs(title = "Densidad espacial (hexbin): Medellín vs Cali",
       x = "Longitud", y = "Latitud", fill = "n") +
  theme_minimal()
ggsave("Output/Comparativo/hexbin_med_vs_cali.png", p_hex, width = 10, height = 5, dpi = 200)

# ---------------------------------------------------------
# 5) Violines de ALTITUD por ciudad
# ---------------------------------------------------------
p_violin <- ggplot(df, aes(x = ciudad, y = alt, fill = ciudad)) +
  geom_violin(trim = TRUE, alpha = .6) +
  geom_boxplot(width = .15, outlier.size = .7, alpha = .8) +
  theme_minimal() +
  labs(title = "Distribución de altitud por ciudad",
       x = "", y = "Altitud (m)") +
  theme(legend.position = "none")
ggsave("Output/Comparativo/violin_altitud_ciudad.png", p_violin, width = 7, height = 5, dpi = 200)

# ---------------------------------------------------------
# 6) Mapas 3D (uno por ciudad)
# ---------------------------------------------------------
p3d_med <- plot_ly(med, x=~lon, y=~lat, z=~alt, type="scatter3d", mode="markers",
                   marker = list(size=3, color=~alt, colorscale="Viridis", showscale=TRUE)) %>%
  layout(title = "Altitud 3D - Medellín",
         scene = list(xaxis=list(title="Longitud"), yaxis=list(title="Latitud"), zaxis=list(title="Altitud (m)")))
htmlwidgets::saveWidget(p3d_med, "Output/Comparativo/Medellin_3D.html", selfcontained = TRUE)

p3d_cal <- plot_ly(cal, x=~lon, y=~lat, z=~alt, type="scatter3d", mode="markers",
                   marker = list(size=3, color=~alt, colorscale="Magma", showscale=TRUE)) %>%
  layout(title = "Altitud 3D - Cali",
         scene = list(xaxis=list(title="Longitud"), yaxis=list(title="Latitud"), zaxis=list(title="Altitud (m)")))
htmlwidgets::saveWidget(p3d_cal, "Output/Comparativo/Cali_3D.html", selfcontained = TRUE)

# ---------------------------------------------------------
# 7) Extra: relación Precisión vs Altitud por ciudad
# ---------------------------------------------------------
p_sc <- ggplot(df, aes(x = acc, y = alt, color = ciudad)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_minimal() +
  labs(title = "Altitud vs Precisión GPS", x = "Precisión (m)", y = "Altitud (m)")
ggsave("Output/Comparativo/altitud_vs_precision_ciudad.png", p_sc, width = 8, height = 5, dpi = 200)

message("✅ Listo: mapas y figuras en Output/Comparativo/")
