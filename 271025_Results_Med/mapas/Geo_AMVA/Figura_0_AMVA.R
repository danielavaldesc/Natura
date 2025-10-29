######################################################
## Figura 1: Georreferenciación tiempos MEDELLÍN    ##
######################################################

library(readxl)
library(ggplot2)
library(dplyr)
library(viridis)
library(sf)
library(rlang)
library(stringr)

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")
dataset <- readxl::read_excel("clean_med_dataset_27102025.xlsx")
dicc    <- readxl::read_excel("diccionario_med.xlsx")

######################################################
## Conteo de registros por municipio y variable     ##
######################################################

# municipios que te interesan
m_seis <- c("Medellín","Bello","Itagüí","Envigado","Caldas","Barbosa")

# variables a revisar
vars_tiempo <- c("p18","p18_p1","p18_p2","p18_p3","p18_p4","p18_c1")

# verificar que existan
stopifnot(all(c("p2_1_1", vars_tiempo) %in% names(dataset)))

# filtrar solo esos municipios
base <- dataset %>%
  filter(p2_1_1 %in% m_seis)

# ---- opción 1: conteo total por municipio ----
conteo_total <- base %>%
  summarise(across(all_of(vars_tiempo),
                   ~sum(!is.na(.x)),
                   .names = "n_{.col}"),
            .by = p2_1_1)

# ---- opción 2: conteo por municipio y género ----
if ("p40" %in% names(base)) {
  conteo_genero <- base %>%
    summarise(across(all_of(vars_tiempo),
                     ~sum(!is.na(.x)),
                     .names = "n_{.col}"),
              .by = c(p2_1_1, p40))
}

# ---- mostrar resultados ----
print("Conteo total por municipio:")
print(conteo_total)

if (exists("conteo_genero")) {
  print("Conteo por municipio × género:")
  print(conteo_genero)
}

# -------- labels desde diccionario --------
label_de <- function(var){
  lbl <- tryCatch(dicc$descripcion[match(var, dicc$codigo)], error = function(e) NA)
  if (is.na(lbl) || length(lbl)==0) return(var)
  as.character(lbl)
}

# -------- shapefile: intenta MACROZONAS y si no, ZONAS SIT --------
leer_shape_mpio <- function(){
  cand <- c("MACROZONAS.shp","ZONAS SIT.shp")
  for (f in cand) if (file.exists(f)) {
    sf <- suppressWarnings(st_read(f, quiet = TRUE))
    campo <- c("Municipio","MUNICIPIO","MPIO","NOMBRE","NOM_MPIO")
    campo <- campo[campo %in% names(sf)][1]
    if (!is.na(campo)) return(rename(sf, p2_1_1 = !!sym(campo)))
  }
  stop("No encontré un SHP con campo de municipio (Municipio/MUNICIPIO/MPIO/NOMBRE/NOM_MPIO).")
}

shape_mpios <- leer_shape_mpio()

# 6 municipios a mostrar
m_seis <- c("Medellín","Bello","Itagüí","Envigado","Caldas","Barbosa")

# Filtra y DISUELVE por municipio (un polígono por ciudad)
shape_seis <- shape_mpios %>%
  filter(p2_1_1 %in% m_seis) %>%
  group_by(p2_1_1) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# -------- datos --------
vars_tiempo <- c("p18","p18_p1","p18_p2","p18_p3","p18_p4","p18_c1")
stopifnot(all(vars_tiempo %in% names(dataset)))
stopifnot(all(c("p2_1_1","p40") %in% names(dataset)))

base <- dataset %>%
  mutate(p2_1_1 = as.character(p2_1_1),
         p40    = as.character(p40)) %>%
  filter(p2_1_1 %in% m_seis, p40 %in% c("Hombre","Mujer")) %>%
  distinct(p2_1_1, p40, across(all_of(vars_tiempo)))  # ya viene agregado

pal_option <- "mako"  # "rocket" o "turbo" si prefieres

for (v in vars_tiempo) {
  titulo_var <- label_de(v)
  
  # Unir valores al shape disuelto
  pinta <- left_join(shape_seis, base[, c("p2_1_1","p40", v)], by = "p2_1_1") %>%
    rename(valor = !!v)
  
  # Rango común de color entre H/M (solo valores no-NA)
  lims <- range(pinta$valor, na.rm = TRUE)
  
  p <- ggplot() +
    # base gris: los 6 municipios (sirve para ver dónde no hay dato)
    geom_sf(data = shape_seis, fill = "grey90", color = "white", size = 0.25) +
    # overlay: colorea SOLO donde hay valor para ese género
    geom_sf(data = subset(pinta, !is.na(valor)), aes(fill = valor),
            color = "white", size = 0.25) +
    facet_wrap(~ p40) +
    scale_fill_viridis_c(option = pal_option, direction = -1,
                         limits = lims, name = "Minutos",
                         na.value = "grey90") +
    labs(title = paste0(titulo_var, " — 6 municipios")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(),
          strip.text = element_text(face = "bold"))
  
  ggsave(
    filename = paste0(v, "_6municipios_facet_HM.png"),
    plot = p, width = 10, height = 6.8, dpi = 300, bg = "transparent"
  )
}
