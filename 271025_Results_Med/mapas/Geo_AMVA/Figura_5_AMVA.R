######################################################
## Figura 5: Georreferenciación de elección modal   ##
######################################################

library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggpubr)
library(plyr)
library(readxl)
library(rlang)
library(dplyr)
library(ggplot2)
library(reshape2)
library(knitr)
library(haven)
library(foreign)
library(stringi)
library(labelled)
library(tidyr)
library(moments)
library(treemapify)
library(hrbrthemes)
library(viridis)
library(kableExtra)
library(sf)
library(rgdal)
library(moments)
library(RColorBrewer)
library(memisc)
library(assertthat)
library(sqldf)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(oz)
library(scatterpie)
library(rgdal)
library(maptools)
library(maps)


setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\mapas\\Geo_AMVA\\")
dataset = readxl::read_excel("clean_med_dataset_27102025.xlsx")

dataset$satisfaccion.medio = as.numeric(dataset$satisfaccion.medio)
dataset$medio[dataset$medio == "Aplicación viajes en motocicleta"] = "Aplicación viajes"
dataset$medio[dataset$medio == "Aplicación viajes en auto"] = "Aplicación viajes"
dataset$medio[dataset$medio == "Taxi"] = "Auto"
data = dataset

# Recodificar zonas
data$zona = data$residencia

for (k in 1:nrow(data)) {
  if (data$residencia[k] %in% c("Medellín", "San Antonio de Prado")) {
    data$zona[k] = "Medellín" 
  }
  
  if (data$residencia[k] %in% c("Girardota", "Barbosa")) {
    data$zona[k] = "Barbosa y Girardota" 
  }
  
  if (data$residencia[k] %in% c("Bello", "Copacabana")) {
    data$zona[k] = "Bello y Copacabana" 
  }
  
  if (data$residencia[k] %in% c("Itagüi", "Sabaneta",
                            "Envigado", "La Estrella", "Caldas")) {
    data$zona[k] =  "Caldas, Itagüí, Sabaneta, Envigado y La Estrella"
  }
  
}

data$medio = as.character(data$medio)
data$medio[data$medio == "Aplicación viajes"] = "Otro"
data$medio[data$medio == "Transporte informal"] = "Otro"
data$medio[data$medio == "Activo"] = "Otro"

data.plot = as.data.frame.array(table(data$zona, data$medio))
data.plot$Municipio = rownames(data.plot)
data.plot$Territory = 1:length(levels(as.factor(data$zona)))

# Archivo shape
shape = st_read("ZONAS SIT.shp")
shape = shape[!is.na(shape$Municipio),]

# Recodificar por municipios
shape$Municipio[shape$Municipio == "Barbosa"] = "Barbosa y Girardota" 
shape$Municipio[shape$Municipio == "Girardota"] = "Barbosa y Girardota" 

shape$Municipio[shape$Municipio == "Bello"] = "Bello y Copacabana" 
shape$Municipio[shape$Municipio == "Copacabana"] = "Bello y Copacabana" 

shape$Municipio[shape$Municipio == "Itagüí"] = "Caldas, Itagüí, Sabaneta, Envigado y La Estrella" 
shape$Municipio[shape$Municipio == "Sabaneta"] = "Caldas, Itagüí, Sabaneta, Envigado y La Estrella" 
shape$Municipio[shape$Municipio == "Envigado"] = "Caldas, Itagüí, Sabaneta, Envigado y La Estrella" 
shape$Municipio[shape$Municipio == "La Estrella"] = "Caldas, Itagüí, Sabaneta, Envigado y La Estrella" 
shape$Municipio[shape$Municipio == "Caldas"] = "Caldas, Itagüí, Sabaneta, Envigado y La Estrella" 


shape_coords <- st_coordinates(st_centroid(shape)) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  mutate(Municipio = shape$Municipio) %>%
  mutate(X = (abs(abs(X) - abs(st_bbox(shape)$xmin)) /
                as.numeric(abs(st_bbox(shape)$xmin) - abs(st_bbox(shape)$xmax))) - 0.5,
         Y = abs(abs(abs(Y) - abs(st_bbox(shape)$ymin)) /
                   as.numeric(abs(st_bbox(shape)$ymin) - abs(st_bbox(shape)$ymax))
         ))

data.plot$X = NA
data.plot$Y = NA
vec_municipios = levels(as.factor(data.plot$Municipio))
list.coords.res = list()
length(list.coords.res) = 4

for (i in 1:nrow(data.plot)) {
  coords.res = shape_coords %>% filter(Municipio == vec_municipios[i])
  nrow = which(data.plot$Municipio == vec_municipios[i])
  list.coords.res[[nrow]] = coords.res
}

data.plot$X[1] = list.coords.res[[1]]$X[1]+0.05
data.plot$Y[1] = list.coords.res[[1]]$Y[1]-0.1

data.plot$X[2] = list.coords.res[[2]]$X[3]
data.plot$Y[2] = list.coords.res[[2]]$Y[3]

data.plot$X[3] = list.coords.res[[3]]$X[4]+0.15
data.plot$Y[3] = list.coords.res[[3]]$Y[4]-0.1

data.plot$X[4] = list.coords.res[[4]]$X[2]+0.2
data.plot$Y[4] = list.coords.res[[4]]$Y[2]+0.11

res <- melt(data.plot, id.vars = c("Territory", "Municipio", "X", "Y"))
colnames(res)[5] = "key"

make_pie <- function(dt, title = NA, legend.position = 0){
  if(is.na(title)){
    title <- unique(dt$Municipio)
  }
  ggplot() +
    geom_bar(data = dt,
             aes(x = "", y = value, fill = key),
             stat = "identity", width = 1) +
    coord_polar("y") +
    theme_void() +
    theme(legend.position = legend.position) +scale_fill_manual(
      values = c("#334b24", "#6b3e2f",
                 "#de7f31", "#dfac3b"))
}

terr1 <- make_pie(dplyr::filter(res, Territory == 1))
terr2 <- make_pie(dplyr::filter(res, Territory == 2))
terr3 <- make_pie(dplyr::filter(res, Territory == 3))
terr4 <- make_pie(dplyr::filter(res, Territory == 4))

(gg_am <- ggplot(data = shape) +
    geom_sf(aes(fill = Municipio)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0 )) +
    theme(legend.position = 0,
          plot.margin = unit(c(0,0,0,0), "cm")) +
scale_fill_manual(values = c(brewer.pal(n=4,name = "Greys")))    
)

leg <- get_legend(make_pie(res, "", legend.position = "left"))

draw_plot_loc <- function(plot, data.plot){
  draw_plot(plot, x = data.plot$X[1], y = data.plot$Y[1],
            height = 0.2)
}

(all <-
    ggdraw(gg_am) +
    draw_plot_loc(terr1, dplyr::filter(res, Territory == 1)) +
    draw_plot_loc(terr2, dplyr::filter(res, Territory == 2)) +
    draw_plot_loc(terr3, dplyr::filter(res, Territory == 3)) +
    draw_plot_loc(terr4, dplyr::filter(res, Territory == 4)) 
)

map.amva = cowplot::plot_grid(all, leg, rel_widths = c(1, 0.1))

ggsave(
  plot = map.amva,
  filename = "map.amva.png",
  bg = "transparent"
) 

#ggsave( plot = map.cali, filename = "map.cali.png", bg = "transparent") 

