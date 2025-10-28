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

setwd("C:/Users/PC/Desktop/Proyecto_movilidad/Fulbright/AMVA/Geo_AMVA")
dataset = readxl::read_excel("Input.remuestreo.2.xlsx")
dataset$satisfaccion.medio = as.numeric(dataset$satisfaccion.medio)
dataset$medio[dataset$medio == "Aplicación viajes en motocicleta"] = "Aplicación viajes"
dataset$medio[dataset$medio == "Aplicación viajes en auto"] = "Aplicación viajes"
dataset$medio[dataset$medio == "Taxi"] = "Auto"
data = dataset


# Archivo shape
shape = readOGR(dsn = ".", layer = "ZONAS SIT")
shape_st = st_read("ZONAS SIT.shp")

for (k in 1:nrow(data)) {
  if (data$residencia[k] %in% c("Medellín", "San Antonio de Prado")) {
    data$residencia[k] = "Medellín" 
  }
}

mean.municipios = aggregate(data$tiempo.medio, list(data$residencia), FUN=mean)
colnames(mean.municipios) = c("Municipio", "mean")
mean.municipios$Municipio[mean.municipios$Municipio == "Itagüi"] = "Itagüí"

mean.plot = merge(shape_st, mean.municipios, by = "Municipio")


mean.plot$Municipio = as.factor(mean.plot$Municipio)


time.amva = ggplot() + geom_sf(data = mean.plot, mapping = aes(fill = mean)) +
  scale_fill_viridis_c(name = "Mean time", direction = -1) 


ggsave(
  plot = time.amva,
  filename = "tiempo.amva.png",
  bg = "transparent"
) 



