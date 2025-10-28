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

setwd("C:/Users/PC/Desktop/Proyecto_movilidad/Fulbright/Cali/Geo_Cali")
dataset = readxl::read_excel("Input.remuestreo.2.xlsx")
dataset$satisfaccion.medio = as.numeric(dataset$satisfaccion.medio)
dataset$medio[dataset$medio == "Aplicación viajes en motocicleta"] = "Aplicación viajes"
dataset$medio[dataset$medio == "Aplicación viajes en auto"] = "Aplicación viajes"
dataset$medio[dataset$medio == "Taxi"] = "Auto"
data = dataset

# La variable barrio no aplica si es de otra ciudad
data$residencia = as.character(data$residencia)
data$barrio = as.character(data$barrio)
for (k in 1:nrow(data)) {
  if (data$residencia[k] != "Cali") {
    data$barrio[k] = "No aplica"
  }
}
data$residencia = as.factor(data$residencia)
data$barrio = as.factor(data$barrio)

# Recuperar la comuna origen
barrios = data.frame(id = data$id, Barrio = data$barrio)
comunas = read_excel("Listado barrios.xlsx", sheet = 2)

no_aplica = data.frame(Comuna = 99, Barrio = "No aplica")
comunas = rbind(no_aplica, comunas)

barrio_comunas = merge(barrios, comunas, by = "Barrio")
barrio_comunas = barrio_comunas[c("id", "Barrio", "Comuna")]
barrio_comunas = barrio_comunas[order(barrio_comunas$id),]

data = merge(data, barrio_comunas, by = "id")

# Recodificar zonas
data$zona = data$Comuna

for (k in 1:nrow(data)) {
  if (data$Comuna[k] %in% c(1, 2, 3, 9)) {
    data$zona[k] = "Noroccidente" 
  }
  
  if (data$Comuna[k] %in% c(4, 5, 6, 7, 8)) {
    data$zona[k] = "Nororiente" 
  }
  
  if (data$Comuna[k] %in% c(11, 12, 13, 14, 15, 16, 21)) {
    data$zona[k] = "Oriente-aguablanca" 
  }
  
  if (data$Comuna[k] %in% c(10, 19, 20, 18, 17, 22)) {
    data$zona[k] = "Sur" 
  }
  
}

data$medio = as.character(data$medio)
data$medio[data$medio == "Aplicación viajes"] = "Otro"
data$medio[data$medio == "Transporte informal"] = "Otro"
data$medio[data$medio == "Activo"] = "Otro"

# conservar != 99
data = data %>% filter(zona != 99)
table_data_mode = table(data$zona, data$medio)
table_data_mode = as.data.frame.array(table_data_mode)
table_data_mode$zona = rownames(table_data_mode)

table_data_mode$long = NA
table_data_mode$long[1] = 1060000-300
table_data_mode$long[2] = 1065000-200 
table_data_mode$long[3] = 1065000-600
table_data_mode$long[4] = 1059.28*1000
  
table_data_mode$lat = NA
table_data_mode$lat[1] = 875000-1050
table_data_mode$lat[2] = 875000+600 
table_data_mode$lat[3] = 870.5*1000
table_data_mode$lat[4] = 866.4*1000

# Recuperar la variable de participación por comuna
norccidente = c(1, 2, 3, 9)
noriente = c(4, 5, 6, 7, 8)
oriente_ag = c(11, 12, 13, 14, 15, 16, 21)
sur = c(10, 19, 20, 18, 17, 22)

medio_comunas = data.frame(comuna = c(norccidente, noriente,
                                      oriente_ag, sur),
                           zona = c(rep("Noroccidente", length(norccidente)),
                                    rep("Noriente", length(noriente)),
                                    rep("Oriente-aguablanca", length(oriente_ag)),
                                    rep("Sur", length(sur))))

# Base de datos final
dataset_map = merge(table_data_mode, medio_comunas, by = "zona")

# Archivo shape
shape = readOGR(dsn = "mc_comunas.shp")
shape_st = fortify(shape)
noriente_cod = c("0", "1", "2", "3", "4")
oriente_cod = c("20", "7", "18", "11", "8", "5","21")
noroccidente_cod = c("6", "14", "15", "16")
sur_cod = c("12", "13", "10", "9", "17", "19")

shape_st$id_cond = as.character(shape_st$id)
for (k in 1:nrow(shape_st)) {
  if (shape_st$id[k] %in% noriente_cod) {
    shape_st$id_cond[k] = "nororiente"
  } 
  
  if (shape_st$id[k] %in% oriente_cod) {
    shape_st$id_cond[k] = "oriente"
  }
  if (shape_st$id[k] %in% noroccidente_cod) {
    shape_st$id_cond[k] = "noroccidente"
  }
  if (shape_st$id[k] %in% sur_cod) {
    shape_st$id_cond[k] = "sur"
  }
    
}

palette = brewer.pal(n=4,name = "Greys")
               
map.cali = ggplot(data = shape_st, aes(long, lat, group=group,
                            fill = id_cond)) +
  geom_polygon(col = "black") + geom_scatterpie(aes(x=long, y=lat, 
                                                    group = zona, r = 190*6), 
                  data = table_data_mode, 
                  cols = colnames(table_data_mode[,c(1:4)])) + scale_fill_manual(
                    values = c("#334b24", "#6b3e2f", palette[1], palette[2],palette[3],
                      "#de7f31", palette[4], "#dfac3b")
                  )

ggsave(
  plot = map.cali,
  filename = "map.cali.png",
  bg = "transparent"
) 

# Probar coordenadas de los pie charts
ggplot(data = shape_st, aes(long, lat, group=group, fill = id_cond)) +
  geom_polygon(color = "black")  +
  geom_point(aes(x=1065000-600, y=870.5*1000), colour="red") +
  geom_point(aes(x=1059.28*1000, y=866.4*1000), colour="green") +
  geom_point(aes(x=1065000-200, y=875000+600), colour="blue")+
  geom_point(aes(x=1060000-300, y=875000-1050), colour="yellow")


