######################################################
## Figura 1: Georreferenciación tiempos CALI        ##
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

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\mapas\\Geo_Cali\\")
dataset = readxl::read_excel("clean_cali_dataset_21102025.xlsx")
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


data$medio = as.character(data$medio)
data$medio[data$medio == "Aplicación viajes"] = "Otro"
data$medio[data$medio == "Transporte informal"] = "Otro"
data$medio[data$medio == "Activo"] = "Otro"


# Filtrar para la información de Cali
data_grouped = data %>% filter(Comuna != 99)
data_grouped = data_grouped %>% filter(residencia == "Cali")

# Recodificar el tiempo recorrido
data_grouped = data_grouped %>% mutate(tiempo_class = factor(tiempo.medio,
                                                             levels = c("Entre 10 y 20 minutos",
                                                                        "Entre 21 y 30 minutos",
                                                                        "Entre 31 y 40 minutos",
                                                                        "Entre 41 y 50 minutos",
                                                                        "Entre 51 y 60 minutos",
                                                                        "Más de 60 minutos"), 
                                                             labels = c("11-20", 
                                                                        "21-30",
                                                                        "31-40",
                                                                        "41-50",
                                                                        "51-60", 
                                                                        "61-70")))

dataset = data_grouped[c("id", "estrato", "Comuna",
                         "tiempo_class")]
colnames(dataset) = c("id", "est", "comuna",
                      "tiempo")
grouped_var = c("tiempo")

# Datos diferenciados según comuna


n.comunas = length(levels(as.factor(dataset$comuna)))
input.comunas = list(length = n.comunas)
for (i in 1:n.comunas) {
  df.i = dataset %>% filter(comuna == i)
  input.comunas[[i]] = df.i
  names(input.comunas)[i] = paste0("Comuna.",i)
}

time.comunas = data.frame(comuna = levels(as.factor(dataset$comuna)),
                          n =NA,
                          mean = NA, sd = NA,
                          median = NA, Q1 = NA, Q3 = NA)

output.list = list(tiempo = time.comunas)

for (i in grouped_var) {
  n.list = which(names(output.list) == i)
  
  for (j in 1:length(input.comunas)) {
    gp.comuna = input.comunas[[j]]
    gp_aux = grouped_function(gp.comuna, i)
    comuna.row = which(output.list[[n.list]]$comuna == j)
    
    output.list[[n.list]]$comuna[comuna.row] = paste0("Comuna ", j) 
    output.list[[n.list]]$n[comuna.row] = sum(gp_aux$f)
    output.list[[n.list]]$mean[comuna.row] = mean_gp(gp_aux)
    output.list[[n.list]]$sd[comuna.row] = sd_gp(gp_aux)
    output.list[[n.list]]$median[comuna.row] = quartil_qp(gp_aux, 0.5)
    output.list[[n.list]]$Q1[comuna.row] = quartil_qp(gp_aux, 0.25)
    output.list[[n.list]]$Q3[comuna.row] = quartil_qp(gp_aux, 0.75)
    
  }
  
}

data.plot = output.list[[1]]
data.plot$Comuna = data.plot$comuna


# Archivo shape
shape = readOGR(dsn = ".", layer = "mc_comunas")
shape_st = st_read("mc_comunas.shp")
shape_st = shape_st[order(shape_st$comuna),]
colnames(shape_st)[2] = "Comuna"

shape_st = merge(shape_st, data.plot, by = "Comuna")

shape_st$Comuna = as.factor(shape_st$Comuna)
palette = brewer.pal(n=4,name = "Greys")

time.cali = ggplot() + geom_sf(data = shape_st, mapping = aes(fill = mean)) +
  scale_fill_viridis_c(name = "Mean time", direction = -1) 


ggsave(
  plot = time.cali,
  filename = "tiempo.cali.png",
  bg = "transparent"
) 



