#########################################
#########################################
#########################################
## Reducción de dimensionalidad 1:     ##
## Análisis de componentes principales ##
#########################################
#########################################
#########################################

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\")

# Cargar librerías
library(readxl)
library(tidyverse)
library(MASS)
library(caret)
library(gmodels)
library(mvnormtest)

# Cargar base de datos
dataset = readxl::read_excel("output\\clean_med_dataset_27102025.xlsx")
N = nrow(dataset)

#-----------------------------------------------#
# Preliminar: selección de variables            #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------#

# Cargar diccionario
diccionario_clasificado <- read_excel("output/diccionario_med.xlsx")

# Seleccionar variables del Módulo 1
cat_m1 <- c("edad_r2", "pais", "p3_agregado", "p5_agregado",
                     "p7_agregado", "p8_agregado", "p9_estrato3", "p40")
cont_m1 <- c("p1edad")

# Seleccionar variables del Módulo 2
cat_m2 <- c("edad_r2", "pais",
                     "p13","p14",
                     "p15_autos_agregado", 
                     "p15_1_autos_propios_agregado", 
                     "p16_motos_agregado", 
                     "p16_1_motos_propias_agregado",
                     "p17_modo_agregado",
                     "cilindraje_auto_agregado",
                     "cilindraje_moto_agregado",
                     "modelo_vehiculo_agregado",
                     "p19comuna", "p22",
                     "p23_agregado")
cont_m2 <- c("p1edad", "p18",
                      "p18_p1",
                      "p18_p2",
                      "p18_p3",
                      "p18_p4",
                      "p18_c1")

# Seleccionar variables del Módulo 3
cat_m3 <- c(
  #"p25_razones_agregadas",       # Razones para elección de modo
  "p26_agregado",                # Aspecto que menos le gusta
  #"p27_situaciones_multiples",   # Situaciones percibidas / evitadas
  "p29_modo_ideal_agregado",     # Modo de transporte ideal
  "p30_razon_no_uso_agregado",   # Razones para no usar modo ideal
  "p31_fuente_contaminacion_agregada", # Fuente percibida de contaminación
  "p33_modo_contaminante_agregado",    # Modo más contaminante percibido
  "p35_razon_agregada"                # Razones principales (agregadas)
)

cont_m3 <- c(
  "p24",
  "p28_importancia_costo_compra",
  "p28_importancia_costo_uso",
  "p28_importancia_comodidad",
  "p28_importancia_tiempo",
  "p28_importancia_riesgo_robo",
  "p28_importancia_riesgo_acoso",
  "p28_importancia_discriminacion",
  "p28_importancia_emisiones",
  "p28_importancia_siniestralidad",
  "p32_contaminacion_likert",
  "p36_influencia_amigos",
  "p37_influencia_familia"
)


# Seleccionar variables del Módulo 4
cat_m4 <- c("p38p38_1", "p38p38_2", "p38p38_3",
  "p38p38_4","p38p38_5","p38p38_6","p38p38_7","p38p38_99"
)


# Vector de variables categóricas
cat_vars = c(cat_m1, cat_m2, cat_m3, cat_m4)

# Vector de variables continuas
cont_vars = c(cont_m1, cont_m2, cont_m3)

#----------------------------#
# Crear variables previas    #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------#
dataset$id = dataset$cuestionario
dataset$medio = dataset$p17_modo_agregado

dataset = dataset %>% dplyr::select(id, medio, cat_vars, cont_vars)

# Variable de tiempo
dataset$tiempo_total <- rowSums(
  dataset[, c("p18", "p18_p1", "p18_p2", "p18_p3", "p18_p4", "p18_c1")],
  na.rm = TRUE
)

dataset = dataset %>% dplyr::select(-c("p18", "p18_p1", "p18_p2", "p18_p3", "p18_p4", "p18_c1"))

#----------------------------#
# Análisis de valores NAs    #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------#

# Tabla de resumen NAs
na_summary <- dataset %>%
  summarise(across(everything(),
                   ~ sum(is.na(.)),
                   .names = "na_{.col}")) %>%
  tidyr::pivot_longer(everything(),
                      names_to = "variable",
                      values_to = "n_missing") %>%
  mutate(prop_missing = round(n_missing / nrow(dataset) * 100, 2))

na_summary %>%
  arrange(desc(prop_missing)) %>%
  head(20)   # muestra las 20 con más NA

# Gráfica de missings
library(ggplot2)
na_summary %>%
  filter(n_missing > 0) %>%
  ggplot(aes(x = reorder(variable, prop_missing),
             y = prop_missing)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Variable",
       y = "% de valores faltantes",
       title = "Proporción de valores NA por variable") +
  theme_minimal(base_size = 13)


#-------------------------------#
# Guardar dataset: input.famd   #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------#
writexl::write_xlsx(dataset, "output/input_famd_med_29102025.xlsx")

