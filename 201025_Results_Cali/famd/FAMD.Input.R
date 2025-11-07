

#########################################
#########################################
#########################################
## Reducción de dimensionalidad 1:     ##
## Análisis de componentes principales ##
#########################################
#########################################
#########################################

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\")
dataset = readxl::read_excel("output/clean_cali_dataset_21102025.xlsx")

# Cargar librerías
library(readxl)
library(tidyverse)
library(MASS)
library(caret)
library(gmodels)
library(mvnormtest)

# Cargar base de datos
dataset = readxl::read_excel("output/clean_cali_dataset_21102025.xlsx")
N = nrow(dataset)

#-----------------------------------------------#
# Preliminar: selección de variables            #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------#

# Cargar diccionario
diccionario_clasificado <- read_excel("output/diccionario_cali.xlsx")

# Seleccionar variables del Módulo 1
cat_m1 <- c("edad_r2", "p3_agregado", "p5_agregado",
                     "p7_agregado", "p9_estrato3","p12_dificultad_binaria","p40")
cont_m1 <- c("p1edad")

# Seleccionar variables del Módulo 2
cat_m2 <- c("edad_r2", "p13","p14",
                     "p15_autos_agregado", 
                     "p16_motos_agregado",
                     "p19comuna","p22",
                     "p23_agregado")
cont_m2 <- c("p1edad", "p18",
                      "p18_p1",
                      "p18_p2",
                      "p18_p3",
                      "p18_p4",
                      "p18_c1")

# Seleccionar variables del Módulo 3
cat_m3 <- c(
  "p32_contaminacion_likert" # Nivel de contaminación generado por su forma de movilizarse
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
cat_m4 <- c("p38p38_dummy", "p39_lugar_agregado_mod"
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

# Variables continuas
cont_vars = cont_vars[!cont_vars %in%c("p18", "p18_p1", "p18_p2", "p18_p3", "p18_p4", "p18_c1")]
dataset <- dataset %>%
  dplyr::mutate(
    across(all_of(cat_vars), ~ as.factor(.x)),    # Categóricas a factor
    across(all_of(cont_vars), ~ as.numeric(.x))   # Continuas a numérico
  )

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
writexl::write_xlsx(dataset, "output/input_famd_cali_29102025.xlsx")

