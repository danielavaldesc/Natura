###############################################################################
# DESCRIPTIVE SUMMARIES BY GROUP
# Módulo 3: Percepciones, preferencias y deseos
# Descripción:
#   Genera tablas descriptivas (n(%), mediana [Q1–Q3]) 
#   para las variables del Módulo 3, diferenciadas por:
#     1. Género
#     2. Estrato socioeconómico
#     3. Modo de transporte principal
#   Las tablas se exportan en formato Excel para análisis posterior.
###############################################################################

## ============================================================================
## 0. Cargar entorno y funciones auxiliares
## ============================================================================

source("aux_functions/aux_summary.R")

library(dplyr)
library(readxl)
library(writexl)
library(tidyverse)


## ============================================================================
## 1. Cargar datos y diccionario
## ============================================================================

setwd("C:/Users/sergio.barona/Desktop/Natura/201025_Results/")

dataset <- read_excel("output/clean_cali_dataset_21102025.xlsx")
diccionario_clasificado <- read_excel("output/diccionario_cali.xlsx")

# Seleccionar variables del Módulo 3
vars_m3 <- diccionario_clasificado %>%
  filter(modulo == "Módulo 3: Percepciones, preferencias y deseos")

# Crear carpeta de salida si no existe
if (!dir.exists("output/m3")) dir.create("output/m3", recursive = TRUE)


## ============================================================================
## 2. Preparar datos base
## ============================================================================

# Mantener solo observaciones con género definido (Hombre / Mujer)
dataset <- dataset %>%
  filter(p40 %in% c("Mujer", "Hombre"))

# Subconjunto de variables del módulo
df_m3 <- dataset %>% select(all_of(vars_m3$codigo))


## ============================================================================
## 3. Clasificación de variables del Módulo 3
## ============================================================================

# Variables categóricas (respuestas nominales o múltiples)
cat_to_describe <- c(
  "p24",                         # Satisfacción general o percepción
  "p25_razones_agregadas",       # Razones para elección de modo
  "p26_agregado",                # Aspecto que menos le gusta
  "p27_situaciones_multiples",   # Situaciones percibidas / evitadas
  "p29_modo_ideal_agregado",     # Modo de transporte ideal
  "p30_razon_no_uso_agregado",   # Razones para no usar modo ideal
  "p31_fuente_contaminacion_agregada", # Fuente percibida de contaminación
  "p33_modo_contaminante_agregado",    # Modo más contaminante percibido
  "p35_razon_agregada"                # Razones principales (agregadas)
)

# Variables ordinales (tipo Likert o escala 1–5)
cont_to_describe <- c(
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


## ============================================================================
## 4. Descripción por género
## ============================================================================

tabla_m3_gender <- describe_by_group(
  data = dataset,
  vars_cat = cat_to_describe,
  vars_cont = cont_to_describe,
  control_var = "p40",  # Variable de control: Género
  diccionario = diccionario_clasificado
)

# Exportar resultados
write_xlsx(tabla_m3_gender, "output/m3/21102025_mod3_by_gender.xlsx")


## ============================================================================
## 5. Descripción por nivel socioeconómico (SES)
## ============================================================================


tabla_m3_ses <- describe_by_group(
  data = dataset,
  vars_cat = cat_to_describe,
  vars_cont = cont_to_describe,
  control_var = "p9_estrato3",  # Variable de control: Estrato
  diccionario = diccionario_clasificado
)

write_xlsx(tabla_m3_ses, "output/m3/21102025_mod3_by_ses.xlsx")


## ============================================================================
## 6. Descripción por modo de transporte principal
## ============================================================================

tabla_m3_travel <- describe_by_group(
  data = dataset,
  vars_cat = cat_to_describe,
  vars_cont = cont_to_describe,
  control_var = "p17_modo_agregado",  # Variable de control: Modo principal
  diccionario = diccionario_clasificado
)

write_xlsx(tabla_m3_travel, "output/m3/21102025_mod3_by_travel_mode.xlsx")


