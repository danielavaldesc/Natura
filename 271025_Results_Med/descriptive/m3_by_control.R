###############################################################################
# DESCRIPTIVE SUMMARIES BY GROUP
# Módulo 3: Percepciones, preferencias y deseos
# Descripción:
#   Genera tablas descriptivas (n(%), mediana [Q1–Q3]) 
#   para las variables del Módulo 3, diferenciadas por:
#     1. Género
#     2. Estrato socioeconómico
#     3. Modo de transporte principal
#     4. Edad
#     5. Nivel de educación
#     6. Ocupación
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

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\")

dataset <- read_excel("output/clean_med_dataset_27102025.xlsx")
diccionario_clasificado <- read_excel("output/diccionario_med.xlsx")

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
df_m3 <- dataset %>% dplyr::select(all_of(vars_m3$codigo))


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
## 4. Tablas descriptivas
## ============================================================================

# Asegura carpeta de salida
dir.create("output/m3", recursive = TRUE, showWarnings = FALSE)

# Mapa: variable de control -> sufijo de archivo
control_vars_m3 <- c(
  "p40"               = "by_gender",        # Género
  "p9_estrato3"       = "by_ses",           # Estrato
  "p17_modo_agregado" = "by_travel_mode",   # Modo principal
  "p5_agregado"       = "by_education",     # Nivel educativo agrupado
  "p7_agregado"       = "by_main_activity",  # Actividad principal
  "edad_r2"           = "by_age_r"           # Edad
)

# Ejecuta y exporta
resultados_m3 <- lapply(names(control_vars_m3), function(ctrl) {
  tabla <- describe_by_group(
    data        = dataset,
    vars_cat    = cat_to_describe,
    vars_cont   = cont_to_describe,
    control_var = ctrl,
    diccionario = diccionario_clasificado
  )
  out_path <- sprintf("output/m3/27102025_mod3_%s.xlsx", control_vars_m3[[ctrl]])
  writexl::write_xlsx(tabla, out_path)
  tabla
})



