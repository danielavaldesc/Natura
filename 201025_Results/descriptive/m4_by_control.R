###############################################################################
# DESCRIPTIVE SUMMARIES BY GROUP
# Módulo 4: Experiencias de acoso, inseguridad y VBG
# Descripción:
#   Este script genera tablas descriptivas (n(%), mediana [Q1–Q3]) 
#   para las variables del Módulo 4, diferenciadas por:
#     1. Género
#     2. Estrato socioeconómico
#     3. Modo de transporte principal
#   Las tablas se exportan a archivos Excel para análisis posterior.
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

# Seleccionar variables del Módulo 4
vars_m4 <- diccionario_clasificado %>%
  filter(modulo == "Módulo 4: Experiencias de acoso, inseguridad y VBG")

# Crear carpeta de salida si no existe
if (!dir.exists("output/m4")) dir.create("output/m4", recursive = TRUE)

## ============================================================================
## 2. Preparar datos base
## ============================================================================

# Mantener solo observaciones con género definido (Hombre / Mujer)
dataset <- dataset %>%
  filter(p40 %in% c("Mujer", "Hombre"))

# Subconjunto de variables del módulo
df_m4 <- dataset %>% select(all_of(vars_m4$codigo))


## ============================================================================
## 3. Clasificación de variables del Módulo 4
## ============================================================================

# Variables categóricas (respuestas nominales o múltiples)
cat_to_describe <- c(
  "p38p38_1",
  "p38p38_2",
  "p38p38_3",
  "p38p38_4",
  "p38p38_5",
  "p38p38_6",
  "p38p38_7",
  "p38p38_99"
)

# En este módulo no hay variables continuas ni Likert puras
cont_to_describe <- c()


## ============================================================================
## 4. Descripción por género
## ============================================================================


tabla_m4_gender <- describe_by_group(
  data = dataset,
  vars_cat = cat_to_describe,
  vars_cont = cont_to_describe,
  control_var = "p40",  # Variable de control: Género
  diccionario = diccionario_clasificado
)

# Exportar resultados
write_xlsx(tabla_m4_gender, "output/m4/21102025_mod4_by_gender.xlsx")


## ============================================================================
## 5. Descripción por nivel socioeconómico (SES)
## ============================================================================

tabla_m4_ses <- describe_by_group(
  data = dataset,
  vars_cat = cat_to_describe,
  vars_cont = cont_to_describe,
  control_var = "p9_estrato3",  # Variable de control: Estrato
  diccionario = diccionario_clasificado
)

write_xlsx(tabla_m4_ses, "output/m4/21102025_mod4_by_ses.xlsx")


## ============================================================================
## 6. Descripción por modo de transporte principal
## ============================================================================

tabla_m4_travel <- describe_by_group(
  data = dataset,
  vars_cat = cat_to_describe,
  vars_cont = cont_to_describe,
  control_var = "p17_modo_agregado",  # Variable de control: Modo principal
  diccionario = diccionario_clasificado
)

write_xlsx(tabla_m4_travel, "output/m4/21102025_mod4_by_travel_mode.xlsx")


## ============================================================================
## 7. Finalización
## ============================================================================
