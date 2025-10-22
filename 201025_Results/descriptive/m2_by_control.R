###############################################################################
# DESCRIPTIVE SUMMARIES BY GROUP
# Módulo 2: Movilidad
# Descripción:
#   Este script genera tablas descriptivas (n(%), mediana [Q1–Q3]) 
#   para las variables del Módulo 2, diferenciadas por:
#   Las tablas se exportan a archivos Excel para análisis posterior.
###############################################################################

## ============================================================================
## 0. Cargar entorno y funciones auxiliares
## ============================================================================

# Función principal para resúmenes
source("aux_functions/aux_summary.R")

# Paquetes requeridos
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

# Seleccionar variables del Módulo 1
vars_m2 <- diccionario_clasificado %>%
  filter(modulo == "Módulo 2: Movilidad")


## ============================================================================
## 2. Preparar datos base
## ============================================================================

# Mantener solo observaciones con género definido (Hombre / Mujer)
dataset <- dataset %>%
  filter(p40 %in% c("Mujer", "Hombre"))

# Seleccionar variables relevantes del módulo
df_m2 <- dataset %>% select(all_of(vars_m2$codigo))

# Variables categóricas y continuas a describir
cat_to_describe <- c("edad_r2", "pais",
                     "p13",
                     "p14",
                     "p15_autos_agregado", 
                     "p15_1_autos_propios_agregado", 
                     "p16_motos_agregado", 
                     "p16_1_motos_propias_agregado",
                     "p17_modo_agregado",
                     "cilindraje_auto_agregado",
                     "cilindraje_moto_agregado",
                     "cilindraje_camion_agregado",
                     "modelo_vehiculo_agregado",
                     "p19comuna",
                     "p23_agregado")

cont_to_describe <- c("p1edad", "p18",
                      "p18_p1",
                      "p18_p2",
                      "p18_p3",
                      "p18_p4",
                      "p18_c1")

## ============================================================================
## 3. Descripción por género
## ============================================================================

tabla_m2 <- describe_by_group(
  data = dataset,
  vars_cat = cat_to_describe,
  vars_cont = cont_to_describe,
  control_var = "p40",  # Variable de control: Género
  diccionario = diccionario_clasificado
)

# Mostrar y exportar
writexl::write_xlsx(tabla_m2, "output/m2/21102025_mod2_by_gender.xlsx")


## ============================================================================
## 4. Descripción por nivel socioeconómico (SES)
## ============================================================================

tabla_m2 <- describe_by_group(
  data = dataset,
  vars_cat = cat_to_describe,
  vars_cont = cont_to_describe,
  control_var = "p9_estrato3",  # Variable de control: Estrato
  diccionario = diccionario_clasificado
)

# Mostrar y exportar
writexl::write_xlsx(tabla_m2, "output/m2/21102025_mod2_by_ses.xlsx")


## ============================================================================
## 5. Descripción por modo de transporte principal
## ============================================================================

tabla_m2 <- describe_by_group(
  data = dataset,
  vars_cat = cat_to_describe,
  vars_cont = cont_to_describe,
  control_var = "p17_modo_agregado",  # Variable de control: Modo principal
  diccionario = diccionario_clasificado
)

# Mostrar y exportar
writexl::write_xlsx(tabla_m2, "output/m2/21102025_mod2_by_travel_mode.xlsx")



