###############################################################################
# DESCRIPTIVE SUMMARIES BY GROUP
# Módulo 1: Socio-Demográfico
# Descripción:
#   Este script genera tablas descriptivas (n(%), mediana [Q1–Q3]) 
#   para las variables del Módulo 1, diferenciadas por:
#     1. Género
#     2. Estrato socioeconómico
#     3. Modo de transporte principal
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


## ============================================================================
## 1. Cargar datos y diccionario
## ============================================================================

setwd("C:/Users/Portatil/Desktop/Natura/201025_Results/")

dataset <- read_excel("output/clean_cali_dataset_21102025.xlsx")
diccionario_clasificado <- read_excel("output/diccionario_cali.xlsx")

# Seleccionar variables del Módulo 1
vars_m1 <- diccionario_clasificado %>%
  filter(modulo == "Módulo 1: Socio-Demográfico")


## ============================================================================
## 2. Preparar datos base
## ============================================================================

# Mantener solo observaciones con género definido (Hombre / Mujer)
dataset <- dataset %>%
  filter(p40 %in% c("Mujer", "Hombre"))

# Seleccionar variables relevantes del módulo
df_m1 <- dataset %>% select(all_of(vars_m1$codigo))

# Variables categóricas y continuas a describir
cat_to_describe <- c("edad_r2", "pais", "p3_agregado", "p5_agregado",
                     "p7_agregado", "p8_agregado", "p9_estrato3", "p40")

cont_to_describe <- c("p1edad")


## ============================================================================
## 3. Descripción por género
## ============================================================================

tabla_m1 <- describe_by_group(
  data = dataset,
  vars_cat = cat_to_describe,
  vars_cont = cont_to_describe,
  control_var = "p40",  # Variable de control: Género
  diccionario = diccionario_clasificado
)

# Mostrar y exportar
print(head(tabla_m1))
writexl::write_xlsx(tabla_m1, "output/m1/21102025_mod1_by_gender.xlsx")


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
print(head(tabla_m2))
writexl::write_xlsx(tabla_m2, "output/m1/21102025_mod1_by_ses.xlsx")


## ============================================================================
## 5. Descripción por modo de transporte principal
## ============================================================================

tabla_m3 <- describe_by_group(
  data = dataset,
  vars_cat = cat_to_describe,
  vars_cont = cont_to_describe,
  control_var = "p17_modo_agregado",  # Variable de control: Modo principal
  diccionario = diccionario_clasificado
)

# Mostrar y exportar
print(head(tabla_m3))
writexl::write_xlsx(tabla_m3, "output/m1/21102025_mod1_by_travel_mode.xlsx")



