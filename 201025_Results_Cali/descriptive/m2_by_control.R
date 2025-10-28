###############################################################################
# DESCRIPTIVE SUMMARIES BY GROUP
# Módulo 2: Movilidad
# Descripción:
#   Este script genera tablas descriptivas (n(%), mediana [Q1–Q3]) 
#   para las variables del Módulo 2, diferenciadas por:
#     1. Género
#     2. Estrato socioeconómico
#     3. Modo de transporte principal
#     4. Edad
#     5. Nivel de educación
#     6. Ocupación
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

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\")

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
## 3. Tablas descriptivas
## ============================================================================
# Asegura carpeta de salida
dir.create("output/m2", recursive = TRUE, showWarnings = FALSE)

# Mapa: variable de control -> sufijo de archivo
control_vars_m2 <- c(
  "p40"               = "by_gender",        # Género
  "p9_estrato3"       = "by_ses",           # Estrato
  "p17_modo_agregado" = "by_travel_mode",   # Modo principal
  "p5_agregado"       = "by_education",     # Nivel educativo agrupado
  "p7_agregado"       = "by_main_activity", # Actividad principal
  "edad_r2"           = "by_age_r"          # Grupos de edad 
)

# Ejecuta y exporta 
resultados_m2 <- lapply(names(control_vars_m2), function(ctrl) {
  tabla <- describe_by_group(
    data        = dataset,
    vars_cat    = cat_to_describe,
    vars_cont   = cont_to_describe,
    control_var = ctrl,
    diccionario = diccionario_clasificado
  )
  out_path <- sprintf("output/m2/21102025_mod2_%s.xlsx", control_vars_m2[[ctrl]])
  writexl::write_xlsx(tabla, out_path)
  tabla
})



