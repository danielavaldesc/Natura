###############################################################################
# DESCRIPTIVE SUMMARIES BY GROUP
# Módulo 1: Socio-Demográfico
# Descripción:
#   Este script genera tablas descriptivas (n(%), mediana [Q1–Q3]) 
#   para las variables del Módulo 1, diferenciadas por:
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

## ============================================================================
## 1. Cargar datos y diccionario
## ============================================================================

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\")

dataset <- read_excel("output/clean_med_dataset_27102025.xlsx")
diccionario_clasificado <- read_excel("output/diccionario_med.xlsx")

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
df_m1 <- dataset %>% dplyr::select(any_of(vars_m1$codigo))

# Variables categóricas y continuas a describir
cat_to_describe <- c("edad_r2", "pais", "p3_agregado", "p5_agregado",
                     "p7_agregado", "p8_agregado", "p9_estrato3", "p40")

cont_to_describe <- c("p1edad")


## ============================================================================
## 3. Tablas descriptivas
## ============================================================================

# Asegura carpeta de salida
dir.create("output/m1", recursive = TRUE, showWarnings = FALSE)

# Mapa: control_var
control_vars <- c(
  "p40"               = "by_gender",        # Género
  "p9_estrato3"       = "by_ses",           # Estrato
  "p17_modo_agregado" = "by_travel_mode",   # Modo principal
  "p5_agregado"       = "by_education",     # Nivel educativo agrupado
  "p7_agregado"       = "by_main_activity",  # Actividad principal
  "edad_r2"           = "by_age_r"           # Edad
)

# Función tablas 
resultados <- lapply(names(control_vars), function(ctrl) {
  tabla <- describe_by_group(
    data        = dataset,
    vars_cat    = cat_to_describe,
    vars_cont   = cont_to_describe,
    control_var = ctrl,
    diccionario = diccionario_clasificado
  )
  print(head(tabla))
  out_path <- sprintf("output/m1/27102025_mod1_%s.xlsx", control_vars[[ctrl]])
  writexl::write_xlsx(tabla, out_path)
  tabla
})



