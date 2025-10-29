###############################################################################
# MÓDULO 4: EXPERIENCIAS DE ACOSO, INSEGURIDAD Y VBG
# Descripción: Recodificación de variables sobre acciones tomadas ante
#              situaciones de acoso, inseguridad o violencia basada en género.
# Objetivo: Estandarizar categorías de respuesta y clasificar acciones
#            posteriores al evento para análisis comparativo.
###############################################################################

## ============================================================================
## 0. Cargar entorno y configuración inicial
## ============================================================================

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\")

source("cleaning/m3_clean_data.R")

mod_vbg <- diccionario_clasificado %>%
  filter(modulo == "Módulo 4: Experiencias de acoso, inseguridad y VBG")

library(purrr)
library(stringr)
library(dplyr)

###############################################################################
## MÓDULO 4: Experiencias — Lugar donde ocurrió la situación (p39)
###############################################################################

dataset <- dataset %>%
  mutate(
    p39_lugar_agregado = case_when(
      # Transporte público masivo o colectivo
      p39 %in% c("En los buses del transporte público (metro)",
                 "En los paraderos o estaciones",
                 "En uno de los alimentadores") ~ "Transporte público / estaciones",
      
      # Transporte intermunicipal
      p39 %in% c("En un bus de transporte intermunicipal") ~ "Transporte intermunicipal",
      
      # Transporte escolar
      p39 == "En la ruta escolar" ~ "Transporte escolar",
      
      # Transporte informal
      p39 %in% c("En un jeep (guala)",
                 "En un motoratón") ~ "Transporte informal",
      
      # Transporte privado o individual
      p39 %in% c("En un taxi",
                 "En un vehículo de aplicación Uber, Cabify,",
                 "En una motocicleta") ~ "Transporte privado o individual",
      
      # Entorno peatonal
      p39 %in% c("Mientras caminaba",
                 "Entre el paradero y la casa") ~ "Entorno peatonal / calle",
      
      # Otro
      p39 == "Otro" ~ "Otro lugar",
      
      TRUE ~ NA_character_
    )
  )


###############################################################################
## Actualizar diccionario de variables
###############################################################################

diccionario_clasificado <- diccionario_clasificado %>%
  add_row(
    codigo = "p39_lugar_agregado",
    descripcion = "Lugar donde ocurrió la situación (categorías agrupadas)",
    modulo = "Módulo 4: Experiencias de acoso, inseguridad y VBG"
  )



###############################################################################
## Guardar diccionario y dataset
###############################################################################

writexl::write_xlsx(dataset, "output/clean_med_dataset_27102025.xlsx")
writexl::write_xlsx(diccionario_clasificado, "output/diccionario_med.xlsx")


