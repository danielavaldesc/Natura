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

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\201025_Results_Cali\\")

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
    p39_norm = str_squish(str_to_lower(p39)),
    p39_lugar_agregado = case_when(
      p39_norm %in% str_to_lower(c(
        "En los buses del transporte público (metro)",
        "En los paraderos o estaciones",
        "En uno de los alimentadores"
      )) ~ "Transporte público / estaciones",
      
      p39_norm == "en un bus de transporte intermunicipal" ~ "Transporte intermunicipal",
      p39_norm == "en la ruta escolar" ~ "Transporte escolar",
      p39_norm %in% str_to_lower(c("En un jeep (guala)", "En un motoratón")) ~ "Transporte informal",
      p39_norm %in% str_to_lower(c("En un taxi", "En un vehículo de aplicación Uber, Cabify,", "En una motocicleta")) ~ "Transporte privado o individual",
      p39_norm %in% str_to_lower(c("Mientras caminaba", "Entre el paradero y la casa")) ~ "Entorno peatonal / calle",
      p39_norm == "otro" ~ "Otro lugar",
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

writexl::write_xlsx(dataset, "output/clean_cali_dataset_21102025.xlsx")
writexl::write_xlsx(diccionario_clasificado, "output/diccionario_cali.xlsx")


