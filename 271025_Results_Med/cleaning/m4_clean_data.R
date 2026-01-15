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

source("cleaning/m3_clean_data.R", encoding = "UTF-8")

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
    p38p38_dummy = case_when(
      # Si hay al menos un "Sí"
      if_any(starts_with("p38p38_"), ~ .x == "Si") ~ 1,
      
      # Si todas son "No" o "No sabe"
      if_all(starts_with("p38p38_"), ~ .x %in% c("No", "No sabe")) ~ 0,
      
      # Si todo es NA (sin respuestas)
      if_all(starts_with("p38p38_"), is.na) ~ NA_real_,
      
      TRUE ~ NA_real_
    )
  )


dataset <- dataset %>%
  mutate(
    # -------------------------
    # p39_lugar_agregado (detallado)
    # -------------------------
    p39_lugar_agregado = case_when(
      # Transporte público masivo o colectivo
      p39 %in% c(
        "En los buses del transporte público (metro)",
        "En los paraderos o estaciones",
        "En uno de los alimentadores"
      ) ~ "Transporte público / estaciones",
      
      # Transporte intermunicipal
      p39 == "En un bus de transporte intermunicipal" ~ "Transporte intermunicipal",
      
      # Transporte escolar
      p39 == "En la ruta escolar" ~ "Transporte escolar",
      
      # Transporte informal
      p39 %in% c("En un jeep (guala)", "En un motoratón") ~ "Transporte informal",
      
      # Transporte privado o individual
      p39 %in% c(
        "En un taxi",
        "En un vehículo de aplicación Uber, Cabify,",
        "En una motocicleta"
      ) ~ "Transporte privado o individual",
      
      # Entorno peatonal
      p39 %in% c("Mientras caminaba", "Entre el paradero y la casa") ~ "Entorno peatonal / calle",
      
      # Otro
      p39 == "Otro" ~ "Otro lugar",
      
      TRUE ~ NA_character_
    ),
    
    # -------------------------
    # p39_lugar_agregado_mod (agrupado en 2 categorías)
    # -------------------------
    p39_lugar_agregado_mod = case_when(
      p39_lugar_agregado %in% c(
        "Transporte público / estaciones",
        "Transporte intermunicipal",
        "Transporte escolar",
        "Transporte informal",
        "Transporte privado o individual"
      ) ~ "En su modo de transporte",
      
      p39_lugar_agregado %in% c(
        "Entorno peatonal / calle",
        "Otro lugar"
      ) ~ "En otro lugar",
      
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
  ) %>%
  add_row(
    codigo = "p39_lugar_agregado_mod",
    descripcion = "Variable dicotómica: en su modo de transporte o en otro lugar",
    modulo = "Módulo 4: Experiencias de acoso, inseguridad y VBG"
  ) %>%
  add_row(
    codigo = "p38p38_dummy",
    descripcion = "Dummy: 1 si vivió al menos una situación de acoso/inseguridad, 0 si no vivió ninguna",
    modulo = "Módulo 4: Experiencias de acoso, inseguridad y VBG"
  )

###############################################################################
## Guardar diccionario y dataset
###############################################################################

writexl::write_xlsx(dataset, "output/clean_med_dataset_27102025.xlsx")
writexl::write_xlsx(diccionario_clasificado, "output/diccionario_med.xlsx")

