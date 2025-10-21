###############################################################################
# MÓDULO 3: PERCEPCIONES, PREFERENCIAS Y DESEOS
# Descripción: Recodificación de variables relacionadas con tenencia de vehículos,
#              modos de transporte, características del viaje y propósito principal.
# Objetivo: Estandarizar categorías para análisis comparativo por género y grupos.
###############################################################################

## ============================================================================
## 0. Cargar entorno y configuración inicial
## ============================================================================

source("cleaning/m2_clean_data.R")

mod_percepciones <- diccionario_clasificado %>%
  filter(modulo == "Módulo 3: Percepciones, preferencias y deseos")

vars <- mod_percepciones$codigo

## ============================================================================
## 1. Nivel de satisfacción (p24) — Escala Likert 1 a 5
## ============================================================================

dataset <- dataset %>%
  mutate(
    p24 = case_when(
      as.character(p24) %in% c("1", "Nada Satisfecho", "nada satisfecho") ~ 1,
      as.character(p24) %in% c("2", "Poco Satisfecho", "poco satisfecho") ~ 2,
      as.character(p24) %in% c("3", "Satisfecho", "satisfecho") ~ 3,
      as.character(p24) %in% c("4", "Muy Satisfecho", "muy satisfecho") ~ 4,
      as.character(p24) %in% c("5", "Totalmente satisfecho", "totalmente satisfecho") ~ 5,
      TRUE ~ NA_real_
    )
  )


###############################################################################
## MÓDULO 2: Movilidad — Razones de elección del modo de transporte (p25)
###############################################################################

library(purrr)
library(stringr)
library(dplyr)

# 1. Unificar respuestas múltiples
dataset <- dataset %>%
  mutate(
    razones_transporte = pmap_chr(
      select(., p25, p25_1, p25_2, p25_3, p25_4),
      ~ c(...) %>% discard(is.na) %>% unique() %>% paste(collapse = ", ")
    )
  )

# 2. Versión textual (preservar naturaleza múltiple)
dataset <- dataset %>%
  mutate(
    p25_razones_multiples = case_when(
      razones_transporte == "" ~ NA_character_,
      TRUE ~ razones_transporte
    )
  )

# 3. Variables binarias
dataset <- dataset %>%
  mutate(
    p25_costo        = if_else(str_detect(razones_transporte, "costo|posibilidades económicas|capacidad adquisitiva"), 1, 0, missing = 0),
    p25_tiempo       = if_else(str_detect(razones_transporte, "tiempo"), 1, 0, missing = 0),
    p25_comodidad    = if_else(str_detect(razones_transporte, "comodidad|confort"), 1, 0, missing = 0),
    p25_autonomia    = if_else(str_detect(razones_transporte, "autonomía|control"), 1, 0, missing = 0),
    p25_seguridad    = if_else(str_detect(razones_transporte, "seguridad"), 1, 0, missing = 0),
    p25_distancia    = if_else(str_detect(razones_transporte, "distancia"), 1, 0, missing = 0),
    p25_salud        = if_else(str_detect(razones_transporte, "salud"), 1, 0, missing = 0),
    p25_ambiental    = if_else(str_detect(razones_transporte, "ambiental"), 1, 0, missing = 0),
    p25_restriccion  = if_else(str_detect(razones_transporte, "restricciones|pico y placa"), 1, 0, missing = 0),
    p25_otro         = if_else(str_detect(razones_transporte, "otro"), 1, 0, missing = 0)
  )

# 4. Variable resumen
dataset <- dataset %>%
  mutate(
    p25_razones_agregadas = paste0(
      ifelse(p25_costo == 1, "Costo económico, ", ""),
      ifelse(p25_tiempo == 1, "Tiempo de viaje, ", ""),
      ifelse(p25_comodidad == 1, "Comodidad / confort, ", ""),
      ifelse(p25_autonomia == 1, "Autonomía / control, ", ""),
      ifelse(p25_seguridad == 1, "Percepción de seguridad, ", ""),
      ifelse(p25_distancia == 1, "Distancia, ", ""),
      ifelse(p25_salud == 1, "Condiciones de salud, ", ""),
      ifelse(p25_ambiental == 1, "Motivos ambientales, ", ""),
      ifelse(p25_restriccion == 1, "Restricciones, ", ""),
      ifelse(p25_otro == 1, "Otro motivo, ", "")
    ) %>%
      str_remove(", $") %>%
      na_if("")
  )


###############################################################################
## MÓDULO 2: Movilidad — Aspectos que menos le gustan del modo de transporte (p26)
###############################################################################

dataset <- dataset %>%
  mutate(
    p26_agregado = case_when(
      p26 %in% c("El costo de compra", "El costo de uso u operación") ~ "Costo económico",
      p26 %in% c("El tiempo de espera", "El tiempo de viaje") ~ "Tiempo de viaje / espera",
      p26 %in% c("Las condiciones de incomodidad", "La exposición a condiciones climáticas desfavorables (lluvia o calor)") ~ "Incomodidad / clima",
      p26 == "La falta de autonomía o control sobre el viaje" ~ "Falta de autonomía / control",
      p26 == "La percepción de inseguridad personal (robo o atraco, acoso o violencia de algún tipo)" ~ "Inseguridad personal",
      p26 == "La vulnerabilidad frente accidentes de tránsito" ~ "Riesgo de accidente",
      p26 == "El nivel de emisiones (contaminación)" ~ "Impacto ambiental",
      p26 == "Nada me disgusta" ~ "Nada le disgusta",
      p26 %in% c("No sabe/ No responde") ~ "Sin respuesta",
      p26 == "Otra razón" ~ "Otro motivo",
      TRUE ~ NA_character_
    )
  )


###############################################################################
## MÓDULO 2: Movilidad — Situaciones que buscó evitar al elegir su modo (p27)
###############################################################################

dataset <- dataset %>%
  mutate(
    situaciones_evitar = pmap_chr(
      select(., starts_with("p27")),
      ~ c(...) %>% discard(is.na) %>% unique() %>% paste(collapse = ", ")
    ),
    p27_situaciones_multiples = na_if(trimws(situaciones_evitar), "")
  ) %>%
  mutate(
    p27_costo       = if_else(str_detect(str_to_lower(situaciones_evitar), "costo|econ[oó]mic|parqueadero|m[aá]s econ[oó]mic|capacidad adquisitiva"), 1, 0, missing = 0),
    p27_congestion  = if_else(str_detect(str_to_lower(situaciones_evitar), "congesti[oó]n|tr[aá]fico|espera|viaje|tiempo"), 1, 0, missing = 0),
    p27_fisico      = if_else(str_detect(str_to_lower(situaciones_evitar), "esfuerzo|f[ií]sico|clim[aá]tic|lluvia|calor|desfavorables"), 1, 0, missing = 0),
    p27_seguridad   = if_else(str_detect(str_to_lower(situaciones_evitar), "robo|atraco|acoso|violenc|discriminaci[oó]n|riesg|accident|inseguridad"), 1, 0, missing = 0),
    p27_comodidad   = if_else(str_detect(str_to_lower(situaciones_evitar), "multitud|aglomeraci[oó]n|personas"), 1, 0, missing = 0),
    p27_ambiental   = if_else(str_detect(str_to_lower(situaciones_evitar), "ambient|emision|contaminaci[oó]n|ecol[oó]gic"), 1, 0, missing = 0),
    p27_otro        = if_else(str_detect(str_to_lower(situaciones_evitar), "otro|ningun|ninguna|nada|no sabe|no aplica|es lo que hay|porque era [úu]til|comodidad|v[ií]as|horarios|autonom[ií]a|molesta|servicio|accidente|todos los riesgos"), 1, 0, missing = 0)
  )


###############################################################################
## MÓDULO 2: Movilidad — Influencia social en la elección del transporte (p36, p37)
###############################################################################

dataset <- dataset %>%
  mutate(
    p36_influencia_amigos = case_when(
      as.character(p36) == "Nada Importante" ~ 1,
      as.character(p36) == "2" ~ 2,
      as.character(p36) == "3" ~ 3,
      as.character(p36) == "4" ~ 4,
      as.character(p36) == "Muy importante" ~ 5,
      TRUE ~ NA_real_
    ),
    p37_influencia_familia = case_when(
      as.character(p37) == "Nada Importante" ~ 1,
      as.character(p37) == "2" ~ 2,
      as.character(p37) == "3" ~ 3,
      as.character(p37) == "4" ~ 4,
      as.character(p37) == "Muy importante" ~ 5,
      TRUE ~ NA_real_
    )
  )


###############################################################################
## Actualización del diccionario
###############################################################################

diccionario_clasificado <- diccionario_clasificado %>%
  add_row(
    codigo = c(
      "p25_razones_agregadas", "p26_agregado", "p27_situaciones_multiples",
      "p28_importancia_costo_compra", "p28_importancia_costo_uso", "p28_importancia_comodidad",
      "p28_importancia_tiempo", "p28_importancia_riesgo_robo", "p28_importancia_riesgo_acoso",
      "p28_importancia_discriminacion", "p28_importancia_emisiones", "p28_importancia_siniestralidad",
      "p29_modo_ideal_agregado", "p30_razon_no_uso_agregado", "p31_fuente_contaminacion_agregada",
      "p32_contaminacion_likert", "p33_modo_contaminante_agregado", "p35_razon_agregada",
      "p36_influencia_amigos", "p37_influencia_familia"
    ),
    descripcion = c(
      "Razones agregadas de elección del modo de transporte",
      "Aspectos negativos principales del modo de transporte",
      "Situaciones múltiples que se buscó evitar",
      "Importancia del costo de compra",
      "Importancia del costo de uso u operación",
      "Importancia de la comodidad",
      "Importancia del tiempo de viaje",
      "Importancia del riesgo de robo o atraco",
      "Importancia del riesgo de acoso",
      "Importancia del riesgo de discriminación",
      "Importancia del nivel de emisiones",
      "Importancia de la siniestralidad vial",
      "Modo ideal o soñado de movilizarse (agregado)",
      "Razón para no usar el modo ideal (agregada)",
      "Mayor fuente de contaminación percibida",
      "Nivel percibido de contaminación (escala Likert)",
      "Modo de movilidad que más contamina (agregado)",
      "Razón por la cual no hay más personas usando modos sostenibles (agregada)",
      "Influencia de los amigos en la elección del transporte (Likert 1-5)",
      "Influencia de la familia en la elección del transporte (Likert 1-5)"
    ),
    modulo = "Módulo 3: Percepciones, preferencias y deseos"
  )
