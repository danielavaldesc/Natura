###############################################################################
# MÓDULO 3: PERCEPCIONES, PREFERENCIAS Y DESEOS
# Descripción: Recodificación de variables relacionadas con percepciones,
#              preferencias y factores de decisión en la movilidad.
# Objetivo: Estandarizar categorías para análisis comparativo por género y grupos.
###############################################################################

## ============================================================================
## 0. Cargar entorno y configuración inicial
## ============================================================================

source("cleaning/m2_clean_data.R")

mod_percepciones <- diccionario_clasificado %>%
  filter(modulo == "Módulo 3: Percepciones, preferencias y deseos")

vars <- mod_percepciones$codigo

library(purrr)
library(stringr)
library(dplyr)

## ============================================================================
## 1. Nivel de satisfacción (p24)
## ============================================================================

dataset <- dataset %>%
  mutate(
    p24 = case_when(
      as.character(p24) %in% c("1", "Nada Satisfecho") ~ 1,
      as.character(p24) %in% c("2", "Poco Satisfecho") ~ 2,
      as.character(p24) %in% c("3", "Satisfecho") ~ 3,
      as.character(p24) %in% c("4", "Muy Satisfecho") ~ 4,
      as.character(p24) %in% c("5", "Totalmente satisfecho") ~ 5,
      TRUE ~ NA_real_
    )
  )


## ============================================================================
## 2. Razones de elección del modo de transporte (p25)
## ============================================================================

dataset <- dataset %>%
  mutate(
    razones_transporte = pmap_chr(
      dplyr::select(., p25, p25_1),
      ~ c(...) %>% discard(is.na) %>% unique() %>% paste(collapse = ", ")
    ),
    p25_razones_agregadas = na_if(trimws(razones_transporte), "")
  )


## ============================================================================
## 3. Aspectos que menos le gustan del modo (p26)
## ============================================================================

dataset <- dataset %>%
  mutate(
    p26_agregado = case_when(
      p26 %in% c("El costo de compra", "El costo de uso u operación") ~ "Costo económico",
      p26 %in% c("El tiempo de espera", "El tiempo de viaje") ~ "Tiempo de viaje / espera",
      p26 %in% c("Las condiciones de incomodidad", 
                 "La exposición a condiciones climáticas desfavorables (lluvia o calor)") ~ "Incomodidad / clima",
      p26 == "La falta de autonomía o control sobre el viaje" ~ "Falta de autonomía / control",
      p26 == "La percepción de inseguridad personal (robo o atraco, acoso o violencia de algún tipo)" ~ "Inseguridad personal",
      p26 == "La vulnerabilidad frente accidentes de tránsito" ~ "Riesgo de accidente",
      p26 == "El nivel de emisiones (contaminación)" ~ "Impacto ambiental",
      p26 == "Nada me disgusta" ~ "Nada le disgusta",
      p26 == "No sabe/ No responde" ~ "Sin respuesta",
      p26 == "Otra razón" ~ "Otro motivo",
      TRUE ~ NA_character_
    )
  )


## ============================================================================
## 4. Situaciones que buscó evitar (p27)
## ============================================================================

dataset <- dataset %>%
  mutate(
    situaciones_evitar = pmap_chr(
      dplyr::select(., starts_with("p27")),
      ~ c(...) %>% discard(is.na) %>% unique() %>% paste(collapse = ", ")
    ),
    p27_situaciones_multiples = na_if(trimws(situaciones_evitar), "")
  )


## ============================================================================
## 5. Importancia de distintos factores (p28_1 a p28_9)
###############################################################################

dataset <- dataset %>%
  mutate(
    p28_importancia_costo_compra = case_when(
      as.character(p28p28_1) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_1) %in% c("2") ~ 2,
      as.character(p28p28_1) %in% c("3") ~ 3,
      as.character(p28p28_1) %in% c("4") ~ 4,
      as.character(p28p28_1) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_costo_uso = case_when(
      as.character(p28p28_2) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_2) %in% c("2") ~ 2,
      as.character(p28p28_2) %in% c("3") ~ 3,
      as.character(p28p28_2) %in% c("4") ~ 4,
      as.character(p28p28_2) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_comodidad = case_when(
      as.character(p28p28_3) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_3) %in% c("2") ~ 2,
      as.character(p28p28_3) %in% c("3") ~ 3,
      as.character(p28p28_3) %in% c("4") ~ 4,
      as.character(p28p28_3) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_tiempo = case_when(
      as.character(p28p28_4) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_4) %in% c("2") ~ 2,
      as.character(p28p28_4) %in% c("3") ~ 3,
      as.character(p28p28_4) %in% c("4") ~ 4,
      as.character(p28p28_4) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_riesgo_robo = case_when(
      as.character(p28p28_5) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_5) %in% c("2") ~ 2,
      as.character(p28p28_5) %in% c("3") ~ 3,
      as.character(p28p28_5) %in% c("4") ~ 4,
      as.character(p28p28_5) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_riesgo_acoso = case_when(
      as.character(p28p28_6) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_6) %in% c("2") ~ 2,
      as.character(p28p28_6) %in% c("3") ~ 3,
      as.character(p28p28_6) %in% c("4") ~ 4,
      as.character(p28p28_6) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_discriminacion = case_when(
      as.character(p28p28_7) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_7) %in% c("2") ~ 2,
      as.character(p28p28_7) %in% c("3") ~ 3,
      as.character(p28p28_7) %in% c("4") ~ 4,
      as.character(p28p28_7) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_emisiones = case_when(
      as.character(p28p28_8) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_8) %in% c("2") ~ 2,
      as.character(p28p28_8) %in% c("3") ~ 3,
      as.character(p28p28_8) %in% c("4") ~ 4,
      as.character(p28p28_8) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_siniestralidad = case_when(
      as.character(p28p28_9) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_9) %in% c("2") ~ 2,
      as.character(p28p28_9) %in% c("3") ~ 3,
      as.character(p28p28_9) %in% c("4") ~ 4,
      as.character(p28p28_9) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    )
  )


## ============================================================================
## 6. Modo ideal de transporte (p29)
## ============================================================================

dataset <- dataset %>%
  mutate(
    p29_modo_ideal_agregado = case_when(
      str_detect(str_to_lower(p29), "auto|carro") ~ "Automóvil",
      p29 == "Van" ~ "Automóvil",
      str_detect(str_to_lower(p29), "moto") ~ "Motocicleta",
      str_detect(str_to_lower(p29), "bici") ~ "Bicicleta",
      str_detect(str_to_lower(p29), "bus|mio|transporte") ~ "Transporte público",
      str_detect(str_to_lower(p29), "caminar|a pie") ~ "Caminar",
      p29 == "Caminata" ~ "Caminar",
      str_detect(str_to_lower(p29), "otro") ~ "Otro",
      p29 == "Camión liviano" ~ "Otro",
      p29 == "Taxi" ~ "Taxi",
      TRUE ~ NA_character_
    )
  )


## ============================================================================
## 7. Razón de no uso del modo ideal (p30)
## ============================================================================

dataset <- dataset %>%
  mutate(
    p30_razon_no_uso_agregado = case_when(
      str_detect(str_to_lower(p30), "dinero|cost") ~ "Limitaciones económicas",
      str_detect(str_to_lower(p30), "seguridad|acoso|robo|violenc") ~ "Inseguridad / acoso",
      p30 == "Nivel de siniestralidad vial" ~ "Inseguridad / acoso",
      str_detect(str_to_lower(p30), "infraestructura|anden|cicloruta|distancia") ~ "Falta de infraestructura / distancia",
      p30 == "No hay cobertura del modo de transporte" ~ "Falta de infraestructura / distancia",
      str_detect(str_to_lower(p30), "salud|edad") ~ "Condiciones físicas / salud",
      str_detect(str_to_lower(p30), "tiempo") ~ "Tiempo / disponibilidad",
      p30 == "Es el medio de transporte que utiliza" ~ "Modo actual",
      str_detect(str_to_lower(p30), "otro") ~ "Otro motivo",
      p30 == "Otra razón ¿Cuál?" ~ "Otro motivo",
      TRUE ~ NA_character_
    )
  )


## ============================================================================
## 8. Fuente percibida de contaminación (p31)
## ============================================================================

dataset <- dataset %>%
  mutate(
    p31_fuente_contaminacion_agregada = case_when(
      str_detect(str_to_lower(p31), "veh[ií]culos|carro|bus|moto|camiones") ~ "Vehículos motorizados",
      str_detect(str_to_lower(p31), "f[aá]bricas|industrias") ~ "Industria/Obras",
      p31 %in% c("Construcciones y obras civiles o demoliciones",
                 "Empresas manufactureras") ~ "Industria/Obras",
      str_detect(str_to_lower(p31), "basura|quema|residuos") ~ "Quema de residuos",
      str_detect(str_to_lower(p31), "otro") ~ "Otra fuente",
      p31 == "No sabe /no responde" ~ "Otra fuente",
      p31 == "Uso de aerosoles y productos químicos" ~ "Productos químicos",
      p31 == "Vertederos (basureros) y rellenos sanitarios" ~ "Vertederos (basureros) y rellenos sanitarios",
      TRUE ~ NA_character_
    )
  )


## ============================================================================
## 9. Nivel percibido de contaminación (p32)
## ============================================================================

dataset <- dataset %>%
  mutate(
    p32_contaminacion_likert = case_when(
      as.character(p32) %in% c("1", "Muy baja", "Nulo") ~ 1,
      as.character(p32) %in% c("2", "Baja", "Bajo") ~ 2,
      as.character(p32) %in% c("3", "Moderada", "Moderado") ~ 3,
      as.character(p32) %in% c("4", "Alta", "Alto") ~ 4,
      as.character(p32) %in% c("5", "Muy alta", "Muy alto") ~ 5,
      TRUE ~ NA_real_
    )
  )


## ============================================================================
## 10. Modo percibido como más contaminante (p33)
## ============================================================================

dataset <- dataset %>%
  mutate(
    p33_modo_contaminante_agregado = case_when(
      str_detect(str_to_lower(p33), "cam[ií]on") ~ "Camión",
      str_detect(str_to_lower(p33), "auto|carro|taxi") ~ "Automóvil",
      str_detect(str_to_lower(p33), "moto") ~ "Motocicleta",
      str_detect(str_to_lower(p33), "bus|mio") ~ "Transporte público",
      str_detect(str_to_lower(p33), "otro|patineta") ~ "Otro modo",
      str_detect(str_to_lower(p33), "camión") ~ "Camión",
      TRUE ~ NA_character_
    )
  )


## ============================================================================
## 11. Razón por la cual no hay más personas usando modos sostenibles (p35)
## ============================================================================

dataset <- dataset %>%
  mutate(
    p35_razon_agregada = case_when(
      str_detect(str_to_lower(p35), "inseguridad|violenc|robo|acoso") ~ "Inseguridad / violencia",
      str_detect(str_to_lower(p35), "infraestructura|anden|cicloruta|terreno") ~ "Falta de infraestructura",
      str_detect(str_to_lower(p35), "clim|lluvia|calor") ~ "Condiciones climáticas",
      str_detect(str_to_lower(p35), "informaci") ~ "Falta de información",
      str_detect(str_to_lower(p35), "costo|econ") ~ "Costos altos",
      str_detect(str_to_lower(p35), "salud|edad") ~ "Condiciones físicas / salud",
      str_detect(str_to_lower(p35), "otro") ~ "Otro motivo",
      TRUE ~ NA_character_
    )
  )


## ============================================================================
## 12. Influencia social (p36, p37)
## ============================================================================

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
## 13. Actualización del diccionario
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

###############################################################################
## FIN DEL SCRIPT DE LIMPIEZA — MÓDULO 3
###############################################################################
