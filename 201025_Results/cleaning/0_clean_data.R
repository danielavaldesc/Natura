###############################################################################
## ASIGNACIÓN ROBUSTA DE ETIQUETAS SEGÚN VARIABLES EXISTENTES
###############################################################################

library(tidyverse)
library(labelled)

# --- Cargar tu base limpia ya existente ---
dataset <- readxl::read_excel("Input\\BD Base Movilidad Cali 2025_V01_Cliente.xlsx") %>%
  janitor::clean_names()

# --- Diccionario ajustado (fragmento representativo; puedes ampliarlo) ---
diccionario <- tribble(
  ~codigo, ~descripcion,
  "factor", "FEX",
  "zona", "zona",
  "campo", "En hogares o intersección",
  "genero", "genero - encuesta",
  "filtro_a", "¿Usted es mayor de edad?",
  "edadr", "Edad rango",
  "filtro_b", "¿Actualmente trabaja como conductor o domiciliario?",
  "filtro_c", "¿Vive hace más de un año en Cali?",
  "p1edad", "¿Cuántos años cumplidos tiene?",
  "p2", "¿Usted nació en Colombia?",
  "p2_1", "¿En qué departamento de Colombia?",
  "p2_1_1", "¿En qué municipio de Colombia?",
  "p2_2", "¿En qué país nació?",
  "p2_2_1", "Otro país ¿cuál?",
  "p3", "Usted se autoreconoce como:",
  "p4", "¿Usted se autoreconoce como campesina/o?",
  "p5", "¿Cuál es el último nivel educativo alcanzado?",
  "p6", "¿Ha sido víctima del conflicto armado?",
  "p7", "¿A qué actividad se dedica principalmente?",
  "p7_1", "En el trabajo usted es:",
  "p7_2", "Días que trabaja presencialmente 0 (múltiples opciones)",
  "p7_2_1", "Días que trabaja presencialmente 1 (múltiples opciones)",
  "p7_2_2", "Días que trabaja presencialmente 2 (múltiples opciones)",
  "p7_2_3", "Días que trabaja presencialmente 3 (múltiples opciones)",
  "p7_2_4", "Días que trabaja presencialmente 4 (múltiples opciones)",
  "p7_3", "Días que estudia presencialmente 0 (múltiples opciones)",
  "p7_3_1", "Días que estudia presencialmente 1 (múltiples opciones)",
  "p7_3_2", "Días que estudia presencialmente 2 (múltiples opciones)",
  "p8", "¿Con quién vive actualmente?",
  "p9estrato", "Estrato de la vivienda",
  "p10", "Gasto mensual en transporte",
  "p11", "¿Personas que requieren cuidado permanente en el hogar?",
  "p11_1", "Responsable principal del cuidado",
  "p12", "Dificultades en la vida diaria (movilidad, audición, visión, etc.) 0 (múltiples opciones)",
  "p12_1", "Dificultades en la vida diaria (movilidad, audición, visión, etc.) 1 (múltiples opciones)",
  "p12_2", "Dificultades en la vida diaria (movilidad, audición, visión, etc.) 2 (múltiples opciones)",
  "p13", "¿Sabe conducir?",
  "p14", "¿Tiene licencia de conducción?",
  "p15", "Número de autos en el hogar",
  "p15_1", "Número de autos propios",
  "p16", "Número de motocicletas en el hogar",
  "p16_1", "Número de motocicletas propias",
  "p17", "Modo principal de transporte",
  "p17_v2_1", "Cilindraje del auto",
  "p17_v2_2", "Cilindraje de la motocicleta",
  "p17_v2_3", "Cilindraje de camiones pesados",
  "p17_v3", "Fuente de energía principal del vehículo",
  "p17_v3_otro", "Otra fuente de energía ¿cuál?",
  "p17_v4", "Modelo del vehículo",
  "p17_v5", "Municipio de matrícula del vehículo",
  "p17_v6", "Rol en el vehículo",
  "p17_v7", "Comparte el vehículo con otras personas",
  "p17_v7_1", "Número de personas con las que viaja",
  "p18", "Tiempo total de viaje (minutos)",
  "p18_p1", "Minutos caminando hasta el modo de transporte",
  "p18_p2", "Minutos de espera del transporte",
  "p18_p3", "Minutos de trayecto en transporte (arranca -> baja)",
  "p18_p4", "Minutos caminando después del descenso",
  "p18_c1", "Minutos de la caminata a su destino",
  "p19comuna", "Comuna donde vive",
  "p20", "Destino frecuente dentro o fuera de Cali",
  "p20_1", "Comuna del destino frecuente",
  "p20_1_otro", "Otra comuna de destino ¿cuál?",
  "p21", "Número de destinos distintos en un día típico",
  "p22", "Distancia promedio hogar–destino (km)",
  "p23", "Propósito principal de los viajes",
  "p23_1", "A quién acompaña o lleva frecuentemente",
  "p23_2", "Motivo de acompañamiento",
  "p24", "Nivel de satisfacción con el modo principal de transporte",
  "p25", "Razones de elección del modo de transporte 0 (múltiples)",
  "p25_1", "Razones de elección del modo de transporte 1 (múltiples)",
  "p25_2", "Razones de elección del modo de transporte 2 (múltiples)",
  "p25_3", "Razones de elección del modo de transporte 3 (múltiples)",
  "p25_4", "Razones de elección del modo de transporte 4 (múltiples)",
  "p26" , "Aspectos que menos le gustan del modo de transporte",
  "p27", "Situaciones que buscó evitar al elegir su modo 0 (múltiples)",
  "p27_1", "Situaciones que buscó evitar al elegir su modo 1 (múltiples)",
  "p27_2", "Situaciones que buscó evitar al elegir su modo 2 (múltiples)",
  "p27_3", "Situaciones que buscó evitar al elegir su modo 3 (múltiples)",
  "p27_4", "Situaciones que buscó evitar al elegir su modo 4 (múltiples)",
  "p27_5", "Situaciones que buscó evitar al elegir su modo 5 (múltiples)",
  "p27_6", "Situaciones que buscó evitar al elegir su modo 6 (múltiples)",
  "p27_7", "Situaciones que buscó evitar al elegir su modo 7 (múltiples)",
  "p27_8", "Situaciones que buscó evitar al elegir su modo 8 (múltiples)",
  "p27_9", "Situaciones que buscó evitar al elegir su modo 9 (múltiples)",
  "p27_10", "Situaciones que buscó evitar al elegir su modo 10 (múltiples)",
  "p27_otro", "Situaciones que buscó evitar al elegir su modo 11 (múltiples)",
  "p27_otro", "Situaciones que buscó evitar al elegir su modo 11 (múltiples)",
  "p28p28_1", "Importancia: costo de compra",
  "p28p28_2", "Importancia: costo de uso u operación",
  "p28p28_3", "Importancia: comodidad",
  "p28p28_4", "Importancia: tiempo de viaje",
  "p28p28_5", "Importancia: riesgo de robo o atraco",
  "p28p28_6", "Importancia: riesgo de acoso",
  "p28p28_7", "Importancia: riesgo de discriminación",
  "p28p28_8", "Importancia: nivel de emisiones (contaminación)",
  "p28p28_9", "Importancia: siniestralidad vial asociada",
  "p29", "Forma ideal o soñada de movilizarse",
  "p30", "Razón por la cual no usa ese medio ideal",
  "p31", "Mayor fuente de contaminación en la ciudad",
  "p32", "¿Usted considera que el nivel de contaminación generado por su forma de movilizarse actualmente es?",
  "p33", "Forma de movilidad que más contamina",
  "p33_otro" , "Forma de movilidad que más contamina (otro)",
  "p34", "Forma de movilidad que menos contamina",
  "p34_otro" , "Forma de movilidad que menos contamina (otro)",
  "p35", "Razón para que no haya más personas usando modos sostenibles",
  "p36", "Influencia de los amigos en la elección del transporte",
  "p37", "Influencia de la familia en la elección del transporte",
  "p38p38_1", "Ha recibido comentarios ofensivos o discriminatorios",
  "p38p38_2", "Le han mirado morbosamente el cuerpo",
  "p38p38_3", "Le han dicho piropos ofensivos",
  "p38p38_4", "Se le recargaron sin consentimiento",
  "p38p38_5", "Le hicieron sentir miedo",
  "p38p38_6", "La tocaron o manosearon sin consentimiento",
  "p38p38_7", "Le robaron o atracaron",
  "p38p38_99", "Otra situación",
  "p38_99_otro", "Otra situación (especifique)",
  "p39", "Lugar donde ocurrió la situación",
  "p39_otro", "Lugar donde ocurrió la situación (otro)",
  "p39_1", "Acciones tomadas luego de la situación 0 (múltiple)",
  "p39_1_1", "Acciones tomadas luego de la situación 1 (múltiple)",
  "p39_1_2", "Acciones tomadas luego de la situación 2 (múltiple)",
  "p39_1_otro", "Acciones tomadas luego de la situación (otro)",
  "p39_2", "Ha sido testigo de estas situaciones",
  "p40", "Con qué género se identifica",
  "p41", "¿Se identifica como parte de la comunidad LGTBIQ+?",
  "p47latitude", "Latitud",
  "p47longitude", "Longitud", 
  "p47altitude", "Altitud",
  "p47accuracy", "Accuracy"
  )


# 1. Coincidencias válidas
vars_existentes <- names(dataset)
diccionario_filtrado <- diccionario %>%
  filter(codigo %in% vars_existentes)

# 2. Crear lista nombrada (formato correcto)
etiquetas_lista <- as.list(diccionario_filtrado$descripcion)
names(etiquetas_lista) <- diccionario_filtrado$codigo

# 3. Aplicar etiquetas sólo a las variables que existen
for (var in names(etiquetas_lista)) {
  if (var %in% names(dataset)) {
    var_label(dataset[[var]]) <- etiquetas_lista[[var]]
  }
}

var_label(dataset)

###############################################################################
## IDENTIFICACIÓN DE CADA MÓDULO Y SUBMÓDULO
###############################################################################


# Definir vectores de clasificación
mod_socio <- c("zona", "campo", "genero", "filtro_a", "edadr", "filtro_b", "filtro_c",
               "p1edad", "p2", "p2_1", "p2_1_1", "p2_2", "p2_2_1", "p3", "p4",
               "p5", "p6", "p7", "p7_1", "p7_2", "p7_2_1", "p7_2_2", "p7_2_3",
               "p7_2_4", "p7_3", "p7_3_1", "p7_3_2", "p8", "p9estrato", "p10",
               "p11", "p11_1", "p12", "p12_1", "p12_2")

mod_movilidad_modo <- c("p13", "p14", "p15", "p15_1", "p16", "p16_1",
                        "p17", "p17_v2_1", "p17_v2_2", "p17_v2_3",
                        "p17_v3", "p17_v3_otro", "p17_v4", "p17_v5")

mod_movilidad_habitos <- c("p17_v6", "p17_v7", "p17_v7_1")

mod_movilidad_tiempo <- c("p18", "p18_p1", "p18_p2", "p18_p3", "p18_p4", "p18_c1")

mod_movilidad_caract <- c("p19comuna", "p20", "p20_1", "p20_1_otro",
                          "p21", "p22", "p23", "p23_1", "p23_2")

mod_percepciones_modo <- c("p24", "p25", "p25_1", "p25_2", "p25_3", "p25_4", "p26",
                           "p27", "p27_1", "p27_2", "p27_3", "p27_4", "p27_5",
                           "p27_6", "p27_7", "p27_8", "p27_9", "p27_10", "p27_otro",
                           "p28p28_1", "p28p28_2", "p28p28_3", "p28p28_4",
                           "p28p28_5", "p28p28_6", "p28p28_7", "p28p28_8", "p28p28_9")

mod_deseos <- c("p29", "p30")

mod_percepcion_ambiental <- c("p31", "p32", "p33", "p33_otro", "p34", "p34_otro", "p35")

mod_influencia <- c("p36", "p37")

mod_acoso_vbg <- c("p38p38_1", "p38p38_2", "p38p38_3", "p38p38_4",
                   "p38p38_5", "p38p38_6", "p38p38_7", "p38p38_99","p38_99_otro",
                   "p39", "p39_otro", "p39_1", "p39_1_1", "p39_1_2", "p39_1_otro", "p39_2")

mod_identidad <- c("p40", "p41")

mod_geo <- c("factor", "p47latitude", "p47longitude", "p47altitude", "p47accuracy")


# Clasificar todas las variables del diccionario
diccionario_clasificado <- diccionario %>%
  mutate(
    modulo = case_when(
      codigo %in% c(mod_socio, mod_identidad) ~ "Módulo 1: Socio-Demográfico",
      codigo %in% c(mod_movilidad_modo, mod_movilidad_habitos,
                    mod_movilidad_tiempo, mod_movilidad_caract) ~ "Módulo 2: Movilidad",
      codigo %in% c(mod_percepciones_modo, mod_deseos, mod_percepcion_ambiental) ~
        "Módulo 3: Percepciones, preferencias y deseos",
      codigo %in% mod_influencia ~ "Módulo 3: Percepciones, preferencias y deseos",
      codigo %in% mod_acoso_vbg ~ "Módulo 4: Experiencias de acoso, inseguridad y VBG",
      codigo %in% mod_geo ~ "Variables técnicas o geográficas",
      TRUE ~ NA_character_
    ),
    submodulo = case_when(
      codigo %in% mod_movilidad_modo ~ "Modo de transporte",
      codigo %in% mod_movilidad_habitos ~ "Hábitos en vehículos privados",
      codigo %in% mod_movilidad_tiempo ~ "Tiempo de viaje",
      codigo %in% mod_movilidad_caract ~ "Características del viaje",
      codigo %in% mod_percepciones_modo ~ "Percepciones sobre el modo",
      codigo %in% mod_deseos ~ "Deseos sobre la movilidad",
      codigo %in% mod_percepcion_ambiental ~ "Percepción del impacto ambiental",
      TRUE ~ NA_character_
    )
  )




