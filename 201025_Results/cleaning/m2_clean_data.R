###############################################################################
# MÓDULO 2: MOVILIDAD
# Descripción: Recodificación de variables relacionadas con tenencia de vehículos,
#              modos de transporte, características del viaje y propósito principal.
# Objetivo: Estandarizar categorías para análisis comparativo por género y grupos.
###############################################################################

## ============================================================================ 
## 0. Cargar entorno y configuración inicial
## ============================================================================ 

source("cleaning/m1_clean_data.R")

mod_mov <- diccionario_clasificado %>%
  filter(modulo == "Módulo 2: Movilidad")

vars <- mod_mov$codigo


## ============================================================================ 
## 1. Tenencia de vehículos (autos y motocicletas)
## ============================================================================ 

# --- Autos en el hogar
dataset <- dataset %>%
  mutate(
    p15_autos_agregado = case_when(
      p15 == "0" ~ "Sin autos",
      p15 == "1" ~ "1 auto",
      p15 %in% c("2", "3", "Más de 3") ~ "2 o más autos",
      TRUE ~ NA_character_
    )
  )

# --- Autos propios
dataset <- dataset %>%
  mutate(
    p15_1_autos_propios_agregado = case_when(
      p15_1 %in% c("0", NA) ~ "Sin autos propios",
      p15_1 == "1" ~ "1 auto propio",
      p15_1 %in% c("2", "3", "4", "5") ~ "2 o más autos propios",
      TRUE ~ NA_character_
    )
  )

# --- Motocicletas en el hogar
dataset <- dataset %>%
  mutate(
    p16_motos_agregado = case_when(
      p16 == "0" ~ "Sin motocicletas",
      p16 == "1" ~ "1 motocicleta",
      p16 %in% c("2", "3", "Más de 3") ~ "2 o más motocicletas",
      TRUE ~ NA_character_
    )
  )

# --- Motocicletas propias
dataset <- dataset %>%
  mutate(
    p16_1_motos_propias_agregado = case_when(
      p16_1 %in% c("0", NA) ~ "Sin motocicletas propias",
      p16_1 == "1" ~ "1 motocicleta propia",
      p16_1 %in% c("2", "3", "4", "5") ~ "2 o más motocicletas propias",
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 2. Modo principal de transporte
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    p17_modo_agregado = case_when(
      # Modos activos
      p17 %in% c("Caminata", "Bicicleta") ~ "Modo activo",
      
      # Transporte público
      p17 %in% c("Transporte público (MIO)", "Transporte público colectivo busetas") ~ "Transporte público",
      
      # Taxi y plataformas
      p17 %in% c("Taxi",
                 "Automóvil en plataforma (Ejemplo:Uber, Yango, Didi, InDriver)",
                 "Moto en plataforma (Ejemplo: Didi, Picap, Uber, otras)") ~ "Taxi / Plataforma",
      
      # Vehículos privados
      p17 %in% c("Automóvil", "Campero/ Camioneta (SUV)", "Van/Camioneta con platón") ~ "Auto privado",
      p17 %in% c("Moto 2T", "Moto 4T") ~ "Moto privada",
      
      # Transporte informal
      p17 %in% c("Moto taxi (moto ratón)", "Guala o pirata",
                 "Bicitaxi con motor", "Bicitaxi sin motor") ~ "Transporte informal",
      
      # Vehículo pesado
      p17 == "Camión / vehículo de carga" ~ "Vehículo pesado",
      
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 3. Cilindraje de vehículos
## ============================================================================ 

# --- Autos
dataset <- dataset %>%
  mutate(
    cilindraje_auto_agregado = case_when(
      p17 %in% c("Automóvil", "Campero/ Camioneta (SUV)", "Van/Camioneta con platón") ~ case_when(
        p17_v2_1 == "Menos de 1000 cc" ~ "Menos de 1000 cc",
        p17_v2_1 == "Entre 1000 y 1499 cc" ~ "1000 - 1499 cc",
        p17_v2_1 == "Entre 1500 y 1999 cc" ~ "1500 - 1999 cc",
        p17_v2_1 == "Entre 2000 y 2499 cc" ~ "2000 - 2499 cc",
        p17_v2_1 == "Entre 2500 y 2999 cc" ~ "2500 - 2999 cc",
        p17_v2_1 == "No aplica - (por ejemplo Vehículo eléctrico)" ~ "Eléctrico / No aplica",
        p17_v2_1 == "No sabe / No responde" ~ "No sabe / No responde",
        TRUE ~ NA_character_
      ),
      TRUE ~ "No aplica"
    )
  )

# --- Motocicletas
dataset <- dataset %>%
  mutate(
    cilindraje_moto_agregado = case_when(
      p17 %in% c("Moto 2T", "Moto 4T", "Moto taxi (moto ratón)") ~ case_when(
        p17_v2_2 == "Menos de 125 cc" ~ "Menos de 125 cc",
        p17_v2_2 == "125 cc" ~ "125 cc",
        p17_v2_2 == "150 cc" ~ "150 cc",
        p17_v2_2 == "Entre 150 cc y 250 cc" ~ "150 - 250 cc",
        p17_v2_2 == "Más de 250 cc" ~ "Más de 250 cc",
        p17_v2_2 == "No aplica - (por ejemplo moto eléctrica)" ~ "Eléctrica / No aplica",
        p17_v2_2 == "No sabe / No responde" ~ "No sabe / No responde",
        TRUE ~ NA_character_
      ),
      TRUE ~ "No aplica"
    )
  )

# --- Camiones (futuro)
dataset <- dataset %>%
  mutate(
    cilindraje_camion_agregado = case_when(
      p17 == "Camión / vehículo de carga" ~ coalesce(p17_v2_3, "No especificado"),
      TRUE ~ "No aplica"
    )
  )


## ============================================================================ 
## 4. Modelo del vehículo
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    p17_v4_num = suppressWarnings(as.numeric(p17_v4)),
    modelo_vehiculo_agregado = case_when(
      p17_modo_agregado %in% c("Auto privado", "Moto privada") &
        p17_v4 == "Modelos anteriores a 2005" ~ "Anterior a 2005",
      p17_modo_agregado %in% c("Auto privado", "Moto privada") &
        p17_v4_num >= 2005 & p17_v4_num <= 2010 ~ "2005 - 2010",
      p17_modo_agregado %in% c("Auto privado", "Moto privada") &
        p17_v4_num >= 2011 & p17_v4_num <= 2015 ~ "2011 - 2015",
      p17_modo_agregado %in% c("Auto privado", "Moto privada") &
        p17_v4_num >= 2016 & p17_v4_num <= 2020 ~ "2016 - 2020",
      p17_modo_agregado %in% c("Auto privado", "Moto privada") &
        p17_v4_num >= 2021 ~ "2021 o más reciente",
      p17_modo_agregado %in% c("Auto privado", "Moto privada") &
        p17_v4 %in% c("No sabe / No responde - NO LEA", NA) ~ "Sin información",
      !(p17_modo_agregado %in% c("Auto privado", "Moto privada")) ~ "No aplica",
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 5. Número de destinos distintos en un día típico
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    p21_destinos_agregado = case_when(
      p21 == "No salgo del hogar con frecuencia" ~ "No sale con frecuencia",
      p21 == "Uno" ~ "1 destino",
      p21 == "Dos" ~ "2 destinos",
      p21 %in% c("Tres", "Cuatro o más") ~ "3 o más destinos",
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 6. Propósito principal del viaje (armonizado y detallado)
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    p23_agregado = case_when(
      # Cuidado y familia
      p23 == "A acompañar o llevar a alguien" &
        p23_1 %in% c("Una niña o niño", "Un adolescente/jóven") &
        p23_2 == "Asistir a un centro educativo (colegio, jardín infantil)" ~
        "Cuidado y familia (centro educativo, niños/as o jóvenes)",
      p23 == "A acompañar o llevar a alguien" &
        p23_1 %in% c("Una niña o niño", "Un adolescente/jóven") &
        p23_2 == "Otro asunto" ~
        "Cuidado y familia (otro lugar, niños/as o jóvenes)",
      p23 == "A acompañar o llevar a alguien" &
        p23_1 %in% c("Una niña o niño", "Un adolescente/jóven") &
        is.na(p23_2) ~
        "Cuidado y familia (sin detalle, niños/as o jóvenes)",
      p23 == "A acompañar o llevar a alguien" & is.na(p23_1) ~
        "Cuidado y familia (sin especificar)",
      
      # Trabajo / estudio
      p23 %in% c("Ir a trabajar", "A buscar trabajo") ~ "Trabajo",
      p23 == "Ir a estudiar" ~ "Estudio",
      
      # Compras / trámites
      p23 %in% c("A llevar y/o dejar algo",
                 "A realizar algún trámite personal",
                 "A realizar compras") ~ "Compras y trámites",
      
      # Recreación, salud, personales
      p23 %in% c("A realizar actividades físicas y/o deportivas (ir al gym, trotar, entrenar)",
                 "A realizar actividades recreativas y culturales (ir a cine, a un concierto, una presentación etc)",
                 "A asistir a alguna actividad de tipo religioso y/o de culto (la iglesia)",
                 "A una cita médica, tomarse  examenes o reclamar medicamentos para usted mismo") ~
        "Recreación, salud y actividades personales",
      
      # Visitas
      p23 == "A visitar a alguien (familiar o amigo)" ~ "Visitas sociales",
      
      # Otro
      p23 == "A otro asunto" ~ "Otro",
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 7. Registrar variables derivadas en el diccionario clasificado
## ============================================================================ 

nuevas_vars_mod2 <- tribble(
  ~codigo, ~descripcion, ~modulo, ~submodulo,
  "p15_autos_agregado", "Tenencia de autos en el hogar (agregado)", "Módulo 2: Movilidad", NA,
  "p15_1_autos_propios_agregado", "Autos propios (agregado)", "Módulo 2: Movilidad", NA,
  "p16_motos_agregado", "Motocicletas en el hogar (agregado)", "Módulo 2: Movilidad", NA,
  "p16_1_motos_propias_agregado", "Motocicletas propias (agregado)", "Módulo 2: Movilidad", NA,
  "p17_modo_agregado", "Modo principal de transporte (agrupado)", "Módulo 2: Movilidad", NA,
  "cilindraje_auto_agregado", "Cilindraje de automóviles (agregado)", "Módulo 2: Movilidad", NA,
  "cilindraje_moto_agregado", "Cilindraje de motocicletas (agregado)", "Módulo 2: Movilidad", NA,
  "cilindraje_camion_agregado", "Cilindraje de camiones (futuro)", "Módulo 2: Movilidad", NA,
  "modelo_vehiculo_agregado", "Modelo del vehículo (agrupado)", "Módulo 2: Movilidad", NA,
  "p21_destinos_agregado", "Número de destinos distintos en un día típico (agregado)", "Módulo 2: Movilidad", NA,
  "p23_agregado", "Propósito principal del viaje (armonizado y detallado con grupos de cuidado: niños/as o jóvenes)", "Módulo 2: Movilidad", NA
)

diccionario_clasificado <- bind_rows(diccionario_clasificado, nuevas_vars_mod2) %>%
  distinct(codigo, .keep_all = TRUE)
