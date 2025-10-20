
#############################################
#############################################
## Procesamiento: Base de datos: Medellín ##
#############################################
#############################################
#############################################

# Cargar librerías
library(tidyverse)
library(readxl)
library(reshape2)
library(tidyr)
library(dplyr)
library(janitor)
library(cli)
library(openxlsx)
library(writexl)

# Cargar base de datos inicial
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\")

dataset <- readxl::read_excel("Input\\BD Base Movilidad Medellin 2025_Cliente.xlsx")


##---------------------------------------------##
## Primera parte: recodificación de variables  ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##---------------------------------------------##
cat <- c("filtro_a", "filtro_b", "filtro_c", "Edadr", "p2",	
         "p2_1", "p2_1_1", "p2_2", "p2_2_1", "p3", "p4", "p5", 
         "p6", "p7", "p7_1", "p7_2", "p7_2_1", "p7_2_2", "p7_2_3",
         "p7_2_4", "p7_3", "p7_3_1", "p7_3_2", "p8", "p9Estrato", "p10", "p11", 
         "p11_1", "p12", "p12.1", "p12.2", "p13", "p14", "p15", "p15_1", 
         "p16", "p16_1", "p17", "p17_v2_1", "p17_v2_2", "p17_v2_3", "p17_v3", 
         "p17_v3_otro", "p17_v4", "p17_v5", "p17_v6", "p17_v7", "p17_v7_1",
         "p19Comuna", "p20", "p20_1", "p20_1_otro", "p21", "p22", "p23", 
         "p23_1", "p23_2", "p25", "p25.1", "p25.2", "p25.3", "p25.4", "p26",
         "p27", "p27.1", "p27.2", "p27.3", "p27.4", "p27.5", "p27.6", "p27.7", 
         "p27.8", "p27.9", "p27.10", "p27_otro", "p29", "p29_otro", "p30", 
         "p30_otro", "p33", "p33_otro", "p34", "p34_otro", "p35", "p35_otro", 
         "p38p38_1", "p38p38_2", "p38p38_3", "p38p38_4", "p38p38_5", "p38p38_6", 
         "p38p38_7", "p38p38_99", "p38_99_otro", "p39", "p39.1", "p39_otro", 
         "p39_1", "p39_1_1", "p39_1_otro", "p39_2", "p40", "p41")

cont <- c("p1Edad", "p18", "p18_p1", "p18_p2", "p18_p3", "p18_p4", "p18_c1")

likert <- c("p24", "p28p28_1", "p28p28_2", "p28p28_3", "p28p28_4", 
            "p28p28_5", "p28p28_6", "p28p28_7", "p28p28_8", "p28p28_9", 
            "p32", "p36", "p37")


dataset <- dataset %>% dplyr::mutate(across(all_of(cont), as.numeric))

# Recodificar variables likert:
dataset <- dataset %>%
  mutate(across(all_of(likert), ~ case_when(
    . %in% c("1", "Nada importante", "nada importante", "Nada  importante") ~ 1,
    . %in% c("2", "Poco importante", "poco importante") ~ 2,
    . %in% c("3", "Algo importante", "Medianamente importante", "algo importante") ~ 3,
    . %in% c("4", "Importante", "importante") ~ 4,
    . %in% c("5", "Muy importante", "muy importante", "Muy  importante") ~ 5,
    TRUE ~ NA_real_
  )))

dataset <- dataset %>%
  mutate(p24 = case_when(
    as.character(p24) %in% c("1", "Nada Satisfecho", "nada satisfecho") ~ 1,
    as.character(p24) %in% c("2", "Poco Satisfecho", "poco satisfecho") ~ 2,
    as.character(p24) %in% c("3", "Satisfecho", "satisfecho") ~ 3,
    as.character(p24) %in% c("4", "Muy Satisfecho", "muy satisfecho") ~ 4,
    as.character(p24) %in% c("5", "Totalmente satisfecho", "totalmente satisfecho") ~ 5,
    TRUE ~ NA_real_
  ))

dataset <- dataset %>%
  mutate(p32 = case_when(
    as.character(p32) %in% c("1", "Nulo", "nulo") ~ 1,
    as.character(p32) %in% c("2", "Bajo", "bajo") ~ 2,
    as.character(p32) %in% c("3", "Moderado", "moderado") ~ 3,
    as.character(p32) %in% c("4", "Alto", "alto") ~ 4,
    as.character(p32) %in% c("5", "Muy alto", "muy alto", "Muy  alto") ~ 5,
    TRUE ~ NA_real_
  ))

# Vector con tus variables Likert
likert_vars <- c("p24", "p28p28_1", "p28p28_2", "p28p28_3", "p28p28_4",
                 "p28p28_5", "p28p28_6", "p28p28_7", "p28p28_8", "p28p28_9", "p32")

# Convertir las columnas Likert a numéricas 
dataset <- dataset %>%
  mutate(across(all_of(likert_vars),
                ~ suppressWarnings(as.numeric(as.character(.x)))))


# Recodificar la variable edadr (edad en rango)
dataset$Edadr[dataset$Edadr %in% c("18 - 24 años","25 - 34 años")] = "18 - 34 años"
dataset$Edadr[dataset$Edadr %in% c("35 - 44 años", "45 - 54 años")] = "35 - 54 años"
dataset$Edadr[dataset$Edadr %in% c("55 - 64 años", "65 - 80 años")] = "55 - 80 años"


dataset$p5 <- dplyr::recode(dataset$p5,
                            # Sin educación o preescolar
                            "Ninguno" = "Sin educación o preescolar",
                            "Preescolar" = "Sin educación o preescolar",
                            "No sabe, no informa" = "Sin educación o preescolar",
                            
                            # Básica primaria
                            "Básica primaria completa" = "Básica primaria",
                            "Básica primaria incompleta" = "Básica primaria",
                            
                            # Secundaria o media
                            "Secundaria completa" = "Secundaria o media",
                            "Secundaria incompleta" = "Secundaria o media",
                            
                            # Técnico o tecnológico
                            "Técnico profesional" = "Técnico o tecnológico",
                            "Tecnológico" = "Técnico o tecnológico",
                            
                            # Universitario y posgrado
                            "Universitario" = "Universitario o posgrado",
                            "Especialización" = "Universitario o posgrado",
                            "Maestría" = "Universitario o posgrado",
                            "Doctorado" = "Universitario o posgrado"
)


# Recodificar medios de transporte (p17)
dataset$p17 <- as.character(dataset$p17)
# --- Automóvil / camioneta ---
dataset$p17[dataset$p17 %in% c("Automóvil",
                               "Campero/ Camioneta (SUV)",
                               "Van/Camioneta con platón")] <- "Auto"

# --- Automóvil por plataforma ---
dataset$p17[dataset$p17 %in% c("Automóvil en plataforma (Ejemplo:Uber, Yango, Didi, InDriver)",
                               "Taxi")] <- "Aplicación viajes / Taxi (auto)"

# --- Motos (particulares) ---
dataset$p17[dataset$p17 %in% c("Moto 2T", "Moto 4T")] <- "Moto"

# --- Moto por plataforma ---
dataset$p17[dataset$p17 %in% c("Moto en plataforma (Ejemplo: Didi, Picap, Uber, otras)")] <- "Aplicación viajes (moto)"

# --- Activos: bici / caminata ---
dataset$p17[dataset$p17 %in% c("Bicicleta",
                               "Caminata")] <- "Activos"

# --- Transporte público formal (Medellín) ---
dataset$p17[dataset$p17 %in% c("Bus, colectivo, buseta",
                               "Metro",
                               "Metroplus",
                               "Metro Cable",
                               "Tranvía")] <- "Público (formal)"

# Recodificar la educación (p5)
dataset$p5[dataset$p5 %in% c("Básica primaria incompleta",
                             "Básica primaria completa", 
                             "Preescolar")] = "Básica"
dataset$p5[dataset$p5 %in% c("Secundaria completa", 
                             "Secundaria incompleta")] = "Secundaria"
dataset$p5[dataset$p5 %in% c("Doctorado",
                             "Maestría",
                             "Especialización",
                             "Universitario",
                             "Técnico profesional",
                             "Tecnológico")] = "Terciaria"

# Recodificación de la ocupación (p7)
# Desocupado o inactivo; ama(o) de casa; estudiante; empleado o independiente
dataset$p7[dataset$p7 %in% c("Hacer trabajo doméstico en su propio hogar")] = "Ama(o) de casa"
dataset$p7[dataset$p7 %in% c("Trabajar")] = "Empleado o independiente"
dataset$p7[dataset$p7 %in% c("Trabajar y estudiar",
                             "Estudiar")] = "Estudiante"
dataset$p7[dataset$p7 %in% c("Está desempleado",
                             "Es pensionado(a)",
                             "Incapacitado permanente para trabajar")] = "Desocupado o inactivo"

# Recodificar estrato p9Estrato
dataset$p9Estrato[dataset$p9Estrato %in% c("Estrato 1")] = "1"
dataset$p9Estrato[dataset$p9Estrato %in% c("Estrato 2")] = "2"
dataset$p9Estrato[dataset$p9Estrato %in% c("Estrato 3")] = "3"
dataset$p9Estrato[dataset$p9Estrato %in% c("Estrato 4")] = "4"
dataset$p9Estrato[dataset$p9Estrato %in% c("Estrato 5")] = "5"
dataset$p9Estrato[dataset$p9Estrato %in% c("Estrato 6")] = "6"
dataset$estrato = factor(dataset$p9Estrato, 
                         levels = c(1,2,3,4,5,6),
                         labels = c("Bajo", "Bajo",
                                    "Medio", "Medio",
                                    "Alto", "Alto"))

# Crear variable id
dataset$id <- seq(1, nrow(dataset), by = 1)

# Recodificar n_autos hogar (p15)
dataset$p15[dataset$p15 %in% c("3", "Más de 3")] = "3 o más"

# Recodificar n_motos hogar (p16)
dataset$p16[dataset$p16 %in% c("3", "Más de 3")] = "3 o más"

# Recodificar la variable propósito de viaje (p23)
dataset$p23 <- dplyr::recode(dataset$p23,
                             # --- Cuidado y familia ---
                             "A acompañar o llevar a alguien" = "Cuidado y familia",
                             "A visitar a alguien (familiar o amigo)" = "Visitas sociales",
                             
                             # --- Religión ---
                             "A asistir a alguna actividad de tipo religioso y/o de culto (la iglesia)" = "Recreación, salud y actividades personales",
                             
                             # --- Trabajo / Estudio ---
                             "A buscar trabajo" = "Trabajo",
                             "Ir a trabajar" = "Trabajo",
                             "Ir a estudiar" = "Estudio",
                             
                             # --- Compras y trámites ---
                             "A llevar y/o dejar algo" = "Compras y trámites",
                             "A realizar algún trámite personal" = "Compras y trámites",
                             "A realizar compras" = "Compras y trámites",
                             
                             # --- Salud ---
                             "A una cita médica, tomarse  examenes o reclamar medicamentos para usted mismo" = "Recreación, salud y actividades personales",
                             
                             # --- Recreación y deporte ---
                             "A realizar actividades físicas y/o deportivas (ir al gym, trotar, entrenar)" = "Recreación, salud y actividades personales",
                             "A realizar actividades recreativas y culturales (ir a cine, a un concierto, una presentación etc)" = "Recreación, salud y actividades personales",
                             
                             # --- Otro ---
                             "A otro asunto" = "Otro"
)

# Recodificar la variable razón de elección del medio (p25)
dataset$p25 <- dplyr::recode(dataset$p25,
                             # Económicas
                             "El costo (sus posibilidades económicas o capacidad adquisitiva)" = "Económicas",
                             
                             # Tiempo y distancia
                             "El tiempo de viaje" = "Tiempo y distancia",
                             "La distancia que debe recorrer" = "Tiempo y distancia",
                             
                             # Comodidad y control
                             "La comodidad (confort)" = "Comodidad y control",
                             "La autonomía o control sobre el viaje" = "Comodidad y control",
                             
                             # Seguridad y salud
                             "La percepción de seguridad" = "Seguridad y salud",
                             "Condiciones de salud propias o de un familiar" = "Seguridad y salud",
                             
                             # Restricciones externas
                             "Restricciones de circulación (Ejemplo: pico y placa)" = "Restricciones externas",
                             
                             # Ambientales
                             "Por razones medioambientales (consciencia ambiental)" = "Ambientales",
                             
                             # Otro
                             "Otro" = "Otro"
)

# Recodificar la variable lo que menos le gusta del transporte (p26)
dataset$p26 <- dplyr::recode(dataset$p26,
                             # Económicas
                             "El costo de compra" = "Económicas",
                             "El costo de uso u operación" = "Económicas",
                             
                             # Tiempo
                             "El tiempo de espera" = "Tiempo",
                             "El tiempo de viaje" = "Tiempo",
                             
                             # Clima
                             "La exposición a condiciones climáticas desfavorables (lluvia o calor)" = "Clima y condiciones externas",
                             
                             # Comodidad y control
                             "La falta de autonomía o control sobre el viaje" = "Comodidad y control",
                             "Las condiciones de incomodidad" = "Comodidad y control",
                             
                             # Seguridad
                             "La percepción de inseguridad personal (robo o atraco, acoso o violencia de algún tipo)" = "Seguridad",
                             "La vulnerabilidad frente accidentes de tránsito" = "Seguridad",
                             
                             # Ambientales
                             "El nivel de emisiones (contaminación)" = "Ambientales",
                             
                             # Nada / No sabe
                             "Nada me disgusta" = "Nada/NSNR",
                             "No sabe/ No responde" = "Nada/NSNR",
                             
                             # Otro
                             "Otra razón" = "Otro"
)

# Guardar base procesada en Excel
writexl::write_xlsx(dataset, "Output/base_Med_2025.xlsx")
