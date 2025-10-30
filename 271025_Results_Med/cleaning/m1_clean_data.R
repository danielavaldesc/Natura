
###############################################################################
# MÓDULO 1: Movilidad
# Descripción: Recodificación de variables demográficas, educativas, laborales
#              y de composición del hogar para análisis por género.
###############################################################################

## ============================================================================ 
## 0. Cargar entorno y configuración inicial
## ============================================================================ 

source("cleaning/0_clean_data.R")

# Seleccionar variables del módulo
mod_socio <- diccionario_clasificado %>%
  filter(modulo == "Módulo 1: Socio-Demográfico")

vars <- mod_socio$codigo


## ============================================================================ 
## 1. Edad
## ============================================================================ 

# Filtro de rango etario válido
dataset <- dataset %>% filter(p1edad >= 18 & p1edad <= 80)

# Agrupación en tramos amplios
dataset <- dataset %>%
  mutate(
    edad_r2 = case_when(
      edadr %in% c("18 - 24 años", "25 - 34 años") ~ "18 - 34 años",
      edadr %in% c("35 - 44 años", "45 - 54 años") ~ "35 - 54 años",
      edadr %in% c("55 - 64 años", "65 - 80 años") ~ "55 - 80 años",
      TRUE ~ NA_character_
    )
  )

mod_socio <- rbind(
  mod_socio,
  data.frame(
    codigo = "edad_r2",
    descripcion = "Recodificación edad (18–34 / 35–54 / 55–80)",
    modulo = "Módulo 1: Socio-Demográfico",
    submodulo = NA
  )
)


## ============================================================================ 
## 2. País de nacimiento
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    pais = case_when(
      p2 == "No" ~ p2_2,               # Nació fuera de Colombia
      p2 == "Si" ~ "Colombia",
      TRUE ~ NA_character_
    ),
    pais = ifelse(is.na(pais), p2_2_1, pais)
  )

mod_socio <- rbind(
  mod_socio,
  data.frame(
    codigo = "pais",
    descripcion = "País de nacimiento (Colombia u otro)",
    modulo = "Módulo 1: Socio-Demográfico",
    submodulo = NA
  )
)


## ============================================================================ 
## 3. Autorreconocimiento étnico
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    p3_agregado = case_when(
      p3 == "Indígena" ~ "Pueblos indígenas",
      p3 %in% c(
        "Negro(a), mulalto(a), afrodescendiente, afrocolombiano(a)",
        "Raizal del Archipiélago de San Andrés, Providencia y Santa Catalina", 
        "Palenquero(a)"
      ) ~ "Población afrodescendiente",
      p3 == "Ninguna de las anteriores" ~ "Ninguna",
      p3 == "No sabe/ No responde" ~ "Sin respuesta",
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 4. Nivel educativo
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    p5_agregado = case_when(
      p5 %in% c("Ninguno", "Preescolar",
                "Básica primaria completa",
                "Básica primaria incompleta") ~ "Primaria o menos",
      p5 %in% c("Secundaria completa", "Secundaria incompleta") ~ "Secundaria",
      p5 %in% c("Técnico profesional", "Tecnológico") ~ "Técnico / Tecnológico",
      p5 %in% c("Universitario", "Especialización", "Maestría", "Doctorado") ~ "Superior",
      p5 == "No sabe, no informa" ~ "Sin respuesta",
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 5. Situación laboral principal (p7)
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    p7_agregado = case_when(
      p7 %in% c("Trabajar", "Trabajar y estudiar") ~ "Ocupado/a",
      p7 %in% c("Está desempleado",
                "Es pensionado(a)",
                "Incapacitado permanente para trabajar") ~ "Desocupado o inactivo",
      p7 == "Estudiar" ~ "Estudiante",
      p7 == "Hacer trabajo doméstico en su propio hogar" ~ "Trabajo doméstico no remunerado",
      p7 == "Otra actividad" ~ "Otro",
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 6. Posición ocupacional (p7_1)
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    p7_1_agregado = case_when(
      p7_1 == "Empleado en empresa, gobierno u organización" ~ "Asalariado formal",
      p7_1 == "Empleado(a) doméstico" ~ "Empleado(a) doméstico",
      p7_1 == "Patrón o empleador" ~ "Empleador",
      p7_1 == "Profesional independiente (por ejemplo, por prestación de servicios)" ~ "Independiente profesional",
      p7_1 == "Trabajador independiente o por cuenta propia" ~ "Independiente no profesional",
      p7_1 == "Trabajador de finca, tierra o parcela propia" ~ "Trabajador de finca, tierra o parcela",
      p7_1 %in% c(
        "Ayudante sin remuneración (hijo o familiar de empleados del servicio doméstico,mayordomos, jornaleros, etc.)",
        "Trabajador sin remuneración"
      ) ~ "No remunerado / familiar colaborador",
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 7. Días presenciales de trabajo (p7_2)
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    dias_presenciales = pmap_chr(
      dplyr::select(., p7_2, p7_2_1, p7_2_2, p7_2_3, p7_2_4),
      ~ c(...) %>% discard(is.na) %>% unique() %>% paste(collapse = ", ")
    ),
    p7_2_agregado = case_when(
      str_detect(dias_presenciales, "Ningún día") ~ "No trabaja presencialmente",
      str_detect(dias_presenciales, "De lunes a domingo") ~ "Toda la semana",
      str_detect(dias_presenciales, "De lunes a sábado") ~ "Lunes a sábado",
      str_detect(dias_presenciales, "De lunes a viernes") ~ "Lunes a viernes",
      str_detect(dias_presenciales, "Lunes|Martes|Miércoles|Jueves|Viernes|Sábado|Domingo") ~ "Algunos días",
      TRUE ~ NA_character_
    ),
    # Control de aplicabilidad
    p7_2_agregado = case_when(
      p7_agregado != "Ocupado/a" ~ "No aplica",
      is.na(p7_2_agregado) & p7_agregado == "Ocupado/a" ~ "Sin información",
      TRUE ~ p7_2_agregado
    )
  )


## ============================================================================ 
## 8. Días presenciales de estudio (p7_3)
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    dias_estudio = pmap_chr(
      dplyr::select(., p7_3, p7_3_1, p7_3_2),
      ~ c(...) %>% discard(is.na) %>% unique() %>% paste(collapse = ", ")
    ),
    p7_3_agregado = case_when(
      str_detect(dias_estudio, "Ningún día") ~ "No estudia presencialmente",
      str_detect(dias_estudio, "De lunes a domingo") ~ "Toda la semana",
      str_detect(dias_estudio, "De lunes a sábado") ~ "Lunes a sábado",
      str_detect(dias_estudio, "De lunes a viernes") ~ "Lunes a viernes",
      str_detect(dias_estudio, "Lunes|Martes|Miércoles|Jueves|Viernes|Sábado|Domingo") ~ "Algunos días",
      TRUE ~ NA_character_
    ),
    # Control de aplicabilidad
    p7_3_agregado = case_when(
      p7_agregado %in% c("Estudiante") ~ p7_3_agregado,
      !(p7_agregado %in% c("Estudiante")) ~ "No aplica",
      is.na(p7_3_agregado) & p7_agregado %in% c("Estudiante", "Ocupado/a") ~ "Sin información",
      TRUE ~ p7_3_agregado
    )
  )


## ============================================================================ 
## 9. Composición del hogar (p8)
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    p8_agregado = case_when(
      p8 == "Sola(o)" ~ "Vive solo/a",
      p8 %in% c("Con su pareja",
                "Con su pareja y sus hijos o hijas",
                "Con su pareja y otros familiares",
                "Con su pareja, sus hijos y otros familiares") ~ "Vive con pareja (con o sin hijos/as)",
      p8 %in% c("Sola(o) con sus hijas o hijos",
                "Con sus hijos y otros familiares") ~ "Vive con hijos/as (sin pareja)",
      p8 == "Con otros familiares diferentes a sus hijos o pareja" ~ "Vive con familiares (otros)",
      p8 == "Con otras personas que no son de su familia (amigos, compañeros)" ~ "Vive con no familiares",
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 10. Estrato socioeconómico (p9)
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    p9_estrato3 = case_when(
      p9estrato %in% c("Estrato 1", "Estrato 2") ~ "Bajo",
      p9estrato %in% c("Estrato 3", "Estrato 4") ~ "Medio",
      p9estrato %in% c("Estrato 5", "Estrato 6") ~ "Alto",
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 11. Responsabilidad de cuidado (p11 y p11_1)
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    p11_cuidado_agregado = case_when(
      p11 == "No" ~ "Sin personas dependientes",
      p11_1 == "Usted" ~ "Asumido por la persona",
      p11_1 %in% c("Una mujer de la familia que vive con usted",
                   "Una mujer de la familia que no vive con usted") ~ "Cuidado familiar femenino",
      p11_1 == "Un hombre de la familia que vive con usted" ~ "Cuidado familiar masculino",
      p11_1 == "Una cuidadora o trabajadora doméstica" ~ "Cuidado remunerado (externo)",
      p11_1 == "Otra persona que no es familia" ~ "Cuidado no familiar (voluntario u ocasional)",
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 12. Dificultades funcionales (p12, p12_1, p12_2)
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    dificultades = pmap_chr(
      dplyr::select(., p12, p12_1),
      ~ c(...) %>% discard(is.na) %>% unique() %>% paste(collapse = ", ")
    )
  )

dataset <- dataset %>%
  mutate(
    p12_dificultad_binaria = case_when(
      str_detect(dificultades, "Ninguna dificultad") ~ "Sin dificultad",
      dificultades == "" | is.na(dificultades) ~ NA_character_,
      TRUE ~ "Con alguna dificultad"
    ),
    p12_dificultad_principal = case_when(
      str_detect(dificultades, "permanente para moverse") ~ "Dificultad permanente para moverse",
      str_detect(dificultades, "temporal para moverse") ~ "Dificultad temporal para moverse",
      str_detect(dificultades, "ver") ~ "Dificultad visual",
      str_detect(dificultades, "oír") ~ "Dificultad auditiva",
      str_detect(dificultades, "Otro tipo de dificultad") ~ "Otra dificultad",
      str_detect(dificultades, "Ninguna dificultad") ~ "Ninguna dificultad",
      TRUE ~ NA_character_
    )
  )


## ============================================================================ 
## 13. Género (p40)
## ============================================================================ 

dataset <- dataset %>%
  mutate(
    genero_agregado = case_when(
      p40 == "Hombre" ~ "Hombre",
      p40 == "Mujer" ~ "Mujer",
      p40 %in% c("Otras identidades de género", "Prefiere no responder") ~ "Otros / No responde",
      TRUE ~ NA_character_
    )
  )


###############################################################################
## AGREGAR VARIABLES DERIVADAS DEL MÓDULO 1 AL DICCIONARIO CLASIFICADO
###############################################################################

# --- Variables derivadas del Módulo 1: Socio-Demográfico ---

dicc_vars_mod1 <- tribble(
  ~codigo,               ~descripcion,                                                    ~modulo,                              ~submodulo,
  "edad_r2",             "Grupos etarios: 18-34 / 35-54 / 55-80",                        "Módulo 1: Socio-Demográfico",        NA,
  "pais",                "País de nacimiento (Colombia / otro)",                         "Módulo 1: Socio-Demográfico",        NA,
  "p3_agregado",         "Autorreconocimiento étnico (agregado)",                        "Módulo 1: Socio-Demográfico",        NA,
  "p5_agregado",         "Nivel educativo agrupado",                                     "Módulo 1: Socio-Demográfico",        NA,
  "p7_agregado",         "Actividad principal (ocupado, estudiante, etc.)",              "Módulo 1: Socio-Demográfico",        NA,
  "p7_1_agregado",       "Posición ocupacional (agregado)",                              "Módulo 1: Socio-Demográfico",        NA,
  "dias_presenciales",   "Días de trabajo presencial (texto combinado)",                 "Módulo 1: Socio-Demográfico",        NA,
  "p7_2_agregado",       "Frecuencia de trabajo presencial",                             "Módulo 1: Socio-Demográfico",        NA,
  "dias_estudio",        "Días de estudio presencial (texto combinado)",                 "Módulo 1: Socio-Demográfico",        NA,
  "p7_3_agregado",       "Frecuencia de estudio presencial",                             "Módulo 1: Socio-Demográfico",        NA,
  "p8_agregado",         "Composición del hogar (vive solo/a, con pareja, hijos/as...)", "Módulo 1: Socio-Demográfico",        NA,
  "p9_estrato3",         "Nivel socioeconómico agrupado: Bajo / Medio / Alto",           "Módulo 1: Socio-Demográfico",        NA,
  "p11_cuidado_agregado","Responsable principal del cuidado (por tipo de persona)",      "Módulo 1: Socio-Demográfico",        NA,
  "p12_dificultad_binaria","Indicador binario: presencia de alguna dificultad",          "Módulo 1: Socio-Demográfico",        NA,
  "p12_dificultad_principal","Tipo principal de dificultad (visual, movilidad, etc.)",   "Módulo 1: Socio-Demográfico",        NA,
  "genero_agregado",     "Identidad de género (agrupada)",                               "Módulo 1: Socio-Demográfico",        NA
)

# --- Unir al diccionario clasificado ---
diccionario_clasificado <- bind_rows(diccionario_clasificado, dicc_vars_mod1)

# --- Comprobar duplicados (opcional) ---
diccionario_clasificado <- diccionario_clasificado %>%
  distinct(codigo, .keep_all = TRUE)

# --- Aplicar etiquetas a las nuevas variables ---
for (i in 1:nrow(dicc_vars_mod1)) {
  var <- dicc_vars_mod1$codigo[i]
  if (var %in% names(dataset)) {
    var_label(dataset[[var]]) <- dicc_vars_mod1$descripcion[i]
  }
}



## ============================================================================ 
## FIN DEL MÓDULO 1
## ============================================================================ 
