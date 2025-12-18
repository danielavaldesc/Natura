
##############################################
##############################################
## Modelo logit multinomial (MNL)           ##
## Medellín - Fecha de creación: 09/11/2025 ##
##############################################
##############################################

#####################################
## 1. Cargar librerías
#####################################

library(dplyr)
library(readxl)
library(tidyr)
library(tibble)
library(nnet)
library(stargazer)
library(writexl)
library(stringr)

#####################################
## 2. Cargar datos
#####################################

setwd("C:\\Users\\Portatil\\Desktop\\Natura\\")
input = readxl::read_excel("271025_Results_Med\\output\\input_famd_med_29102025.xlsx")

#####################################
## 3. Revisión de categorías
#####################################

# Definición de variables continuas y categóricas
continuas <- c(
  "p24",
  "p28_importancia_costo_compra",
  "p28_importancia_costo_uso",
  "p28_importancia_comodidad",
  "p28_importancia_tiempo",
  "p28_importancia_riesgo_robo",
  "p28_importancia_riesgo_acoso",
  "p28_importancia_discriminacion",
  "p28_importancia_emisiones",
  "p28_importancia_siniestralidad",
  "p32_contaminacion_likert",
  "p36_influencia_amigos",
  "p37_influencia_familia",
  "tiempo_total",
  "p1edad"
)

categoricas <- setdiff(names(dataset), c(continuas, "id", "medio"))

df.cat = input %>% select(categoricas) %>%
  mutate(across(everything(), ~ as.factor(.x)))

df.mnl = cbind(df.cat, input[c("id", "medio", continuas)])

# Agrupación de categorías
df.mnl <- df.mnl %>%
  mutate(
    p23_agr5 = fct_collapse(
      p23_agregado,
      "Trabajo"           = c("Trabajo"),
      "Compras/Trámites"  = c("Compras y trámites", "Compras y tr\u00e1mites"),
      "Tiempo personal"   = c("Recreación, salud y actividades personales",
                              "Recreaci\u00f3n, salud y actividades personales",
                              "Visitas sociales"),
      "Estudio"           = c("Estudio"),
      "Cuidado"     = c("Cuidado y familia (centro educativo, niños/as o jóvenes)",
                        "Cuidado y familia (otro lugar, niños/as o jóvenes)",
                        "Cuidado y familia (escuela, ni niños)",
                        "Cuidado y familia (persona con discapacidad)",
                        "Cuidado y familia (persona enferma)",
                        "Cuidado y familia (recreación, niños)",
                        "Cuidado y familia (salud, niños)",
                        "Cuidado y familia (salud, ni\u00f1as/os)",
                        "Cuidado y familia (recreaci\u00f3n, ni\u00f1as/os)",
                        "Cuidado y familia (escuela, ni\u00f1as/os)"),
      "Otros"     = c("Otro")
    ) %>% fct_drop()
  )

# Filtros adicionales
df.mnl <- df.mnl %>%
  filter(p23_agr5 != "Otros") %>% mutate(p23_agr5 = as.factor(as.character(p23_agr5)))

df.mnl <- df.mnl %>% filter(p5_agregado  != "Sin respuesta",
                               p40 %in% c("Hombre", "Mujer")) %>%
  mutate(p40 = as.factor(as.character(p40)))

df.mnl <- df.mnl %>% filter(p7_agregado  != "Otro") %>%
  mutate(p7_agregado = as.factor(as.character(p7_agregado)))

#####################################
## 4. Validación con tablas cruzadas
#####################################

# Tablas cruzadas
i = 16
print(categoricas[i])
round(prop.table(table(df.mnl$medio, 
                       df.mnl[[categoricas[i]]]))*100,0)

# Recodificación resultante
df.mnl <- df.mnl %>%
  mutate(
    aut_rec_etnico = case_when(
      p3_agregado == "Ninguna" ~ "No",
      p3_agregado %in% c("Población afrodescendiente", "Pueblos indígenas") ~ "Si",
      p3_agregado == "Sin respuesta" ~ "No",
      TRUE ~ NA_character_
    ),
    aut_rec_etnico = factor(aut_rec_etnico, levels = c("No", "Si"))
  )

df.mnl <- df.mnl %>%
  mutate(
    educ_3cat = case_when(
      p5_agregado == "Primaria o menos" ~ "Primaria o menos",
      p5_agregado == "Secundaria" ~ "Secundaria",
      p5_agregado %in% c("Superior", "Técnico / Tecnológico") ~ "Terciaria",
      TRUE ~ NA_character_
    ),
    educ_3cat = factor(educ_3cat,
                       levels = c("Primaria o menos", "Secundaria", "Terciaria"))
  )

df.mnl <- df.mnl %>%
  mutate(
    sitlab = case_when(
      p7_agregado == "Ocupado/a" ~ "Asalariado o independiente",
      p7_agregado == "Trabajo doméstico no remunerado" ~ "Trabajo doméstico no remunerado",
      p7_agregado %in% c("Desocupado o inactivo",
                               "Trabajo doméstico no remunerado") ~ "Desocupado o inactivo",
      TRUE ~ NA_character_
    ),
    sitlab = factor(
      sitlab,
      levels = c("Asalariado o independiente", "Trabajo doméstico no remunerado", "Desocupado o inactivo")
    )
  )

df.mnl <- df.mnl %>%
  mutate(
    ten_autos = case_when(
      p15_autos_agregado == "Sin autos" ~ "Sin autos",
      p15_autos_agregado %in% c("2 o más autos",
                         "1 auto") ~ "1 auto o más",
      TRUE ~ NA_character_
    ),
    ten_autos = factor(
      ten_autos,
      levels = c("Sin autos", "1 auto o más")
    )
  )

df.mnl <- df.mnl %>%
  mutate(
    ten_motos = case_when(
      p16_motos_agregado == "Sin motocicletas" ~ "Sin motocicletas",
      p16_motos_agregado %in% c("2 o más motocicletas",
                                "1 motocicleta") ~ "1 motocicleta o más",
      TRUE ~ NA_character_
    ),
    ten_motos = factor(
      ten_motos,
      levels = c("Sin motocicletas", "1 motocicleta o más")
    )
  )

df.mnl <- df.mnl %>%
  mutate(
    dist_recod = case_when(
      p22 %in% c("Menos de 5 km") ~ "Menos de 5 km",
      p22 %in% c("Entre 6 y 10 km", "Entre 11 y 15 km") ~ "Entre 6 y 15 km",
      p22 %in% c("Entre 16 y 20 km", "Más de 20 km") ~ "Más de 15 km",
      TRUE ~ NA_character_
    ),
    dist_recod = factor(
      dist_recod,
      levels = c("Menos de 5 km", "Entre 6 y 15 km", "Más de 15 km")
    )
  )


df.mnl$p23_rec <- dplyr::case_when(
  df.mnl$p23_agr5 == "Trabajo" ~ "Trabajo",
  df.mnl$p23_agr5 %in% c("Tiempo personal", "Compras/Trámites",
                         "Cuidado", "Estudio") ~ "No trabajo",
  TRUE ~ NA_character_
)
df.mnl$p23_rec <- factor(df.mnl$p23_rec, levels = c("Trabajo", "No trabajo"))

#####################################
## 5. Estimación del modelo MNL
#####################################

# Antes de la estimación, se verifican correlaciones
library(corrplot)
mat_cor <- cor(df.mnl[continuas], use = "complete.obs")

corrplot(mat_cor,
         method = "color",      
         type = "upper",       
         addCoef.col = "black",  
         tl.col = "black",      
         tl.cex = 0.8)

# Estimación de los modelos
# Versión de prueba
library(nnet)
mnl.ctrl.test <-  multinom(
  medio ~ 
    # Variables continuas
  p24 +
    p28_importancia_costo_compra +
    p28_importancia_costo_uso +
    p28_importancia_comodidad +
    p28_importancia_tiempo +
    p28_importancia_riesgo_robo +
    p28_importancia_riesgo_acoso +
    p28_importancia_discriminacion +
    p28_importancia_emisiones +
    p28_importancia_siniestralidad +
    p32_contaminacion_likert +
    p36_influencia_amigos +
    p37_influencia_familia +
    tiempo_total +
    
    # Variables categóricas
    edad_r2 + aut_rec_etnico + educ_3cat + sitlab + p9_estrato3 +
    p12_dificultad_binaria + p40 + ten_autos  + ten_motos + dist_recod +
    p23_agr5 + p38p38_dummy +
    
  
  
  # Interacciones
    ten_autos *p40 +
    ten_motos*p40 +
    p38p38_dummy*p40 +
  # Controles
    p19comuna
    ,
  
  
  data  = df.mnl,
  trace = FALSE
) 

# Especificación final
mnl.ctrl2 <-  multinom(
  medio ~ 
    # Variables continuas
    p24 + p28_importancia_costo_compra + p28_importancia_costo_uso + 
    p28_importancia_comodidad + p28_importancia_tiempo + p28_importancia_riesgo_robo +
    p28_importancia_riesgo_acoso + p28_importancia_discriminacion +
    p28_importancia_emisiones + 
    p28_importancia_siniestralidad + 
    tiempo_total +
    
    # Variables categóricas
    edad_r2 
   + aut_rec_etnico 
   + educ_3cat 
  + sitlab 
  + p40  +
  + p38p38_dummy +
    
  # Controles
  p19comuna 
  
  ,
  
  
  data  = df.mnl,
  trace = FALSE
) 


# Stargazer
library(stargazer)
tmp <- tempfile(fileext = ".html")

stargazer(mnl.ctrl2,
          type = "html",
          title = "Modelo multinomial",
          out = tmp)
rstudioapi::viewer(tmp)

#####################################
## 5. Validación de OR
#####################################
exp(coef(mnl.ctrl2))



