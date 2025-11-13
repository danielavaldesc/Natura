##############################################
## OVERSAMPLING PARA MNL MEDELLÍN           ##
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
library(forcats)

# Para oversampling y modelos
library(smotefamily)
library(themis)
library(scutr)
library(ggplot2)
library(caret)
library(fastDummies)

#####################################
## 2. Cargar datos
#####################################

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\")
input <- read_excel("271025_Results_Med\\output\\input_famd_med_29102025.xlsx")

#####################################
## 3. Definir continuas y categóricas
#####################################

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

# Categóricas = todas menos id, medio y las continuas
categoricas <- setdiff(names(input), c(continuas, "id", "medio"))

# Convertir a factor todas las categóricas
df.cat <- input %>%
  select(all_of(categoricas)) %>%
  mutate(across(everything(), as.factor))

# Base completa con cats + id, medio, continuas
df.mnl <- cbind(df.cat, input[c("id", "medio", continuas)])

#####################################
## 4. Agrupación y recodificación de categorías
#####################################

# 4.1. p23_agr5 (motivo del viaje)
df.mnl <- df.mnl %>%
  mutate(
    p23_agr5 = fct_collapse(
      p23_agregado,
      "Trabajo"          = c("Trabajo"),
      "Compras/Trámites" = c("Compras y trámites", "Compras y tr\u00e1mites"),
      "Tiempo personal"  = c("Recreación, salud y actividades personales",
                             "Recreaci\u00f3n, salud y actividades personales",
                             "Visitas sociales"),
      "Estudio"          = c("Estudio"),
      "Cuidado"          = c("Cuidado y familia (centro educativo, niños/as o jóvenes)",
                             "Cuidado y familia (otro lugar, niños/as o jóvenes)",
                             "Cuidado y familia (escuela, ni niños)",
                             "Cuidado y familia (persona con discapacidad)",
                             "Cuidado y familia (persona enferma)",
                             "Cuidado y familia (recreación, niños)",
                             "Cuidado y familia (salud, niños)",
                             "Cuidado y familia (salud, ni\u00f1as/os)",
                             "Cuidado y familia (recreaci\u00f3n, ni\u00f1as/os)",
                             "Cuidado y familia (escuela, ni\u00f1as/os)"),
      "Otros"            = c("Otro")
    ) %>% fct_drop()
  ) %>%
  filter(p23_agr5 != "Otros") %>%
  mutate(p23_agr5 = as.factor(as.character(p23_agr5)))

# 4.2. Filtros básicos y recodificación de sexo y situación laboral original
df.mnl <- df.mnl %>%
  filter(p5_agregado != "Sin respuesta",
         p40 %in% c("Hombre", "Mujer")) %>%
  mutate(p40 = factor(as.character(p40))) %>%
  filter(p7_agregado != "Otro") %>%
  mutate(p7_agregado = factor(as.character(p7_agregado)))

# 4.3. auto-reconocimiento étnico
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

# 4.4. educación 3 categorías
df.mnl <- df.mnl %>%
  mutate(
    educ_3cat = case_when(
      p5_agregado == "Primaria o menos" ~ "Primaria o menos",
      p5_agregado == "Secundaria" ~ "Secundaria",
      p5_agregado %in% c("Superior", "Técnico / Tecnológico") ~ "Terciaria",
      TRUE ~ NA_character_
    ),
    educ_3cat = factor(
      educ_3cat,
      levels = c("Primaria o menos", "Secundaria", "Terciaria")
    )
  )

# 4.5. situación laboral recodificada
df.mnl <- df.mnl %>%
  mutate(
    sitlab = case_when(
      p7_agregado == "Ocupado/a" ~ "Asalariado o independiente",
      p7_agregado == "Trabajo doméstico no remunerado" ~ "Trabajo doméstico no remunerado",
      p7_agregado %in% c("Desocupado o inactivo", "Estudiante") ~ "Desocupado o inactivo",
      TRUE ~ NA_character_
    ),
    sitlab = factor(
      sitlab,
      levels = c("Asalariado o independiente",
                 "Trabajo doméstico no remunerado",
                 "Desocupado o inactivo")
    )
  )

# 4.6. Tenencia de autos
df.mnl <- df.mnl %>%
  mutate(
    ten_autos = case_when(
      p15_autos_agregado == "Sin autos" ~ "Sin autos",
      p15_autos_agregado %in% c("2 o más autos", "1 auto") ~ "1 auto o más",
      TRUE ~ NA_character_
    ),
    ten_autos = factor(ten_autos,
                       levels = c("Sin autos", "1 auto o más"))
  )

# 4.7. Tenencia de motos
df.mnl <- df.mnl %>%
  mutate(
    ten_motos = case_when(
      p16_motos_agregado == "Sin motocicletas" ~ "Sin motocicletas",
      p16_motos_agregado %in% c("2 o más motocicletas", "1 motocicleta") ~ "1 motocicleta o más",
      TRUE ~ NA_character_
    ),
    ten_motos = factor(ten_motos,
                       levels = c("Sin motocicletas", "1 motocicleta o más"))
  )

# 4.8. Distancia recodificada
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

# 4.9. Motivo p23: Trabajo / No trabajo
df.mnl$p23_rec <- dplyr::case_when(
  df.mnl$p23_agr5 == "Trabajo" ~ "Trabajo",
  df.mnl$p23_agr5 %in% c("Tiempo personal", "Compras/Trámites",
                         "Cuidado", "Estudio") ~ "No trabajo",
  TRUE ~ NA_character_
)
df.mnl$p23_rec <- factor(df.mnl$p23_rec, levels = c("Trabajo", "No trabajo"))

#####################################
## 5. Preparar base para SMOTE
#####################################

# Variable respuesta
df.mnl$medio <- as.factor(df.mnl$medio)
prop.table(table(df.mnl$medio)) * 100

# Variables finales para el modelo (continuas + nuevas categóricas)
continuas_modelo <- c(
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
  "tiempo_total"
)

categoricas_modelo <- c(
  "edad_r2",
  "aut_rec_etnico",
  "educ_3cat",
  "sitlab",
  "p40",
  "p38p38_dummy"
)

# Asegurar que las categóricas del modelo son factor
df.mnl <- df.mnl %>%
  mutate(across(all_of(categoricas_modelo), ~ as.factor(.x)))

# Base para oversampling 
df_model <- df.mnl %>%
  select(medio,
         all_of(continuas_modelo),
         all_of(categoricas_modelo))

#####################################
## 6. Oversampling con SMOTENC (themis)
#####################################

set.seed(1234)

smotenc_data <- smotenc(
  df_model,
  var        = "medio",
  over_ratio = 1   # igualar tamaños al de la clase mayoritaria
)

# Distribución de clases después del oversampling
table_original <- table(df_model$medio)
table_smote    <- table(smotenc_data$medio)

table_original
table_smote

# --------- Outcome ---------
df.mnl$medio <- relevel(factor(df.mnl$medio), ref = "Moto privada")

# --------- Fórmulas (base y con comunas) ----------
form_base <- as.formula(
  medio ~
    p24 + p28_importancia_costo_compra + p28_importancia_costo_uso +
    p28_importancia_comodidad + p28_importancia_tiempo + p28_importancia_riesgo_robo +
    p28_importancia_riesgo_acoso + p28_importancia_discriminacion +
    p28_importancia_emisiones + p28_importancia_siniestralidad +
    tiempo_total +
    edad_r2 + aut_rec_etnico + educ_3cat + sitlab + p40 + p38p38_dummy
)

# --------- Estimaciones ----------
smotenc_sin <- multinom(form_base, data = smotenc_data, trace = FALSE)

# --------- Función OR + IC95% + z + p ----------
or_table <- function(model) {
  sm  <- summary(model)
  b   <- sm$coefficients
  se  <- sm$standard.errors
  cf  <- b %>% as.data.frame() %>% rownames_to_column("categoria_medio") %>%
    pivot_longer(-categoria_medio, names_to = "termino", values_to = "estimate")
  sf  <- se %>% as.data.frame() %>% rownames_to_column("categoria_medio") %>%
    pivot_longer(-categoria_medio, names_to = "termino", values_to = "std.error")
  out <- left_join(cf, sf, by = c("categoria_medio","termino")) %>%
    mutate(
      z = estimate / std.error,
      p = 2 * pnorm(abs(z), lower.tail = FALSE),
      OR = exp(estimate),
      CI_low  = exp(estimate - 1.96*std.error),
      CI_high = exp(estimate + 1.96*std.error)
    ) %>%
    mutate(
      OR = round(OR, 3), CI_low = round(CI_low, 3), CI_high = round(CI_high, 3),
      z  = round(z, 3),  p      = round(p, 4)
    ) %>%
    arrange(categoria_medio, termino)
  list(long = out, matrix = exp(coef(model)) %>% as.data.frame() %>% round(3))
}

or_sin <- or_table(smotenc_sin)

# --------- Exportar ----------
out_dir <- "271025_Results_Med\\MNL"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Stargazer
stargazer(smotenc_sin, type = "html",
          title = "MNL (smotenc) – SIN comunas",
          single.row = TRUE, na.replace = "",
          out = file.path(out_dir, "mnl_smotenc_SINcomunas_stargazer.html"))

# Excel OR
writexl::write_xlsx(
  list(
    "SIN_OR_largo"   = or_sin$long,
    "SIN_OR_matriz"  = or_sin$matrix
  ),
  path = file.path(out_dir, "mnl_smotenc_OR_SIN.xlsx")
)

cat("\n✅ Resultados guardados en:\n", normalizePath(out_dir), "\n",
    "- mnl_smotenc_SINcomunas_stargazer.html\n",
    "- mnl_smotenc_OR_SIN.xlsx\n", sep = "")


