# ================================
  # Multinomial Logit (Medellín)
# ================================

#############
# Librerias #
#############

library(dplyr)
library(nnet)
library(readxl)
library(tidyverse)
library(ggplot2)
library(GGally)
library(stargazer)
library(writexl)


############
# Base de  #
#   Datos  #
############

dataset <- read_excel("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\output\\input_famd_med_29102025.xlsx")


## --- Función dummies (robusta) ---
columna_dummy <- function(df, columna) {
  # `columna` es un string con el nombre de la variable categórica
  df %>%
    mutate(!!columna := paste(columna, .data[[columna]], sep = "_")) %>%
    mutate(valor = 1) %>%
    tidyr::pivot_wider(
      names_from  = !!rlang::sym(columna),
      values_from = valor,
      values_fill = 0
    )
}

# Eliminar categorías de genero
dataset <- subset(dataset, !(p40 %in% c("Otro", "Prefiere no responder", "Otras identidades de género")))

# Recodificación 
vars_dummy <- c(
  "edad_r2","p3_agregado","p5_agregado","p7_agregado","p9_estrato3",
  "p12_dificultad_binaria",
  "p40","p13","p14","p15_autos_agregado","p16_motos_agregado",
  "p19comuna","p22","p23_agregado",
  "p30_razon_no_uso_agregado","p31_fuente_contaminacion_agregada",
  "p38p38_1","p38p38_2","p38p38_3","p38p38_4","p38p38_5","p38p38_6","p38p38_7","p38p38_99"
)


# --- Aplicar función columna_dummy  ---
for (v in vars_dummy) {
  if (v %in% names(dataset)) {
    dataset <- columna_dummy(dataset, v)
    message("✅ Dummies creadas para: ", v)
  } else {
    message("⚠️ Variable no encontrada: ", v)
  }
}

if ("id" %in% names(dataset)) dataset <- subset(dataset, select = -id)

# --- Modelo ---
dataset$medio <- relevel(factor(dataset$medio), ref = "Moto privada")

library(nnet)

# ===========================
# Modelos multinomiales
# ===========================

# --- CON COMUNAS (se mantiene el nombre para que tu script abajo funcione igual) ---
modelo_multinomial <- multinom(
  medio ~ 
    # ------------------ CONTINUAS ------------------
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
    p1edad +
    
    # ------------------ CATEGÓRICAS ------------------
    
    # edad_r2  (BASE: 35 - 54 años)
    `edad_r2_18 - 34 años` + `edad_r2_55 - 80 años` +
    
    # p3_agregado  (BASE: Ninguna)
    `p3_agregado_Población afrodescendiente` +
    `p3_agregado_Sin respuesta` +
    `p3_agregado_Pueblos indígenas` +
    
    # p5_agregado  (BASE: Superior)
    `p5_agregado_Secundaria` +
    `p5_agregado_Primaria o menos` +
    `p5_agregado_Técnico / Tecnológico` +
    
    # p7_agregado  (BASE: Ocupado/a)
    `p7_agregado_Trabajo doméstico no remunerado` +
    `p7_agregado_Desocupado o inactivo` +
    `p7_agregado_Estudiante` +
    `p7_agregado_Otro` +
    
    # p9_estrato3  (BASE: Alto)
    `p9_estrato3_Medio` + `p9_estrato3_Bajo` +
    
    # p12_dificultad_binaria (BASE: Sin dificultad)
    `p12_dificultad_binaria_Con alguna dificultad` +
    
    # p40  (BASE: Hombre)
    `p40_Mujer` +
    
    # p13  (BASE: Sí, auto y motocicleta)
    `p13_Sí, auto` + `p13_No` + `p13_Sí, motocicleta` +
    
    # p14  (BASE: Si de auto y motocicleta)
    `p14_Si, auto` + `p14_No` + `p14_Si, motocicleta` +
    
    # p15_autos_agregado  (BASE: Sin autos)
    `p15_autos_agregado_1 auto` + `p15_autos_agregado_2 o más autos` +
    
    # p16_motos_agregado  (BASE: 2 o más motocicletas)
    `p16_motos_agregado_Sin motocicletas` + `p16_motos_agregado_1 motocicleta` +
    
    # p22  (BASE: Más de 20 km)
    `p22_Entre 16 y 20 km` + `p22_Entre 11 y 15 km` +
    `p22_Entre 6 y 10 km` + `p22_Menos de 5 km` +
    
    # p23_agregado  (BASE: Trabajo)
    `p23_agregado_Recreación, salud y actividades personales` +
    `p23_agregado_Compras y trámites` +
    `p23_agregado_Estudio` +
    `p23_agregado_Visitas sociales` +
    `p23_agregado_Cuidado y familia (escuela, niñas/os)` +
    `p23_agregado_Cuidado y familia (recreación, niñas/os)` +
    `p23_agregado_Cuidado y familia (salud, niñas/os)` +
    `p23_agregado_Cuidado y familia (persona enferma)` +
    `p23_agregado_Otro` +
    `p23_agregado_Cuidado y familia (persona con discapacidad)` +
    
    # p30_razon_no_uso_agregado  (BASE: Modo actual)
    `p30_razon_no_uso_agregado_Tiempo / disponibilidad` +
    `p30_razon_no_uso_agregado_Limitaciones económicas` +
    `p30_razon_no_uso_agregado_Inseguridad / acoso` +
    `p30_razon_no_uso_agregado_Otro motivo` +
    `p30_razon_no_uso_agregado_Condiciones físicas / salud` +
    `p30_razon_no_uso_agregado_Falta de infraestructura / distancia` +
    
    # p31_fuente_contaminacion_agregada  (BASE: Vehículos motorizados)
    `p31_fuente_contaminacion_agregada_Industria/Obras` +
    `p31_fuente_contaminacion_agregada_Productos químicos` +
    `p31_fuente_contaminacion_agregada_Otra fuente` +
    `p31_fuente_contaminacion_agregada_Vertederos (basureros) y rellenos sanitarios` +
    `p31_fuente_contaminacion_agregada_Quema de residuos` +
    
    # p38p38_i  (BASE: No) | p38p38_99 (BASE: No sabe)
    `p38p38_1_Si` + `p38p38_1_No sabe` +
    `p38p38_2_Si` + `p38p38_2_No sabe` +
    `p38p38_3_Si` + `p38p38_3_No sabe` +
    `p38p38_4_Si` + `p38p38_4_No sabe` +
    `p38p38_5_Si` + `p38p38_5_No sabe` +
    `p38p38_6_Si` + `p38p38_6_No sabe` +
    `p38p38_7_Si` + `p38p38_7_No sabe` +
    `p38p38_99_No` + `p38p38_99_Si` +
    
    # p19comuna  (BASE: Comuna 16 - Belén)
    `p19comuna_Comuna 13 - San Javier` +
    `p19comuna_Comuna 15 - Guayabal` +
    `p19comuna_Comuna 3 - Manrique` +
    `p19comuna_Comuna 6 - Doce de Octubre` +
    `p19comuna_Comuna 8 - Villa Hermosa` +
    `p19comuna_Comuna 4 - Aranjuez` +
    `p19comuna_Comuna 12 - La América` +
    `p19comuna_Comuna 1 - Popular` +
    `p19comuna_Comuna 5 - Castilla` +
    `p19comuna_Comuna 10 - La Candelaria` +
    `p19comuna_Comuna 2 - Santa Cruz` +
    `p19comuna_Comuna 7 - Robledo` +
    `p19comuna_Comuna 9 - Buenos Aires` +
    `p19comuna_Comuna 11 - Laureles Estadio` +
    `p19comuna_Comuna 14 - El Poblado`
  ,
  data = dataset,
  trace = FALSE
)

# --- SIN COMUNAS (quitas todas las dummies p19comuna_*) ---
modelo_multinomial_sin <- multinom(
  medio ~ 
    # ------------------ CONTINUAS ------------------
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
    p1edad +
    
    # ------------------ CATEGÓRICAS ------------------
    
    `edad_r2_18 - 34 años` + `edad_r2_55 - 80 años` +
    `p3_agregado_Población afrodescendiente` + `p3_agregado_Sin respuesta` + `p3_agregado_Pueblos indígenas` +
    `p5_agregado_Secundaria` + `p5_agregado_Primaria o menos` + `p5_agregado_Técnico / Tecnológico` +
    `p7_agregado_Trabajo doméstico no remunerado` + `p7_agregado_Desocupado o inactivo` + `p7_agregado_Estudiante` + `p7_agregado_Otro` +
    `p9_estrato3_Medio` + `p9_estrato3_Bajo` +
    `p12_dificultad_binaria_Con alguna dificultad` +
    `p40_Mujer` +
    `p13_Sí, auto` + `p13_No` + `p13_Sí, motocicleta` +
    `p14_Si, auto` + `p14_No` + `p14_Si, motocicleta` +
    `p15_autos_agregado_1 auto` + `p15_autos_agregado_2 o más autos` +
    `p16_motos_agregado_Sin motocicletas` + `p16_motos_agregado_1 motocicleta` +
    `p22_Entre 16 y 20 km` + `p22_Entre 11 y 15 km` + `p22_Entre 6 y 10 km` + `p22_Menos de 5 km` +
    `p23_agregado_Recreación, salud y actividades personales` + `p23_agregado_Compras y trámites` + `p23_agregado_Estudio` +
    `p23_agregado_Visitas sociales` + `p23_agregado_Cuidado y familia (escuela, niñas/os)` +
    `p23_agregado_Cuidado y familia (recreación, niñas/os)` + `p23_agregado_Cuidado y familia (salud, niñas/os)` +
    `p23_agregado_Cuidado y familia (persona enferma)` + `p23_agregado_Otro` +
    `p23_agregado_Cuidado y familia (persona con discapacidad)` +
    `p30_razon_no_uso_agregado_Tiempo / disponibilidad` + `p30_razon_no_uso_agregado_Limitaciones económicas` +
    `p30_razon_no_uso_agregado_Inseguridad / acoso` + `p30_razon_no_uso_agregado_Otro motivo` +
    `p30_razon_no_uso_agregado_Condiciones físicas / salud` + `p30_razon_no_uso_agregado_Falta de infraestructura / distancia` +
    `p31_fuente_contaminacion_agregada_Industria/Obras` + `p31_fuente_contaminacion_agregada_Productos químicos` +
    `p31_fuente_contaminacion_agregada_Otra fuente` +
    `p31_fuente_contaminacion_agregada_Vertederos (basureros) y rellenos sanitarios` +
    `p31_fuente_contaminacion_agregada_Quema de residuos` +
    `p38p38_1_Si` + `p38p38_1_No sabe` +
    `p38p38_2_Si` + `p38p38_2_No sabe` +
    `p38p38_3_Si` + `p38p38_3_No sabe` +
    `p38p38_4_Si` + `p38p38_4_No sabe` +
    `p38p38_5_Si` + `p38p38_5_No sabe` +
    `p38p38_6_Si` + `p38p38_6_No sabe` +
    `p38p38_7_Si` + `p38p38_7_No sabe` +
    `p38p38_99_No` + `p38p38_99_Si`
  ,
  data = dataset,
  trace = FALSE
)

# ===========================
# Guardar salidas en carpeta
# ===========================
out_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/271025_Results_Med/MNL"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ===========================
# a) Tablas con stargazer
# ===========================

# --- CON COMUNAS ---
stargazer(
  modelo_multinomial,
  type = "html",
  title = "Resultados del Modelo Logit Multinomial – Medellín (con comunas)",
  single.row = TRUE,
  na.replace = "",
  out = file.path(out_dir, "mnl_med_concomunas_stargazer.html")
)

stargazer(
  modelo_multinomial,
  type = "text",
  title = "Resultados del Modelo Logit Multinomial – Medellín (con comunas)",
  single.row = TRUE,
  na.replace = "",
  out = file.path(out_dir, "mnl_med_concomunas_stargazer.txt")
)

# --- SIN COMUNAS ---
stargazer(
  modelo_multinomial_sin,
  type = "html",
  title = "Resultados del Modelo Logit Multinomial – Medellín (sin comunas)",
  single.row = TRUE,
  na.replace = "",
  out = file.path(out_dir, "mnl_med_sincomunas_stargazer.html")
)

stargazer(
  modelo_multinomial_sin,
  type = "text",
  title = "Resultados del Modelo Logit Multinomial – Medellín (sin comunas)",
  single.row = TRUE,
  na.replace = "",
  out = file.path(out_dir, "mnl_med_sincomunas_stargazer.txt")
)

# ==========================================
# b) OR + IC95% + z + p a Excel (CON COMUNAS)
# ==========================================
sm_con  <- summary(modelo_multinomial)
betas_con <- sm_con$coefficients
ses_con   <- sm_con$standard.errors

coef_long_con <- betas_con %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "categoria") %>%
  tidyr::pivot_longer(-categoria, names_to = "termino", values_to = "estimate")

se_long_con <- ses_con %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "categoria") %>%
  tidyr::pivot_longer(-categoria, names_to = "termino", values_to = "std.error")

tab_or_con <- coef_long_con %>%
  dplyr::left_join(se_long_con, by = c("categoria","termino")) %>%
  dplyr::mutate(
    z       = estimate / std.error,
    p       = 2 * pnorm(abs(z), lower.tail = FALSE),
    OR      = exp(estimate),
    CI_low  = exp(estimate - 1.96 * std.error),
    CI_high = exp(estimate + 1.96 * std.error)
  ) %>%
  dplyr::select(categoria, termino, OR, CI_low, CI_high, z, p) %>%
  dplyr::arrange(categoria, termino)

tab_or_fmt_con <- tab_or_con %>%
  dplyr::mutate(
    OR     = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high= round(CI_high, 3),
    z      = round(z, 3),
    p      = round(p, 4)
  )

or_wide_con <- exp(coef(modelo_multinomial)) %>%
  as.data.frame() %>%
  round(3)

writexl::write_xlsx(
  list("OR_largo" = tab_or_fmt_con,
       "OR_matriz" = or_wide_con),
  path = file.path(out_dir, "mnl_med_concomunas_OR.xlsx")
)

# ============================================
# c) OR + IC95% + z + p a Excel (SIN COMUNAS)
# ============================================
sm_sin  <- summary(modelo_multinomial_sin)
betas_sin <- sm_sin$coefficients
ses_sin   <- sm_sin$standard.errors

coef_long_sin <- betas_sin %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "categoria") %>%
  tidyr::pivot_longer(-categoria, names_to = "termino", values_to = "estimate")

se_long_sin <- ses_sin %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "categoria") %>%
  tidyr::pivot_longer(-categoria, names_to = "termino", values_to = "std.error")

tab_or_sin <- coef_long_sin %>%
  dplyr::left_join(se_long_sin, by = c("categoria","termino")) %>%
  dplyr::mutate(
    z       = estimate / std.error,
    p       = 2 * pnorm(abs(z), lower.tail = FALSE),
    OR      = exp(estimate),
    CI_low  = exp(estimate - 1.96 * std.error),
    CI_high = exp(estimate + 1.96 * std.error)
  ) %>%
  dplyr::select(categoria, termino, OR, CI_low, CI_high, z, p) %>%
  dplyr::arrange(categoria, termino)

tab_or_fmt_sin <- tab_or_sin %>%
  dplyr::mutate(
    OR     = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high= round(CI_high, 3),
    z      = round(z, 3),
    p      = round(p, 4)
  )

or_wide_sin <- exp(coef(modelo_multinomial_sin)) %>%
  as.data.frame() %>%
  round(3)

writexl::write_xlsx(
  list("OR_largo" = tab_or_fmt_sin,
       "OR_matriz" = or_wide_sin),
  path = file.path(out_dir, "mnl_med_sincomunas_OR.xlsx")
)

cat("\n✅ Archivos guardados en:\n", normalizePath(out_dir), "\n",
    "- mnl_med_concomunas_stargazer.html\n",
    "- mnl_med_concomunas_stargazer.txt\n",
    "- mnl_med_concomunas_OR.xlsx\n",
    "- mnl_med_sincomunas_stargazer.html\n",
    "- mnl_med_sincomunas_stargazer.txt\n",
    "- mnl_med_sincomunas_OR.xlsx\n", sep = "")

