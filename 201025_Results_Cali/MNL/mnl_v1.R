# ============================
# Multinomial Logit (Cali) 
# ============================

#############
# Librerias #
#############
library(dplyr)
library(nnet)
library(readxl)
library(tidyr)
library(stargazer)
library(writexl)
library(stringr)

############
# Base de  #
#   Datos  #
############
dataset <- read_excel("C:/Users/danie/OneDrive/Escritorio/Natura/201025_Results_Cali/output/input_famd_cali_29102025.xlsx")

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
  "edad_r2","pais","p3_agregado","p5_agregado","p7_agregado","p8_agregado",
  "p9_estrato3","p40","p13","p14","p15_autos_agregado","p15_1_autos_propios_agregado",
  "p16_motos_agregado","p16_1_motos_propias_agregado","p17_modo_agregado",
  "cilindraje_auto_agregado","cilindraje_moto_agregado","modelo_vehiculo_agregado",
  "p19comuna","p22","p23_agregado","p26_agregado","p29_modo_ideal_agregado",
  "p30_razon_no_uso_agregado","p31_fuente_contaminacion_agregada",
  "p33_modo_contaminante_agregado","p35_razon_agregada",
  "p38p38_1","p38p38_2","p38p38_3","p38p38_4","p38p38_5","p38p38_6","p38p38_7","p38p38_99")

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
    
    # ------------------ CATEGÓRICAS (una base omitida por bloque) ------------------
  
  # edad_r2  (BASE: 35 - 54 años)
  `edad_r2_55 - 80 años` + `edad_r2_18 - 34 años` +
    
    # pais  (BASE: Colombia)
    `pais_Venezuela` +
    
    # p3_agregado  (BASE: Ninguna)
    `p3_agregado_Población afrodescendiente` +
    `p3_agregado_Pueblos indígenas` +
    `p3_agregado_Sin respuesta` +
    
    # p5_agregado  (BASE: Superior)
    `p5_agregado_Primaria o menos` +
    `p5_agregado_Técnico / Tecnológico` +
    `p5_agregado_Secundaria` +
    `p5_agregado_Sin respuesta` +
    
    # p7_agregado  (BASE: Ocupado/a)
    `p7_agregado_Desocupado o inactivo` +
    `p7_agregado_Trabajo doméstico no remunerado` +
    `p7_agregado_Estudiante` +
    `p7_agregado_Otro` +
    
    # p8_agregado  (BASE: Vive con pareja (con o sin hijos/as))
    `p8_agregado_Vive solo/a` +
    `p8_agregado_Vive con hijos/as (sin pareja)` +
    `p8_agregado_Vive con familiares (otros)` +
    `p8_agregado_Vive con no familiares` +
    
    # p9_estrato3  (BASE: Alto)
    `p9_estrato3_Bajo` + `p9_estrato3_Medio` +
    
    # p40  (BASE: Hombre)
    `p40_Mujer` +
    
    # p13  (BASE: Sí, auto y motocicleta)
    `p13_No` + `p13_Sí, motocicleta` + `p13_Sí, auto` +
    
    # p14  (BASE: Si de auto y motocicleta)
    `p14_No` + `p14_Si, motocicleta` + `p14_Si, auto` +
    
    # p15_autos_agregado  (BASE: Sin autos)
    `p15_autos_agregado_1 auto` +
    `p15_autos_agregado_2 o más autos` +
    
    # p15_1_autos_propios_agregado  (BASE: Sin autos propios)
    `p15_1_autos_propios_agregado_1 auto propio` +
    `p15_1_autos_propios_agregado_2 o más autos propios` +
    
    # p16_motos_agregado  (BASE: 2 o más motocicletas)
    `p16_motos_agregado_Sin motocicletas` +
    `p16_motos_agregado_1 motocicleta` +
    
    # p16_1_motos_propias_agregado  (BASE: 1 motocicleta propia)
    `p16_1_motos_propias_agregado_Sin motocicletas propias` +
    `p16_1_motos_propias_agregado_2 o más motocicletas propias` +
    
    # cilindraje_auto_agregado  (BASE: Eléctrico / No aplica)
    `cilindraje_auto_agregado_1000 - 1499 cc` +
    `cilindraje_auto_agregado_Menos de 1000 cc` +
    `cilindraje_auto_agregado_2000 - 2499 cc` +
    `cilindraje_auto_agregado_1500 - 1999 cc` +
    `cilindraje_auto_agregado_No sabe / No responde` +
    `cilindraje_auto_agregado_2500 - 2999 cc` +
    
    # cilindraje_moto_agregado  (BASE: 125 cc)
    `cilindraje_moto_agregado_Eléctrico / No aplica` +
    `cilindraje_moto_agregado_No sabe / No responde` +
    `cilindraje_moto_agregado_Menos de 125 cc` +
    `cilindraje_moto_agregado_150 cc` +
    `cilindraje_moto_agregado_150 - 250 cc` +
    `cilindraje_moto_agregado_Más de 250 cc` +
    `cilindraje_moto_agregado_Eléctrica / No aplica` +
    
    # modelo_vehiculo_agregado  (BASE: 2016 - 2020)
    `modelo_vehiculo_agregado_No aplica` +
    `modelo_vehiculo_agregado_2011 - 2015` +
    `modelo_vehiculo_agregado_2021 o más reciente` +
    `modelo_vehiculo_agregado_Sin información` +
    `modelo_vehiculo_agregado_Anterior a 2005` +
    `modelo_vehiculo_agregado_2005 - 2010` +
    
    # p19comuna  (BASE: Comuna 16)
    `p19comuna_Comuna 14` + `p19comuna_Comuna 21` + `p19comuna_Comuna 20` +
    `p19comuna_Comuna 18` + `p19comuna_Comuna 6`  + `p19comuna_Comuna 5`  +
    `p19comuna_Comuna 8`  + `p19comuna_Comuna 10` + `p19comuna_Comuna 3`  +
    `p19comuna_Comuna 4`  + `p19comuna_Comuna 12` + `p19comuna_Comuna 13` +
    `p19comuna_Comuna 11` + `p19comuna_Comuna 15` + `p19comuna_Comuna 7`  +
    `p19comuna_Comuna 9`  + `p19comuna_Comuna 1`  + `p19comuna_Comuna 17` +
    `p19comuna_Comuna 2`  + `p19comuna_Comuna 22` + `p19comuna_Comuna 19` +
    
    # p22  (BASE: Más de 12 km)
    `p22_Entre 4 y 7 km` + `p22_Entre 8 y 12 km` +
    `p22_Entre 1 y 3 km` + `p22_Menos de 1 km` +
    
    # p23_agregado  (BASE: Trabajo)
    `p23_agregado_Recreación, salud y actividades personales` +
    `p23_agregado_Compras y trámites` +
    `p23_agregado_Estudio` +
    `p23_agregado_Cuidado y familia (centro educativo, niños/as o jóvenes)` +
    `p23_agregado_Cuidado y familia (otro lugar, niños/as o jóvenes)` +
    `p23_agregado_Visitas sociales` +
    `p23_agregado_Otro` +
    
    # p26_agregado  (BASE: Incomodidad / clima)
    `p26_agregado_Riesgo de accidente` +
    `p26_agregado_Sin respuesta` +
    `p26_agregado_Costo económico` +
    `p26_agregado_Tiempo de viaje / espera` +
    `p26_agregado_Nada le disgusta` +
    `p26_agregado_Impacto ambiental` +
    `p26_agregado_Inseguridad personal` +
    `p26_agregado_Otro motivo` +
    `p26_agregado_Falta de autonomía / control` +
    
    # p29_modo_ideal_agregado  (BASE: Motocicleta)
    `p29_modo_ideal_agregado_Transporte público` +
    `p29_modo_ideal_agregado_Otro` +
    `p29_modo_ideal_agregado_Automóvil` +
    `p29_modo_ideal_agregado_Taxi` +
    `p29_modo_ideal_agregado_Bicicleta` +
    `p29_modo_ideal_agregado_Caminar` +
    
    # p30_razon_no_uso_agregado  (BASE: Modo actual)
    `p30_razon_no_uso_agregado_Condiciones físicas / salud` +
    `p30_razon_no_uso_agregado_Limitaciones económicas` +
    `p30_razon_no_uso_agregado_Otro motivo` +
    `p30_razon_no_uso_agregado_Falta de infraestructura / distancia` +
    `p30_razon_no_uso_agregado_Inseguridad / acoso` +
    `p30_razon_no_uso_agregado_Tiempo / disponibilidad` +
    
    # p31_fuente_contaminacion_agregada  (BASE: Vehículos motorizados)
    `p31_fuente_contaminacion_agregada_Productos químicos` +
    `p31_fuente_contaminacion_agregada_Quema de residuos` +
    `p31_fuente_contaminacion_agregada_Industria/Obras` +
    `p31_fuente_contaminacion_agregada_Vertederos (basureros) y rellenos sanitarios` +
    `p31_fuente_contaminacion_agregada_Otra fuente` +
    
    # p33_modo_contaminante_agregado  (BASE: Motocicleta)
    `p33_modo_contaminante_agregado_Camión` +
    `p33_modo_contaminante_agregado_Transporte público` +
    `p33_modo_contaminante_agregado_Automóvil` +
    `p33_modo_contaminante_agregado_Otro modo` +
    
    # p35_razon_agregada  (BASE: Falta de infraestructura)
    `p35_razon_agregada_Costos altos` +
    `p35_razon_agregada_Inseguridad / violencia` +
    `p35_razon_agregada_Falta de información` +
    `p35_razon_agregada_Condiciones climáticas` +
    `p35_razon_agregada_Otro motivo` +
    
    # p38p38_1  (BASE: No)
    `p38p38_1_Si` + `p38p38_1_No sabe` +
    
    # p38p38_2  (BASE: No)
    `p38p38_2_Si` + `p38p38_2_No sabe` +
    
    # p38p38_3  (BASE: No)
    `p38p38_3_Si` + `p38p38_3_No sabe` +
    
    # p38p38_4  (BASE: No)
    `p38p38_4_Si` + `p38p38_4_No sabe` +
    
    # p38p38_5  (BASE: No)
    `p38p38_5_Si` + `p38p38_5_No sabe` +
    
    # p38p38_6  (BASE: No)
    `p38p38_6_Si` + `p38p38_6_No sabe` +
    
    # p38p38_7  (BASE: No)
    `p38p38_7_Si` + `p38p38_7_No sabe` +
    
    # p38p38_99  (BASE: No sabe)
    `p38p38_99_No` + `p38p38_99_Si`
  ,
  data  = dataset,
  trace = FALSE
)


# ===========================
# Guardar salidas en carpeta
# ===========================
out_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/201025_Results_Cali/MNL"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# a) Tabla del modelo con stargazer (HTML y TXT)
coef_names <- colnames(coef(modelo_multinomial))
stargazer(
  modelo_multinomial,
  type = "html",
  title = "Resultados del Modelo Logit Multinomial – Cali",
  single.row = TRUE,
  na.replace = "",                  
  out = file.path(out_dir, "mnl_cali_stargazer.html")
)

stargazer(
  modelo_multinomial,
  type = "text",
  title = "Resultados del Modelo Logit Multinomial – Cali",
  single.row = TRUE,
  na.replace = "",
  out = file.path(out_dir, "mnl_cali_stargazer.txt")
)

# b) OR + IC95% + z + p a Excel
sm  <- summary(modelo_multinomial)
betas <- sm$coefficients
ses   <- sm$standard.errors

coef_long <- betas %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "categoria") %>%
  tidyr::pivot_longer(-categoria, names_to = "termino", values_to = "estimate")

se_long <- ses %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "categoria") %>%
  tidyr::pivot_longer(-categoria, names_to = "termino", values_to = "std.error")

tab_or <- coef_long %>%
  left_join(se_long, by = c("categoria","termino")) %>%
  mutate(
    z       = estimate / std.error,
    p       = 2 * pnorm(abs(z), lower.tail = FALSE),
    OR      = exp(estimate),
    CI_low  = exp(estimate - 1.96 * std.error),
    CI_high = exp(estimate + 1.96 * std.error)
  ) %>%
  dplyr::select(categoria, termino, OR, CI_low, CI_high, z, p) %>%
  arrange(categoria, termino)

# Redondeo
tab_or_fmt <- tab_or %>%
  mutate(
    OR     = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high= round(CI_high, 3),
    z      = round(z, 3),
    p      = round(p, 4)
  )

# Matriz de OR por categoría
or_wide <- exp(coef(modelo_multinomial)) %>%
  as.data.frame() %>%
  round(3)

# Guardar a Excel (dos hojas)
writexl::write_xlsx(
  list("OR_largo" = tab_or_fmt,
       "OR_matriz" = or_wide),
  path = file.path(out_dir, "mnl_cali_OR.xlsx")
)

cat("\n✅ Archivos guardados en:\n", normalizePath(out_dir), "\n",
    "- mnl_cali_stargazer.html\n",
    "- mnl_cali_stargazer.txt\n",
    "- mnl_cali_OR.xlsx\n", sep = "")

