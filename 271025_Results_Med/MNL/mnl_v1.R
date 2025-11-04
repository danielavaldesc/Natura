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

# --- Recodificación / dummies (tu flujo) ---
dataset <- subset(dataset, !(p40 %in% c("Otro", "Prefiere no responder", "Otras identidades de género")))
dataset <- columna_dummy(dataset, "edad_r2")
dataset <- columna_dummy(dataset, "p5_agregado")
dataset <- columna_dummy(dataset, "p9_estrato3")
dataset <- columna_dummy(dataset, "p40")
dataset <- columna_dummy(dataset, "p7_agregado")
dataset <- columna_dummy(dataset, "p22")
if ("id" %in% names(dataset)) dataset <- subset(dataset, select = -id)

# --- Modelo ---
dataset$medio <- relevel(factor(dataset$medio), ref = "Moto privada")

# `p22_Más de 20 km`
# `p7_agregado_Ocupado/a`
# `edad_r2_35 - 54 años`
# p40_Hombre
# `p9_estrato3_Alto`
# `p5_agregado_Superior`

modelo_multinomial <- multinom(
  medio ~ p28_importancia_costo_compra + 
    p28_importancia_costo_uso + p28_importancia_comodidad + 
    p28_importancia_tiempo + p28_importancia_emisiones +
    p28_importancia_siniestralidad + 
    p32_contaminacion_likert + p36_influencia_amigos +
    p37_influencia_familia + tiempo_total +
    `p22_Menos de 5 km` + 
    `p22_Entre 6 y 10 km` + `p22_Entre 11 y 15 km` + 
    `p22_Entre 16 y 20 km` + `p7_agregado_Trabajo doméstico no remunerado` +
    `p7_agregado_Otro` + 
    `p7_agregado_Estudiante` + `p7_agregado_Desocupado o inactivo` + 
    `p40_Mujer` + `edad_r2_18 - 34 años` + 
     `edad_r2_55 - 80 años` + 
    `p5_agregado_Primaria o menos` + `p5_agregado_Secundaria` + 
     `p5_agregado_Técnico / Tecnológico` + 
     `p9_estrato3_Bajo` + p9_estrato3_Medio,
  data = dataset)

# ===========================
# Guardar salidas en carpeta
# ===========================
out_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/271025_Results_Med/MNL"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# a) Tabla del modelo con stargazer (HTML y TXT)
coef_names <- colnames(coef(modelo_multinomial))
stargazer(
  modelo_multinomial,
  type = "html",
  title = "Resultados del Modelo Logit Multinomial – Medellín",
  single.row = TRUE,
  na.replace = "",                  # evita el error de 'valor ausente'
  out = file.path(out_dir, "mnl_med_stargazer.html")
)

stargazer(
  modelo_multinomial,
  type = "text",
  title = "Resultados del Modelo Logit Multinomial – Medellín",
  single.row = TRUE,
  na.replace = "",
  out = file.path(out_dir, "mnl_med_stargazer.txt")
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
  path = file.path(out_dir, "mnl_med_OR.xlsx")
)

cat("\n✅ Archivos guardados en:\n", normalizePath(out_dir), "\n",
    "- mnl_med_stargazer.html\n",
    "- mnl_med_stargazer.txt\n",
    "- mnl_med_OR.xlsx\n", sep = "")

