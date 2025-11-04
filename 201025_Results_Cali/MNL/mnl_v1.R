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

# --- Recodificación / dummies ---
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

# p22_Más de 12 km
# Hombre
# edad_r2_35 - 54 años
#`p5_agregado_Superior`
#`p9_estrato3_Alto`
#`p7_agregado_Ocupado/a`

modelo_multinomial <- multinom(
  medio ~ p28_importancia_costo_compra + 
    p28_importancia_costo_uso + p28_importancia_comodidad + 
    p28_importancia_tiempo + p28_importancia_emisiones +
    p28_importancia_siniestralidad + 
    p32_contaminacion_likert + p36_influencia_amigos +
    p37_influencia_familia + tiempo_total +
    `p22_Menos de 1 km`  + 
    `p22_Entre 8 y 12 km` + `p22_Entre 4 y 7 km` + 
    `p22_Entre 1 y 3 km` + `p7_agregado_Trabajo doméstico no remunerado` +
    `p7_agregado_Otro` + 
    `p7_agregado_Estudiante` + `p7_agregado_Desocupado o inactivo` + 
    `p40_Mujer` + `edad_r2_18 - 34 años` + 
    `edad_r2_55 - 80 años` + 
    `p5_agregado_Primaria o menos` + `p5_agregado_Secundaria` + 
    `p5_agregado_Sin respuesta` + 
    `p5_agregado_Técnico / Tecnológico` +
    `p9_estrato3_Bajo` + p9_estrato3_Medio,
  data = dataset
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

