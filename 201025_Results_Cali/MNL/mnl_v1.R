# ============================
# Multinomial Logit (Cali)
# ============================

# ---- Paquetes ----
library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(nnet)
library(forcats)
library(broom)
library(modelsummary)   

# ---- Directorio de trabajo ----
setwd("C:/Users/danie/OneDrive/Escritorio/Natura/201025_Results_Cali/")

# ---- 1) Cargar base y seleccionar variables ----
path_input <- "output/input_famd_cali_29102025.xlsx"

df <- read_excel(path_input) %>% clean_names()

# Variables independientes 
vars_x <- c(
  "edad_r2","p5_agregado","p9_estrato3","p40",
  "p28_importancia_costo_compra","p28_importancia_costo_uso","p28_importancia_comodidad",
  "p28_importancia_tiempo","p28_importancia_emisiones","p28_importancia_siniestralidad",
  "p32_contaminacion_likert","p36_influencia_amigos","p37_influencia_familia","tiempo_total"
)

vars_keep  <- c("medio", vars_x)
vars_exist <- vars_keep[vars_keep %in% names(df)]
df <- df[, vars_exist, drop = FALSE]

# ---- 2) Tipos: categóricas y numéricas ----
cat_vars <- intersect(c("edad_r2","p5_agregado","p9_estrato3","p40"), names(df))
num_vars <- setdiff(intersect(vars_x, names(df)), cat_vars)

df <- df %>%
  mutate(
    across(all_of(cat_vars), ~ droplevels(as.factor(.))),
    across(all_of(num_vars), ~ suppressWarnings(as.numeric(.))),
    medio = droplevels(as.factor(medio))
  )

# ---- 3) Modelo multinomial (todas las categorías de 'medio') ----
x_in_formula <- paste(intersect(vars_x, names(df)), collapse = " + ")
f_full <- as.formula(paste0("medio ~ ", x_in_formula))

m_full <- nnet::multinom(f_full, data = df, Hess = TRUE, trace = FALSE, maxit = 1000)

# ---- 4) Exportar a HTML con modelsummary (OR + IC95%) ----
dir.create("MNL", showWarnings = FALSE, recursive = TRUE)
html_out <- "MNL/modelo_multinomial_cali.html"

modelsummary(
  list("MNL (Cali)" = m_full),
  output       = html_out,
  exponentiate = TRUE,
  conf_level   = 0.95,
  estimate     = "{estimate} [{conf.low}, {conf.high}]",
  statistic    = "p = {p.value}",
  gof_omit     = "AIC|BIC|Log.Lik|Adj|Pseudo|RMSE|IC|R2",
  shape        = term ~ y.level   # columnas por cada categoría 
)

message("✅ HTML guardado en: ", normalizePath(html_out))
