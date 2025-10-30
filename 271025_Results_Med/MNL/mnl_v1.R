# ================================
  # Multinomial Logit (Medellín)
# ================================

# ---- Paquetes ----
library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(nnet)
library(modelsummary)
library(forcats)

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\")

# ---- 1) Cargar base y seleccionar variables ----
path_input <- "output\\input_famd_med_29102025.xlsx"  

df <- readxl::read_excel(path_input) %>% janitor::clean_names()

# Variables independientes 
vars_x <- c(
  "edad_r2","p5_agregado","p9_estrato3","p40",
  "p28_importancia_costo_compra","p28_importancia_costo_uso","p28_importancia_comodidad",
  "p28_importancia_tiempo","p28_importancia_emisiones","p28_importancia_siniestralidad",
  "p32_contaminacion_likert","p36_influencia_amigos","p37_influencia_familia","tiempo_total"
)

vars_keep <- c("medio", vars_x)
vars_exist <- vars_keep[vars_keep %in% names(df)]
df <- df[, vars_exist, drop = FALSE]

# Tipos: categóricas y numéricas
cat_vars <- c("edad_r2","p5_agregado","p9_estrato3","p40")
num_vars <- setdiff(vars_x, cat_vars)

df <- df %>%
  mutate(
    across(all_of(intersect(cat_vars, names(.))), ~forcats::fct_explicit_na(as.factor(.), na_level = NA)),
    across(all_of(intersect(num_vars, names(.))), ~suppressWarnings(as.numeric(.))),
    medio = as.factor(medio)
  )

# ---- 2) Modelo 1: todas las categorías de "medio" ----
x_in_formula <- paste(intersect(vars_x, names(df)), collapse = " + ")
f_full <- as.formula(paste0("medio ~ ", x_in_formula))

set.seed(123)
m_full <- nnet::multinom(f_full, data = df, Hess = TRUE, trace = FALSE)

# ---- 3) Modelo 2: 4 medios  ----
# "Auto privado", "Modo activo", "Moto privada",
# "Taxi / Plataforma", "Transporte informal", "Transporte público"

df2 <- df %>%
  mutate(
    medio_4 = dplyr::case_when(
      medio == "Transporte público"   ~ "Transporte formal",
      medio == "Transporte informal"  ~ "Transporte informal",
      medio == "Auto privado"         ~ "Auto privado",
      medio == "Moto privada"         ~ "Moto privada",
      medio %in% c("Taxi / Plataforma","Modo activo") ~ NA_character_,
      TRUE ~ NA_character_
    ),
    medio_4 = factor(medio_4,
                     levels = c("Transporte formal","Transporte informal","Auto privado","Moto privada"))
  ) %>%
  filter(!is.na(medio_4))

f_4 <- as.formula(paste0("medio_4 ~ ", x_in_formula))

set.seed(123)
m_4 <- nnet::multinom(f_4, data = df2, Hess = TRUE, trace = FALSE)

# ---- 4) Exportar resultados a HTML ----
html_out <- "MNL\\modelos_multinomiales_med.html"
modelsummary::modelsummary(
  list(
    "Multinomial – todas las categorías" = m_full,
    "Multinomial – 4 categorías (formal / informal / auto / moto)" = m_4
  ),
  output = html_out,
  gof_map = c("n","aic","bic","logLik")
)

message("HTML guardado en: ", normalizePath(html_out))
