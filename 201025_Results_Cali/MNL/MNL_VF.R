##############################################
## MNL Cali – Variante 2 (Taxi/Informal)   ##
##############################################

# --------- Librerías ----------
library(dplyr)
library(readxl)
library(tidyr)
library(tibble)
library(forcats)
library(nnet)
library(stargazer)
library(writexl)
library(stringr)

# --------- Datos ----------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\")
input <- read_excel("201025_Results_Cali\\output\\input_famd_cali_29102025.xlsx")

# --------- Continuas ----------
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
  "tiempo_total"
)

# --------- Ensamble base categórica ----------
categoricas <- setdiff(names(input), c(continuas, "id", "medio"))
df.cat <- input %>% select(any_of(categoricas)) %>% mutate(across(everything(), as.factor))
df.mnl <- cbind(df.cat, input[c("id","medio",continuas)])

# --------- Unificación propósito (p23_agr5) ----------
df.mnl <- df.mnl %>%
  mutate(
    p23_agr5 = fct_collapse(
      p23_agregado,
      "Trabajo"          = c("Trabajo"),
      "Compras/Trámites" = c("Compras y trámites","Compras y tr\u00e1mites"),
      "Tiempo personal"  = c("Recreación, salud y actividades personales",
                             "Recreaci\u00f3n, salud y actividades personales",
                             "Visitas sociales"),
      "Estudio"          = "Estudio",
      "Cuidado"          = c(
        "Cuidado y familia (centro educativo, niños/as o jóvenes)",
        "Cuidado y familia (otro lugar, niños/as o jóvenes)"
      ),
      "Otros"            = "Otro"
    ) %>% fct_drop()
  ) %>%
  filter(p23_agr5 != "Otros") %>%
  mutate(p23_agr5 = as.factor(as.character(p23_agr5)))

# --------- Filtros básicos y género ----------
df.mnl <- df.mnl %>%
  filter(p5_agregado  != "Sin respuesta",
         p40 %in% c("Hombre","Mujer")) %>%
  mutate(p40 = as.factor(as.character(p40)))

# --------- Filtro situación laboral base ----------
df.mnl <- df.mnl %>%
  filter(p7_agregado  != "Otro") %>%
  mutate(p7_agregado = as.factor(as.character(p7_agregado)))

# --------- Étnico binario ----------
df.mnl <- df.mnl %>%
  mutate(
    aut_rec_etnico = dplyr::case_when(
      p3_agregado %in% c("Población afrodescendiente","Pueblos indígenas") ~ "Si",
      TRUE ~ "No"
    ),
    aut_rec_etnico = factor(aut_rec_etnico, levels = c("No","Si"))
  )

# --------- Educación 3 cat ----------
df.mnl <- df.mnl %>%
  mutate(
    educ_3cat = dplyr::case_when(
      p5_agregado == "Primaria o menos" ~ "Primaria o menos",
      p5_agregado == "Secundaria" ~ "Secundaria",
      p5_agregado %in% c("Superior","Técnico / Tecnológico") ~ "Terciaria",
      TRUE ~ NA_character_
    ),
    educ_3cat = factor(educ_3cat, levels = c("Primaria o menos","Secundaria","Terciaria"))
  )

# --------- Situación laboral (incluye Estudiante en Desocupado/Inactivo) ----------
df.mnl <- df.mnl %>%
  mutate(
    sitlab = dplyr::case_when(
      p7_agregado == "Ocupado/a" ~ "Asalariado o independiente",
      p7_agregado == "Trabajo doméstico no remunerado" ~ "Trabajo doméstico no remunerado",
      p7_agregado %in% c("Desocupado o inactivo","Estudiante") ~ "Desocupado o inactivo",
      TRUE ~ NA_character_
    ),
    sitlab = factor(sitlab,
                    levels = c("Asalariado o independiente","Trabajo doméstico no remunerado","Desocupado o inactivo"))
  )

# --------- Tenencia autos / motos ----------
df.mnl <- df.mnl %>%
  mutate(
    ten_autos = dplyr::case_when(
      p15_autos_agregado == "Sin autos" ~ "Sin autos",
      p15_autos_agregado %in% c("1 auto","2 o más autos") ~ "1 auto o más",
      TRUE ~ NA_character_
    ),
    ten_autos = factor(ten_autos, levels = c("Sin autos","1 auto o más")),
    ten_motos = dplyr::case_when(
      p16_motos_agregado == "Sin motocicletas" ~ "Sin motocicletas",
      p16_motos_agregado %in% c("1 motocicleta","2 o más motocicletas") ~ "1 motocicleta o más",
      TRUE ~ NA_character_
    ),
    ten_motos = factor(ten_motos, levels = c("Sin motocicletas","1 motocicleta o más"))
  )

# --------- Distancia 3 grupos ----------
df.mnl <- df.mnl %>%
  mutate(
    dist_recod = dplyr::case_when(
      p22 %in% c("Menos de 1 km","Entre 1 y 3 km") ~ "Menos de 3 km",
      p22 %in% c("Entre 4 y 7 km","Entre 8 y 12 km") ~ "Entre 4 y 12 km",
      p22 %in% c("Más de 12 km") ~ "Más de 12 km",
      TRUE ~ NA_character_
    ),
    dist_recod = factor(dist_recod, levels = c("Menos de 3 km","Entre 4 y 12 km","Más de 12 km"))
  )

# --------- p13 y p14: binarios independientes (auto/moto) ----------
df.mnl <- df.mnl %>%
  mutate(
    p13_auto = ifelse(p13 %in% c("Sí, auto","Sí, auto y motocicleta"), 1L, 0L),
    p13_moto = ifelse(p13 %in% c("Sí, motocicleta","Sí, auto y motocicleta"), 1L, 0L),
    p14_auto = ifelse(p14 %in% c("Sí, auto","Sí, auto y motocicleta","Sí de auto y motocicleta"), 1L, 0L),
    p14_moto = ifelse(p14 %in% c("Sí, motocicleta","Sí, auto y motocicleta","Sí de auto y motocicleta"), 1L, 0L),
    p13_auto = factor(p13_auto, levels = c(0,1)),
    p13_moto = factor(p13_moto, levels = c(0,1)),
    p14_auto = factor(p14_auto, levels = c(0,1)),
    p14_moto = factor(p14_moto, levels = c(0,1))
  )

# "Taxi / Plataforma" + "Transporte informal" -> "Taxi/Informal"
df.mnl <- df.mnl %>%
  mutate(
    medio2 = forcats::fct_collapse(
      medio,
      "Taxi/Informal" = c("Taxi / Plataforma","Transporte informal")
    ),
    medio2 = relevel(factor(medio2), ref = "Moto privada")
  )

# --------- Fórmulas (base y con comunas) ----------
form_base <- as.formula(
  medio2 ~ 
    p24 + p28_importancia_costo_compra + p28_importancia_costo_uso + 
    p28_importancia_comodidad + p28_importancia_tiempo + p28_importancia_riesgo_robo +
    p28_importancia_riesgo_acoso + p28_importancia_discriminacion +
    p28_importancia_emisiones + p28_importancia_siniestralidad +
    tiempo_total +
    edad_r2 + aut_rec_etnico + educ_3cat + sitlab + p40 + p38p38_dummy +
    p13_auto + p13_moto
)

form_con <- update(form_base, . ~ . + p19comuna)

# --------- Estimaciones ----------
mnl_form_con <- multinom(form_con, data = df.mnl, trace = FALSE)
mnl_form_sin <- multinom(form_base, data = df.mnl, trace = FALSE)

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

or_con <- or_table(mnl_form_con)
or_sin <- or_table(mnl_form_sin)

# --------- Exportar ----------
out_dir <- "201025_Results_Cali\\MNL"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

stargazer(mnl_form_con, type = "html",
          title = "MNL – CON comunas (Cali) [Taxi/Informal colapsado]",
          single.row = TRUE, na.replace = "",
          out = file.path(out_dir, "mnl_form_CONcomunas_Cali_TaxiInformal.html"))

stargazer(mnl_form_sin, type = "html",
          title = "MNL – SIN comunas (Cali) [Taxi/Informal colapsado]",
          single.row = TRUE, na.replace = "",
          out = file.path(out_dir, "mnl_form_SINcomunas_Cali_TaxiInformal.html"))

writexl::write_xlsx(
  list(
    "CON_OR_largo"   = or_con$long,
    "CON_OR_matriz"  = or_con$matrix,
    "SIN_OR_largo"   = or_sin$long,
    "SIN_OR_matriz"  = or_sin$matrix
  ),
  path = file.path(out_dir, "mnl_form_OR_CONySIN_Cali_TaxiInformal.xlsx")
)


# ===================
# Variantes 
# ===================

# V1  = base   + p23_agr5
form_v1 <- update(form_base, . ~ . + p23_agr5)
# NO - CAT DESBALANCEADAS AÚN CON RECOD

# V2  = base   + ten_autos + ten_motos
form_v2 <- update(form_base, . ~ . + ten_autos + ten_motos)
# NO

# V3  = base   + dist_recod
form_v3 <- update(form_base, . ~ . + dist_recod)
# SÍ

# V4  = base   + p13_auto + p13_moto
form_v4 <- update(form_base, . ~ . + p13_auto + p13_moto)
# PUEDE

# V5  = base   + p14_auto + p14_moto
form_v5 <- update(form_base, . ~ . + p14_auto + p14_moto)
# NO

# V6  = base   + p13_* + p14_* (ambos)
form_v6 <- update(form_base, . ~ . + p13_auto + p13_moto + p14_auto + p14_moto)
# NO



# Stargazer
library(stargazer)
tmp <- tempfile(fileext = ".html")

stargazer(form_v6,
          type = "html",
          title = "Modelo multinomial",
          out = tmp)
rstudioapi::viewer(tmp)

#####################################
## 5. Validación de OR
#####################################
exp(coef(form_base))

